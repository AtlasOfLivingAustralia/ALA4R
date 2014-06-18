##--------------------------------------------------------------------------
## This code provides the examples demonstrated in:
##
## "Desktop analysis with Atlas data: ALA4R"
## Ben Raymond, Jeremy VanderWal, Lee Belbin
##
## Presented at the Atlas of Living Australia Science Symposium, 11-12 June 2014

##--------------------------------------------------------------------------
## initial setup

library(ALA4R)
library(plyr)
##ala_config(cache_directory="/your/cache/directory") ## optionally uncomment this line to keep a persistent cache of data. Once you have downloaded a given data set, it will be saved here and will not need to be re-downloaded next time
##ala_config(verbose=TRUE) ## optionally uncomment this line if you want to see additional detail on each operation


##--------------------------------------------------------------------------
## Name searching and taxonomic trees
library(ape)
library(phytools)

sx=search_fulltext("penguins")
(sx$data[,c("name","rank","score","commonName")])

tx=taxinfo_download("family:SPHENISCIDAE",fields=c("guid","genus","nameComplete","rank")) ## download data
tx=tx[tx$rank %in% c("species","subspecies"),] ## restrict to species and subspecies
temp=colwise(factor, c("genus","scientificName"))(tx) ## as.phylo requires the taxonomic columns to be factors
ax=as.phylo(~genus/scientificName,data=temp) ## create phylo object of Scientific.Name nested within Genus
tr=plotTree(ax,type="fan",fsize=0.7) ## plot it

## plot tree with images
library(jpeg)
s=lapply(tx$guid,function(z){ species_info(guid=z) }) ## extract species profile for each guid
## now extract the first image (of type jpg) from each of those species profiles, and store that image file in our data cache
imfiles=sapply(s,function(z){ ifelse(any(grepl("(\\.jpg)",z$images$smallImageUrl,ignore.case=TRUE)),ALA4R:::cached_get(z$images$smallImageUrl[grepl("(\\.jpg)",z$images$smallImageUrl,ignore.case=TRUE)][1],type="binary_filename"),"") })
tr=plotTree(ax,type="fan",ftype="off") ## plot tree without labels
## add each image
for (k in which(nchar(imfiles)>0)) {
    tryCatch({ im=readJPEG(imfiles[k]); rasterImage(im,tr$xx[k]-1/10,tr$yy[k]-1/10,tr$xx[k]+1/10,tr$yy[k]+1/10) },error=function(e){invisible(1)})
}


##--------------------------------------------------------------------------
## Area report: what listed species exist in a given area?

library(maptools)
## download an example shapefile of South Australian conservation reserve boundaries: see http://data.sa.gov.au/dataset/conservation-reserve-boundaries
## we use the ALA4R's caching mechanism here, but you could equally download this file directly
shape_filename=ALA4R:::cached_get("http://www.naturemaps.sa.gov.au/files/CONSERVATION_Npwsa_Reserves_shp.zip",type="binary_filename")
## unzip this file
unzip(shape_filename,exdir=ala_config()$cache_directory)
shape=readShapePoly(file.path(ala_config()$cache_directory,"CONSERVATION_NpwsaReserves.shp"))
shape=shape[shape$RESNAME=="Morialta",] ## extract just the Morialta Conservation Park polygon
if (FALSE) {
    ## we can create a WKT string using the rgeos library
    library(rgeos)
    wkt=writeWKT(shape)
    ## unfortunately, in this instance this gives a WKT string that is too long and won't be accepted by the ALA web service
} else {
    ## alternatively, we can construct the WKT string directly, which gives us a little more control over it
    lonlat=shape@polygons[[1]]@Polygons[[1]]@coords ## extract the polygon coordinates
    ## extract the convex hull of the polygon to reduce the length of the WKT string
    temp=chull(lonlat)
    lonlat=lonlat[c(temp,temp[1]),] 
    ## create WKT string
    wkt=paste("POLYGON((",paste(apply(lonlat,1,function(z)paste(z,collapse=" ")),collapse=","),"))",sep="")
}
x=specieslist(wkt=wkt,fq="state_conservation:*")
arrange(x,count)


##--------------------------------------------------------------------------
## Quality assertions

x=occurrences(taxon="Amblyornis newtonianus", download_reason_id=10)
summary(x)
##occurrences_plot(x,qa="fatal") ## uncomment to run: this will create an "Rplots.pdf" file in your working directory

## or as leaflet plot
library(leafletR)
x$data=x$data[!is.na(x$data$Longitude...processed) & !is.na(x$data$Latitude...processed),] ## drop any records with missing lat/lon values: leaflet does not like them
xa=check_assertions(x)
x_afcols=names(x$data) %in% xa$occur.colnames[xa$fatal] ## columns of x corresponding to a fatal assertion
x_afrows=apply(x$data[,x_afcols],1,any) ## rows of x that have a fatal assertion
these_assertions=names(x$data)[x_afcols] ## which fatal assertions are present in this data?
datlist=list(toGeoJSON(data=x$data[!x_afrows,c("Latitude...processed","Longitude...processed")],name="Am0",dest=tempdir())) ## start with the "clean" data (data rows without fatal assertions)
## now for each assertion, create a geojson formatted-file of the associated data
for (k in 1:length(these_assertions)) {
    idx=x$data[,which(x_afcols)[k]]
    datlist[k+1]=toGeoJSON(data=x$data[idx,c("Latitude...processed","Longitude...processed")],name=paste("Am",k,sep=""),dest=tempdir())
}
## create styles
sty0=styleSingle(col="white",fill="black",fill.alpha=1)
sty1=styleSingle(col="red",fill="red",fill.alpha=1)
sty2=styleSingle(col="yellow",fill="yellow",fill.alpha=1)
sty3=styleSingle(col="blue",fill="blue",fill.alpha=1)
## create the leaflet map
alamap=leaflet(data=datlist,title="Amblyornis newtonianus",base.map="mqsat",popup="mag",style=list(sty0,sty1,sty2,sty3),dest=tempdir())

## note: this would more elegantly be mapped as a single data set with categorical styling (marker colours by assertion) --- but for unknown reasons this didn't seem to work properly


##--------------------------------------------------------------------------
## Community composition and turnover

library(vegan)
library(mgcv)
library(geosphere)

## define our area of interest
wkt="POLYGON((152.5 -35,152.5 -32,140 -32,140 -35,152.5 -35))"
x=occurrences(taxon="family:Fabaceae",wkt=wkt,qa="none",download_reason_id=10) ## download legume records: this is a large family of flowering plants
x=x$data ## just take the data component
## subset and re-name some columns for convenience
x=x[,c("Matched.Scientific.Name","Taxon.Rank...matched","Kingdom...matched","Phylum...matched","Class...matched","Order...matched","Family...matched","Genus...matched","Species...matched","Longitude...processed","Latitude...processed","Event.Date...parsed","Basis.Of.Record...processed")]
names(x)=c("Scientific.Name","Taxon.Rank","Kingdom","Phylum","Class","Order","Family","Genus","Species","Longitude","Latitude","Event.Date","Basis.Of.Record")

## bin into 0.5-degree grid cells
x$Longitude=round(x$Longitude*2)/2
x$Latitude=round(x$Latitude*2)/2

## create sites-by-species data frame
## aggregate records within 0.5-degree bins
## this could also be done with e.g. the reshape library or the table() function
## NOTE - this example inherently makes some strong assumptions about *absences* in the data. Follow this example at your own risk
xsub=x$Taxon.Rank %in% c("species","subspecies","variety","form","cultivar") ## discard genus- and higher-level records
unames=unique(x[xsub,]$Scientific.Name) ## unique names 
ull=unique(x[xsub,c("Longitude","Latitude")])
xgridded=matrix(NA,nrow=nrow(ull),ncol=length(unames))
for (uli in 1:nrow(ull)) {
    lidx=xsub & x$Longitude==ull[uli,]$Longitude & x$Latitude==ull[uli,]$Latitude
    xgridded[uli,]=as.numeric(unames %in% x[lidx,]$Scientific.Name)
}
xgridded=as.data.frame(xgridded)
names(xgridded)=unames
xgridded=cbind(ull,xgridded)

## plot richness vs longitude
plot(xgridded$Longitude,apply(xgridded[,-c(1:2)],1,sum),ylab="Richness",xlab="Longitude",pch=20,col="grey25")

## calculate dissimilarity between nearby grid cells as a function of along-transect position
D=vegdist(xgridded[,-c(1:2)],'bray') ## bray-curtis dissimilarity
Dm=as.matrix(D)
Dll=apply(xgridded[,1:2],1,function(z){distVincentySphere(z,xgridded[,1:2])}) ## calculate geographic distance from longitude and latitude
closeidx=Dll>0 & Dll<100e3 ## find grid cells within 100km of each other
temp=matrix(xgridded$Longitude,nrow=nrow(xgridded),ncol=nrow(xgridded)) ## create matrix of Longitude that matches the size  of the pairwise-D matrices
## plot dissimilarity as a function of transect position
plot(temp[closeidx],Dm[closeidx],xlab="Longitude",ylab="Dissimilarity",pch=20,col="grey85")
## add smooth fit via gam()
fit=gam(d~s(tp,k=7),data=data.frame(tp=temp[closeidx],d=Dm[closeidx]))
tpp=seq(from=min(xgridded$Longitude),to=max(xgridded$Longitude),length.out=100)
fitp=predict(fit,newdata=data.frame(tp=tpp))
lines(tpp,fitp,col=1)

## clustering
cl=hclust(D,method="ave") ## UPGMA clustering
plot(cl) ## plot dendrogram
grp=cutree(cl,20) ## extract group labels at the 20-group level
## coalesce small (outlier) groups into a single catch-all
sing=which(table(grp)<5)
grp[grp %in% sing]=21 ## singletons to new combined group
grp=sapply(grp,function(z)which(unique(grp)==z)) ## renumber groups
## plot
with(xgridded,plot(Longitude,Latitude,pch=21,col=grp,bg=grp))
## or slightly nicer map plot
library(maps)
library(mapdata)
map("worldHires","Australia", xlim=c(105,155), ylim=c(-45,-10), col="gray90", fill=TRUE)
thiscol=c("#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd","#8c564b","#e377c2","#7f7f7f","#bcbd22","#17becf") ## colours for cluster
with(xgridded,points(Longitude,Latitude,pch=21,col=thiscol[grp],bg=thiscol[grp],cex=0.75))
