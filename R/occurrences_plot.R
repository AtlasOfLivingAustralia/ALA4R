###DO NOT TEST YET

#' Quick plot of occurrence data
#' 
#' Generates a plot of occurrence data retrieved using \code{occurrences}.
#' 
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' 
#' @param x an list object that has been downloaded using \code{occurrences}
#' @param file name of file to be created; defaults to RPlots.pdf
#' @param qa string vector: list of record issues to be mapped; see \code{ala_fields("assertions")} for valid values, or use "none" to include no record issues
#' @param use_data_table logical: if TRUE, attempt to read the data.csv file using the fread function from the data.table package. Requires data.table to be available. If this fails, or use_data_table is FALSE, then read.table will be used (which may be slow)
#' 
#' @return Generates a pdf that maps the distributions.
#' 
#' @examples
#' \dontrun{ 
#' #download some observations
#' x=occurrences(taxon="golden bowerbird",download_reason_id=10)
#' 
#' 
#' }
#' @export 

### function to visualize data
### NEED to check inputs
occurrences.plot(x, file='Rplots.pdf', grouped=TRUE, 
	qa=c('geospatialIssue','temporalIssue','taxonomicIssue','detectedOutlier','Suspected.outlier','data.Are.Generalised','species.Outside.Expert.Range'),...) 
{
	require(sp) #ensure sp package
	
	#check x
	#check file
	#check grouped
	#check qa
	
	
	###note this will ideally be states
	data(aus) #load('data/aus') #load the data
	
	###plot function to be used
	tplot = function(xx,Main,...) {
		image(aus,col='grey') #draw the base australia
		title(main=Main)
		degAxis(1); degAxis(2) #add on the axis
		points(xx$Longitude...processed,xx$Latitude...processed,pch=19,col='black')
		legend.cols = heat.colors(length(qa)) #define the legend colors
		column.checks = c('geospatialIssue','temporalIssue','taxonomicIssue','detectedOutlier','Suspected.outlier','data.Are.Generalised','species.Outside.Expert.Range') #define the columns to be checked
		for (ii in 1:length(column.checks)) {
			if(column.checks[ii] %in% colnames(xx)) {
				roi = which(xx[,column.checks[ii]]==TRUE) #define the points that have the issue
				points(xx$Longitude...processed[roi],xx$Latitude...processed[roi],pch=19,col=legend.cols[ii])
			}
		}
		legend('bottomleft',legend=c('assumed good','geospatial issue','temporal issue','taxonomic issue','suspected outlier', 'generalized locations'),pch=19,col=c('black',legend.cols),bty='n')
	}
	
	###generate the plots
	pdf(outputname)
		if (grouped) {
			tplot(x$data,Main='all species')
		} else {
			for (spp in unique(x$data$Scientific.Name)) {
				if (spp!="") { 
					tplot(x$data[which(x$data$Scientific.Name==spp),],Main=spp)
				} else {
					tplot(x$data[which(x$data$Scientific.Name==spp),],Main='unmatched species name')
				}
			}
		}
	dev.off()
}


