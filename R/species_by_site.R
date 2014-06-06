#' Species by site
#' 
#' A data.frame is returned as grid cells by species with values in each cell being the number 
#' of occurrences of each species. No null (all zero) species should be returned. The coordinates
#' returned are the TOP-LEFT corner of the grid cell.
#'
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' 
#' @references \url{http://api.ala.org.au/ws}
#' @references \url{http://spatial.ala.org.au/ws}
#' @references \url{http://www.geoapi.org/3.0/javadoc/org/opengis/referencing/doc-files/WKT.html}
#' 
#' @param taxon string: the identifier to get the species data from the ala biocache. E.g. "genus:Macropus".
#' @param wkt string: Bounding area in Well Known Text (WKT) format. E.g. "POLYGON((118 -30,146 -30,146 -11,118 -11,118 -30))".
#' @param gridsize numeric: size of output grid cells in decimal degrees. E.g. 0.1 (=~10km)
#' @param SPdata.frame logical: should the output be returned as a SpatialPointsDataFrame of the sp package?
#' @param verbose logical: show additional progress information? [default is set by ala_config()]
#' @return A dataframe or a SpatialPointsDataFrame containing the species by sites data.... 
#'
#' @examples
#' \dontrun{
#' #download the matrix (data.frame) of Eucalyptus in Tasmania based on a 0.1 degree grid
#' tt=species_by_site(taxon='genus:Eucalyptus',wkt='POLYGON((144 -43,148 -43,148 -40,144 -40,144 -43))',gridsize=0.1,verbose=TRUE)
#' str(tt)
#' # equivalent direct webservice call: POST http://spatial.ala.org.au/alaspatial/ws/sitesbyspecies?speciesq=genus:Eucalyptus&qname=data&area=POLYGON((144%20-43,148%20-43,148%20-40,144%20-40,144%20-43))&bs=http://biocache.ala.org.au/ws/&movingaveragesize=1&gridsize=0.1&sitesbyspecies=1
#' }

# TODO Lee to add dataframe output specifications (sent 23/4/2014)
# TODO need way to better check input species query
# TODO precheck of taxon 
#'
#' @export
species_by_site = function(taxon,wkt,gridsize=0.1,SPdata.frame=FALSE,verbose=ala_config()$verbose) {
    ##TODO data checks
    ##todo setup output structure and class
    ##todo api movingaveragesize is unnecessary... consider removing...
    ## check input parms are sensible
    assert_that(is.string(taxon))
    assert_that(is.string(wkt))
    assert_that(is.logical(SPdata.frame))
	
    ## wkt string supplied and valid?
    if (str_length(wkt)>0) {
        if (! check_wkt(wkt)) {
            warning("WKT string appears to be invalid: ",wkt)
        }
    }
    
    ##setup the key query
    base_url = ala_config()$base_url_alaspatial #get the base url
    url_str = paste(base_url,'sitesbyspecies?speciesq=',taxon,'&qname=data',sep='') #setup the base url string 
    url_str = paste(url_str,'&area=',wkt,sep='') #append the area info
    url_str = paste(url_str,'&bs=',ala_config()$base_url_biocache,sep='') # append hte biocache URL string
    url_str = paste(url_str,'&movingaveragesize=1',sep='') #append hte moving window average value (1 = 1 cell, which means that no moving average applied)
    url_str = paste(url_str,'&gridsize=',gridsize,sep='') #append hte grid size
    url_str = paste(url_str,'&sitesbyspecies=1',sep='') #define the type
    
    this_cache_file=ala_cache_filename(url_str) ## the file that will ultimately hold the results (even if we are not caching, it still gets saved to file)
    if ((ala_config()$caching %in% c("off","refresh")) || (! file.exists(this_cache_file))) {
        pid = cached_post(URLencode(url_str),'',caching='off') #should simply return a pid
        if (pid=="") { stop("there has been an issue with this service. Please try again but if the issue persists, contact support@@ala.org.au") } #catch for these missing pid issues
        status_url = paste('http://spatial.ala.org.au/alaspatial/ws/job?pid=',pid,sep='')
        status=cached_get(URLencode(status_url),type="json",caching="off")#get the data url
        while (status$state != "SUCCESSFUL") {
            if(verbose) { cat('.') } #keep checking the status until finished
            status=cached_get(status_url,type="json",caching="off") #get the status
            if (status$state=='FAILED') { stop(status$message) } #stop if there was an error
            Sys.sleep(2)
        }; cat('\n')
        download_to_file(paste('http://spatial.ala.org.au/alaspatial/ws/download/',pid,sep=''),outfile=this_cache_file,binary_file=TRUE)
    } else {
        ## we are using the existing cached file
        if (verbose) { cat(sprintf("  ALA4R: using cached file %s\n",this_cache_file)) }
    }
    out = read.csv(unz(this_cache_file,'SitesBySpecies.csv'),as.is=TRUE,skip=4) #read in the csv data from the zip file; omit the first 4 header rows

    ##deal with SpatialPointsDataFrame
    if (SPdata.frame) { #if output is requested as a SpatialPointsDataFrame
        if (is.element('sp', installed.packages()[,1])) { #if sp package is available
            require(sp) #load the library		
            ## coerce to SpatialPointsDataFrame class
            if (nrow(out)>0) {
                out=SpatialPointsDataFrame(coords=out[,c("Longitude","Latitude")],proj4string=CRS("+proj=longlat +ellps=WGS84"),data=out)
            }
        } else {
            warning('sp package needs to be installed; data output is output as a simple dataframe')
        }
    }
    ##return the output
    out
}
