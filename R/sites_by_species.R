#' Sites by species
#' 
#' A data.frame is returned as grid cells by species with values in each cell being the number of occurrences of each species. No null (all zero) species should be returned. The coordinates returned are the TOP-LEFT corner of the grid cell.
#'
#' @references Associated web services: \url{http://spatial.ala.org.au/ws}
#' @references \url{http://www.geoapi.org/3.0/javadoc/org/opengis/referencing/doc-files/WKT.html}
#' 
#' @param taxon string: the identifier to get the species data from the ala biocache. E.g. "genus:Macropus". See \code{ala_fields("occurrence_stored")} for valid field names
#' @param wkt string: Bounding area in Well Known Text (WKT) format. E.g. "POLYGON((118 -30,146 -30,146 -11,118 -11,118 -30))".
#' @param gridsize numeric: size of output grid cells in decimal degrees. E.g. 0.1 (=~10km)
#' @param SPdata.frame logical: should the output be returned as a SpatialPointsDataFrame of the sp package?
#' @param verbose logical: show additional progress information? [default is set by ala_config()]
#' @return A dataframe or a SpatialPointsDataFrame containing the species by sites data. Columns will include longitude, latitude, and each species present. Values for species are record counts (i.e. number of recorded occurrences of that taxon in each grid cell). The \code{guid} attribute of the data frame gives the guids of the species (columns) as a named character vector
#'
#' @examples
#' \dontrun{
#' # Eucalyptus in Tasmania based on a 0.1 degree grid
#' ss <- sites_by_species(taxon="genus:Eucalyptus",wkt="POLYGON((144 -43,148 -43,148 -40,
#'   144 -40,144 -43))",gridsize=0.1,verbose=TRUE)
#' head(ss[,1:6])
#' # equivalent direct POST webservice call:
#' # http://spatial.ala.org.au/alaspatial/ws/sitesbyspecies?speciesq=genus:Eucalyptus&qname=data&
#' #  area=POLYGON((144%20-43,148%20-43,148%20-40,144%20-40,144%20-43))&bs=http://biocache.ala.org.au/
#' #  ws/&movingaveragesize=1&gridsize=0.1&sitesbyspecies=1
#'
#' ## get the guid of the first species (which is the third column of the data frame, since the
#' ##  first two columns are longitude and latitude)
#' attr(ss,"guid")[1]
#' }

# TODO need way to better check input species query. If the query is incorrect, the call will fail with message along the lines of: "Error in sites_by_species(taxon = "gen:Eucalyptus", wkt = "POLYGON((144 -43,148 -43,148 -40,144 -40,144 -43))",  :   Error processing your Sites By Species request. Please try again or if problem persists, contact the Administrator."

## fails: ss <- sites_by_species(taxon="rk_genus:Eucalyptus",wkt="POLYGON((144 -43,148 -43,148 -40,144 -40,144 -43))",verbose=TRUE) 
## fails: ss <- sites_by_species(taxon="scientificNameAuthorship:Maiden",wkt="POLYGON((144 -43,148 -43,148 -40,144 -40,144 -43))",verbose=TRUE)
## fails: ss <- sites_by_species(taxon="parentGuid:http://id.biodiversity.org.au/node/apni/6337078",wkt="POLYGON((144 -43,148 -43,148 -40,144 -40,144 -43))",verbose=TRUE)

#' @export
sites_by_species <- function(taxon,wkt,gridsize=0.1,SPdata.frame=FALSE,verbose=ala_config()$verbose) {
    ## check input parms are sensible
    assert_that(is.notempty.string(taxon))
    assert_that(is.notempty.string(wkt))
    assert_that(is.numeric(gridsize), gridsize>0)
    assert_that(is.flag(SPdata.frame))
    assert_that(is.flag(verbose))
    if (SPdata.frame && !requireNamespace("sp",quietly=TRUE)) {
        ## sp package not available
        warning("sp package required for SpatialPointsDataFrame output")
        SPdata.frame <- FALSE
    }
    ##setup the key query
    base_url <- getOption("ALA4R_server_config")$base_url_alaspatial #get the base url
    ## TODO check the taxon query
    url_str <- paste(base_url,'sitesbyspecies?speciesq=',taxon,'&qname=data',sep='') #setup the base url string 
    url_str <- paste(url_str,'&area=',wkt,sep='') #append the area info
    url_str <- paste(url_str,'&bs=',getOption("ALA4R_server_config")$base_url_biocache,sep='') # append the biocache URL string
    url_str <- paste(url_str,'&movingaveragesize=1',sep='') ## append moving window average value (1 = 1 cell, which means that no moving average applied). API seems to require movingaverage to be supplied, even if it is a value of 1
    url_str <- paste(url_str,'&gridsize=',gridsize,sep='') #append the grid size
    url_str <- paste(url_str,'&sitesbyspecies=1',sep='') #define the type

    ## somehow this doesn't work: not sure why. Leave for now
    #this_url <- build_url_from_parts(getOption("ALA4R_server_config")$base_url_alaspatial,"sitesbyspecies",list(speciesq=taxon,qname="data",area=wkt,bs=getOption("ALA4R_server_config")$base_url_biocache,movingaveragesize=1,gridsize=gridsize,sitesbyspecies=1))
    ##moving window average value (1 = 1 cell, which means that no moving average applied)
    #url_str <- build_url(this_url)
    
    this_cache_file <- ala_cache_filename(url_str) ## the file that will ultimately hold the results (even if we are not caching, it still gets saved to file)
    if ((ala_config()$caching %in% c("off","refresh")) || (! file.exists(this_cache_file))) {
        pid <- cached_post(URLencode(url_str),'',caching='off',verbose=verbose) #should simply return a pid
        if (is.null(pid) || pid=="") {
            ## error - but note that we may still get a STATUS 200 from the server in this case
            ## Check the WKT string, maybe that was the problem
            if (!missing(wkt) && !isTRUE(check_wkt(wkt))) warning("WKT string may not be valid: ",wkt)
            stop("there has been an issue with this service. ",getOption("ALA4R_server_config")$notify) } #catch for these missing pid issues
        status_url <- build_url_from_parts(getOption("ALA4R_server_config")$base_url_alaspatial,"job",list(pid=pid))
        if(verbose) { cat("  waiting for sites-by-species results to become available: ") }
        status <- cached_get(URLencode(status_url),type="json",caching="off",verbose=verbose)#get the data url
        while (status$state != "SUCCESSFUL") {
            if(verbose) { cat('.') } #keep checking the status until finished
            status <- cached_get(status_url,type="json",caching="off",verbose=verbose) #get the status
            if (status$state=='FAILED') {
                ## stop if there was an error
                ## first check the wkt string: if it was invalid (or unrecognized by our checker) then warn the user
                if (!missing(wkt) && !isTRUE(check_wkt(wkt))) warning("WKT string may not be valid: ",wkt)
                if (str_detect(status$message,"No occurrences found")) {
                    ## don't consider "No occurrences found" to be an error
                    cat('\n')
                    if (ala_config()$warn_on_empty) {
                        warning("no occurrences found")
                    }
                    return(data.frame()) ## return empty results
                }
                stop(status$message)
            } 
            Sys.sleep(2)
        }; cat('\n')
        download_to_file(build_url_from_parts(getOption("ALA4R_server_config")$base_url_alaspatial,c("download",pid)),outfile=this_cache_file,binary_file=TRUE,verbose=verbose)
    } else {
        ## we are using the existing cached file
        if (verbose) { cat(sprintf("  using cached file %s\n",this_cache_file)) }
    }
    out <- read_csv_quietly(unz(this_cache_file,'SitesBySpecies.csv'),as.is=TRUE,skip=4) ## read in the csv data from the zip file; omit the first 4 header rows. use read_csv_quietly to avoid warnings about incomplete final line
    ## drop the "Species" column, which appears to be a site identifier (but just constructed from the longitude and latitude, so is not particularly helpful
    out <- out[,!names(out)=="Species"]    
    ##deal with SpatialPointsDataFrame
    if (SPdata.frame) { #if output is requested as a SpatialPointsDataFrame
        ## coerce to SpatialPointsDataFrame class
        if (nrow(out)>0) {
            out <- SpatialPointsDataFrame(coords=out[,c("Longitude","Latitude")],proj4string=CRS("+proj=longlat +ellps=WGS84"),data=out)
        }
    }
    ## rename variables
    names(out) <- rename_variables(names(out),type="other")
    ## also read the species guids
    guids <- read.csv(unz(this_cache_file,'SitesBySpecies.csv'),stringsAsFactors=FALSE,nrows=1,header=FALSE)
    guids <- guids[-1:-3] ## first 3 cols will be species lon lat
    guids <- as.character(guids)
    names(guids) <- names(out)[-2:-1]
    attr(out,"guid") <- guids
    ## warn about empty results if appropriate
    if (nrow(out)<1 && ala_config()$warn_on_empty) {
        warning("no occurrences found")
    }
    out
}
