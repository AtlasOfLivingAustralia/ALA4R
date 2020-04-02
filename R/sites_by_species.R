#' Sites by species
#' 
#' A data.frame is returned as grid cells by species with values in each cell being the number of occurrences of each species. No null (all zero) species should be returned. The coordinates returned are the TOP-LEFT corner of the grid cell.
#'
#' @references Associated web services: \url{https://spatial.ala.org.au/ws/capabilities} see PointsToGrid/sitesBySpecies
#' @references \url{http://www.geoapi.org/3.0/javadoc/org/opengis/referencing/doc-files/WKT.html}
#' 
#' @param taxon string: the identifier to get the species data from the ala biocache. E.g. "genus:Heleioporus". See \code{ala_fields("occurrence_stored")} for valid field names
#' @param wkt string: Bounding area in Well Known Text (WKT) format. E.g. "POLYGON((118 -30,146 -30,146 -11,118 -11,118 -30))".
#' @param gridsize numeric: size of output grid cells in decimal degrees. E.g. 0.1 (=~10km)
#' @param SPdata.frame logical: should the output be returned as a SpatialPointsDataFrame of the sp package?
#' @param verbose logical: show additional progress information? [default is set by ala_config()]
#' @return A dataframe or a SpatialPointsDataFrame containing the species by sites data. Columns will include longitude, latitude, and each species present. Values for species are record counts (i.e. number of recorded occurrences of that taxon in each grid cell). The \code{guid} attribute of the data frame gives the guids of the species (columns) as a named character vector
#'
#' @examples
#' \dontrun{
#' # Eucalyptus in Tasmania based on a 0.1 degree grid
#' ss <- sites_by_species(taxon="genus:Eucalyptus", wkt="POLYGON((144 -43,148 -43,148 -40,
#'   144 -40,144 -43))", gridsize=0.1, verbose=TRUE)
#' head(ss[, 1:6])
#'
#' ## get the guid of the first species
#' attr(ss,"guid")[1]
#' #' # Steps: 1. POST webservice creates a task (use single quotes around data-binary argument)
#' #curl 'https://spatial.ala.org.au/ws/tasks/create?userId=0' --data-binary name=PointsToGrid
#' #&input={"area":[{"name":"Current extent"
#' #,"wkt":"POLYGON((144 -43,148 -43,148 -40,144 -40,144 -43))"}]
#' #,"occurrenceDensity":false,"sitesBySpecies":true,"speciesRichness":false
#' #,"species":{"q":["genus:Eucalyptus"]
#' #,"bs":"https://biocache-ws.ala.org.au/ws/","name":"genus:Eucalyptus"}
#' #,"gridCellSize":0.1,"resolution":0.01,"movingAverage":"1x1 (no moving average)"}'
#' #resp eg '{"name":"PointsToGrid","created":1552881125953,"email":"null","history":{}
#' #,"tag":"null","userId":"0","sessionId":"null","status":0,"id":<id>}
#' # 2. check status values: 0 = in_queue, 1 = running, 2 = cancelled, 3 = error, 4 = finished 
#' #curl 'https://spatial.ala.org.au/ws/tasks/status/<id>'
#' #waiting: {"status":1,"message":"getting species data","id":<id>,"name":"PointsToGrid"}
#' #complete:{"status":4,"message":"finished","id":<id>,"name":"PointsToGrid"
#' #,"history":{"1552879452131":"finished","1552879452155":"finished (id:<id>)"}
#' #,"output":[{"name":"files","file":"SitesBySpecies.csv","taskId":<id>,"id":33111}
#' #,{"name":"sxs_metadata.html","taskId":<id>,"file":"sxs_metadata.html","id":33109}
#' #,{"file":"download.zip","taskId":<id>,"name":"download.zip","id":33110}]}
#' #failed:  {"status":4,"message":"finished","id":<id>,"name":"PointsToGrid"
#' #,"history":{"1552881921817":"failed (id:<id>)"}}
#' # 3. download the zip and extract the file
#' # https://spatial.ala.org.au/ws/tasks/output/<id>/download.zip
#' # https://spatial.ala.org.au/ws/tasks/output/<id>/SitesBySpecies.csv
#' }
#' 

# TODO need way to better check input species query. If the query is incorrect, the call will fail with message along the lines of: "Error in sites_by_species(taxon = "gen:Eucalyptus", wkt = "POLYGON((144 -43,148 -43,148 -40,144 -40,144 -43))",  :   Error processing your Sites By Species request. Please try again or if problem persists, contact the Administrator."

## fails: ss <- sites_by_species(taxon="rk_genus:Eucalyptus",wkt="POLYGON((144 -43,148 -43,148 -40,144 -40,144 -43))",verbose=TRUE) 
## fails: ss <- sites_by_species(taxon="scientificNameAuthorship:Maiden",wkt="POLYGON((144 -43,148 -43,148 -40,144 -40,144 -43))",verbose=TRUE)
## fails: ss <- sites_by_species(taxon="parentGuid:http://id.biodiversity.org.au/node/apni/6337078",wkt="POLYGON((144 -43,148 -43,148 -40,144 -40,144 -43))",verbose=TRUE)

#' @export
sites_by_species <- function(taxon, wkt, gridsize=0.1, SPdata.frame=FALSE, verbose=ala_config()$verbose) {
  ## check input parms are sensible
  assert_that(is.notempty.string(taxon))
  assert_that(is.notempty.string(wkt))
  assert_that(is.numeric(gridsize), gridsize>0)
  assert_that(is.flag(SPdata.frame))
  assert_that(is.flag(verbose))
  if (SPdata.frame && !requireNamespace("sp", quietly=TRUE)) {
    ## sp package not available
    warning("sp package required for SpatialPointsDataFrame output")
    SPdata.frame <- FALSE
  }

  ## create the task
  tasks_url <- paste(getOption("ALA4R_server_config")$base_url_spatial, "tasks/", sep="") #get the base url, append /tasks/
  url_str <-  paste(tasks_url,"create?userId=0&sessionId=",ala_sourcetypeid(), sep="")

  # --data-binary values go into cached_post(...body...) and subsequently curlPeform(postfields=body) 
  resolution=0.01
  body <- paste('name=PointsToGrid&input={"area":[{"name":"Current extent","wkt":"', wkt, '"}]', sep="")
  body <- paste(body,',"occurrenceDensity":false', sep="")
  body <- paste(body,',"sitesBySpecies":true', sep="")
  body <- paste(body,',"speciesRichness":false', sep="")
  body <- paste(body,',"species":{"q":["', taxon, '"]', sep="")
  body <- paste(body,',"bs":"', getOption("ALA4R_server_config")$base_url_biocache,'"', sep="")
  body <- paste(body,',"name":"',taxon,'"}', sep="")
  body <- paste(body,',"gridCellSize":',gridsize, sep="")
  body <- paste(body,',"resolution":',resolution, sep="")
  body <- paste(body,',"movingAverage":"1x1 (no moving average)"}', sep="")

  # ## somehow this doesn't work: not sure why. Leave for now
  # #this_url <- build_url_from_parts(getOption("ALA4R_server_config")$base_url_alaspatial, "sitesbyspecies", list(speciesq=taxon, qname="data", area=wkt, bs=getOption("ALA4R_server_config")$base_url_biocache, movingaveragesize=1, gridsize=gridsize, sitesbyspecies=1))
  # ##moving window average value (1 = 1 cell, which means that no moving average applied)
  # #url_str <- build_url(this_url)
   
  this_cache_file <- ala_cache_filename(paste(url_str,body,sep="")) ## the file that will ultimately hold the results (even if we are not caching, it still gets saved to file)
  if ((ala_config()$caching %in% c("off", "refresh")) || (! file.exists(this_cache_file))) {
    create_response <- cached_post(URLencode(url_str), body=body, encoding="json", type="json", content_type="application/x-www-form-urlencoded", caching="off", verbose=verbose) #returns json
    id <- create_response$id
    if (is.null(id) || id=="") {
      ## error - but note that we may still get a STATUS 200 from the server in this case
      ## Check the WKT string, maybe that was the problem
      if (!missing(wkt) && !isTRUE(check_wkt(wkt))) warning("WKT string may not be valid: ", wkt)
      ## NOTE May 2018: in fact it seems that the server can fail to parse the WKT string even if (apparently) valid
      ## see https://github.com/AtlasOfLivingAustralia/biocache-service/issues/225
      stop("there has been an issue with this service. ", getOption("ALA4R_server_config")$notify) } #catch for these missing pid issues
    status_url <- paste(tasks_url, "status/", id, sep="")
    if (verbose) message(paste("Waiting for sites-by-species results to become available: ",status_url,sep=""), appendLF=FALSE)
    status_response <- cached_get(URLencode(status_url), type="json", caching="off", verbose=verbose) #get the status
    while (status_response$status <= 1) {
      if (verbose) message(".", appendLF=FALSE) ## keep checking the status until finished
      status_response <- cached_get(URLencode(status_url), type="json", caching="off", verbose=verbose) #get the status
      if ((status_response$status >= 2) && (is.null(status_response$output))) {
        ## stop if there was an error
        ## first check the wkt string: if it was invalid (or unrecognized by our checker) then warn the user
        if (!missing(wkt) && !isTRUE(check_wkt(wkt))) warning("WKT string may not be valid: ", wkt)
        stop(status_response$history)
      }
      Sys.sleep(2)
    }
    message("") ## to get LF
    output_url <- paste(tasks_url,"output/",id,"/download.zip",sep="")
    download_to_file(output_url, outfile=this_cache_file, binary_file=TRUE, verbose=verbose)
  } else {
    ## we are using the existing cached file
    if (verbose) message(sprintf("Using cached file %s", this_cache_file))
  }
  out <- read_csv_quietly(unz(this_cache_file, "SitesBySpecies.csv"), as.is=TRUE, skip=4) ## read in the csv data from the zip file; omit the first 4 header rows. use read_csv_quietly to avoid warnings about incomplete final line
  ## drop the "Species" column, which appears to be a site identifier (but just constructed from the longitude and latitude, so is not particularly helpful
  out <- out[, names(out)!="Species"]
  ##deal with SpatialPointsDataFrame
  if (SPdata.frame) { #if output is requested as a SpatialPointsDataFrame
    ## coerce to SpatialPointsDataFrame class
    if (nrow(out)>0) {
      out <- SpatialPointsDataFrame(coords=out[, c("Longitude", "Latitude")], proj4string=CRS("+proj=longlat +ellps=WGS84"), data=out)
    }
  }
  ## rename variables
  names(out) <- rename_variables(names(out), type="other")
  ## also read the species guids
  guids <- read.csv(unz(this_cache_file, "SitesBySpecies.csv"), stringsAsFactors=FALSE, nrows=1, header=FALSE)
  guids <- guids[-1:-3] ## first 3 cols will be species lon lat
  guids <- as.character(guids)
  names(guids) <- names(out)[-2:-1]
  attr(out, "guid") <- guids
  ## warn about empty results if appropriate
  if (nrow(out)<1 && ala_config()$warn_on_empty) {
    warning("no occurrences found")
  }
  out
}
