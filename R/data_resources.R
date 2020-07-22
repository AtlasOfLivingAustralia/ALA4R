#' Get a list of current data resources
#'
#' Retrieve a list of all existing data resources, and basic information
#' for each data resource.
#'
#' @references \itemize{
#' \item Associated ALA web service for listing data resources:
#' \url{https://collections.ala.org.au/ws/dataResource}
#' }
#' 
#' @param druid string: data resource UID of the data resource(s)
#' @param verbose logical: show additional progress information? 
#' [default is set by ala_config()]
#' @param max integer: (optional) if all data resources are requested, max 
#' number to return, sorted by record count. Default is top 100 
#'
#' @return data frame of data resources
#' 
#' @examples
#' \dontrun{
#' # Retrieve information for a dataset
#' dr_info <- data_resources('dr375')
#' 
#' # Retrieve stats for all data resources
#' 
#' }
#' @export data_resources

data_resources <- function(druid, verbose=ala_config()$verbose, max=100) {
  this_query <- list()
  assert_that(is.flag(verbose))
  
  if(missing(druid)) {
    this_url <- paste0(getOption("ALA4R_server_config")$base_url_biocache,
                       "occurrence/facets?facets=data_resource_uid&flimit=",
                       max)
    drs <- cached_get(URLencode(this_url), type="json", verbose=verbose,
                      caching = "off")
    druid <- vapply(drs$fieldResult[[1]]$i18nCode, function(x) {
      sub(".*\\.", "", x)
      }, "")
  }
  
  assert_that(is.character(druid))
  
  dr_data <- rbindlist(lapply(druid, function(z) {
    this_url <- paste0(getOption("ALA4R_server_config")$base_url_collectory,
                       "dataResource/", z)
    data <- cached_get(URLencode(this_url), type="json", verbose=verbose,
                       on_server_error = function(z){NULL})
    if (is.null(data)) {
      df <- as.data.frame(list(uid = z, totalRecords = 0))
    }
    else {
      data$totalRecords <- as.integer(occurrences(fq=paste0 ('data_resource_uid:',z),
                                                  record_count_only = TRUE))
      data[vapply(data, is.null, logical(1))] <- NA
      # remove lists of lists 
      data <- data[!(names(data) %in% 
                       c('taxonomyCoverageHints','attributions',
                         'connectionParameters','defaultDarwinCoreValues',
                         'hubMembership','address','logoRef','imageMetadata',
                         'linkedRecordConsumers'))]
      # handle nested lists
      df <- as.data.frame(t(unlist(data)), stringsAsFactors = FALSE)
    }
    
    if (as.integer(df$totalRecords) > 0) {
      # add lifeform counts
      df <- cbind(df, breakdown_stats(z, verbose=verbose))
      
      # add download stats
      df$totalDownloadedRecords <- download_stats(z, verbose=verbose)
    }
    return(df)
  }),fill = TRUE)
  row.names(dr_data) <- NULL
  # Warn if any data resources were invalid
  if(NA %in% unique(dr_data$name) | ncol(dr_data) < 3) {
    warning("One or more of the data resources requested is invalid or
            has been deleted")
  }
  dr_data
}



download_stats <- function(id,verbose=ala_config()$verbose) {
  temp_logger_url <- "https://logger.ala.org.au/service/"
  this_url <- paste0(temp_logger_url,
                     "reasonBreakdown?eventId=1002&entityUid=", id)
  data <- cached_get(URLencode(this_url), type="json", verbose=verbose)
  return(data$all$records)
  
}

# Return kingdom stats for data resource. 
# It should also be possible to break down by other ranks in future 
breakdown_stats <- function(id, rank = "kingdom",
                            verbose=ala_config()$verbose) {
  this_url <- paste0(getOption("ALA4R_server_config")$base_url_biocache,
                     "breakdown/dataResources/",id,"?rank=",rank)
  
  data <- cached_get(URLencode(this_url), type="json", verbose=verbose)
  
  # label count with no kingdom
  data$taxa$label <- sub("^$", "kingdom_unknown", data$taxa$label)
  counts <- data$taxa$count
  names(counts) <- data$taxa$label
  
  return(as.list(counts))
}
