#' Get a list of current data resources
#'
#' Retrieve a list of all existing data resources, and basic information
#' for each data resource.
#' 
#' @param druid string: data resource UID of the data resource(s)
#' @param verbose logical: show additional progress information? 
#' [default is set by ala_config()]
#' @param max integer: (optional) if all data resources are requested, max 
#' number to return, sorted by record count. Default is top 100 
#' @param extra: string: (optional) additional field to retrieve information for
#' the data resource. Must be a valid field. 
#'
#' @return data frame of data resources
#' 
#' @references \itemize{
#' \item Associated ALA web service for listing data resources:
#' \url{https://collections.ala.org.au/ws/dataResource}
#' }
#' 
#' @examples
#' \dontrun{
#' # Retrieve information for a dataset
#' dr_info <- data_resources('dr375')
#' 
#' # Retrieve stats for top 10 data resources wuth assertions breakdown
#' dr_info <- data_resources(max = 10, extra = 'assertions')
#' }
#' @export data_resources

data_resources <- function(druid, verbose=ala_config()$verbose, max=100,
                           extra) {
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

  facet_search <- FALSE
  if (!missing(extra)) {
    facet_search <- TRUE
  }
  dr_data <- data.table::rbindlist(lapply(druid, function(z) {
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
      # add extra cols
      if (facet_search) {
        # paginate facet search
        facet_result <- occurrence_facets(query = 
                                            paste0("data_resource_uid:", z), 
                                          facet =  extra,
                                          verbose = verbose)
        total <- facet_result$meta$count
        remaining <- total - nrow(facet_result$data)
        data <- facet_result$data
        
        while(remaining > 0) {
          facet_result <- occurrence_facets(query = paste0("data_resource_uid", z),
                                   facet = extra, start = nrow(data),
                                   verbose = verbose)
          data <- rbind(data, facet_result$data)
          remaining <- total - nrow(data)
        }
        
        colnames(facet_cols) <- data$label
        facet_cols <- data.frame(t(data))[2,]
        df <- cbind(df, facet_cols, row.names = NULL)
      }
     
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
