#' Get a list of current data resources
#'
#' Retrieve a list of all existing data resources, and basic information
#' for each data resource. For data resource stats, use
#' `data_resource_details.R`
#'
#' @references \itemize {
#' \item Associated ALA web service for listing data resources:
#' \url{https://collections.ala.org.au/ws/dataResource}
#' }
#' 
#' @param druid string: data resource UID of the data resource(s)
#' @param max integer: (optional) if all data resources are requested, max 
#' number to return, sorted by record count. Default is top 100 
#'
#' @return data frame of data resources
#' @export data_resources

data_resources <- function(druid, verbose=ala_config()$verbose, max=100) {
  
  this_query <- list()
  
  assert_that(is.flag(verbose))
  
  if (is.null(getOption("ALA4R_server_config")$base_url_collectory)) {
    base_url <- "https://collections.ala.org.au/ws/"
  }
  else {
    base_url <- getOption("ALA4R_server_config")$base_url_collectory
  }
  
  if(missing(druid)) {
    this_url <- paste0(getOption("ALA4R_server_config")$base_url_biocache,
                       "occurrence/facets?facets=data_resource_uid&flimit=",max)
    drs <- cached_get(URLencode(this_url), type="json", verbose=verbose,
                      caching = "off")
    druid <- sapply(drs$fieldResult[[1]]$i18nCode, function(x) {
      sub(".*\\.", "", x)
      })
  }
  
  assert_that(is.character(druid))
  
  dr_data <- do.call(rbind, lapply(druid, function(z) {
    this_url <- paste0(base_url, "dataResource/", z)
    data <- cached_get(URLencode(this_url), type="json", verbose=verbose,
                       on_server_error = function(z){NULL})
    if (is.null(data)) {
      data <- as.data.frame(list(uid = z, totalRecords = 0))
    }
    else {
      cols <- c("uid", "name", "licenseType", "dateCreated","lastUpdated","doi",
                "Animalia","Bacteria", "Plantae","Chromista","Fungi","Protista",
                "Protozoa","Virus","Unknown","totalDownloadedRecords")
      data <- data[names(data) %in% cols]
      data[sapply(data, is.null)] <- NA
      
      data$totalRecords <- occurrences(fq=paste0('data_resource_uid:',z),
                                  record_count_only = TRUE)
    }
    
    df <- as.data.frame(data)
    
    if (df$totalRecords>0) {
      # add lifeform counts
      df <- cbind(df, lifeform_stats(z))
      
      # add download stats
      df$totalDownloadedRecords <- download_stats(z)
    }
    else {
      # add missing cols so rows are of equal length
      df[cols[!(cols %in% colnames(df))]] = NA
    }
    
    return(df)
    }))
  row.names(dr_data) <- NULL
  # Warn if any data resources were invalid
  if(NA %in% unique(dr_data$name)) {
    warning("One or more of the data resources requested is invalid or
            has been deleted")
  }
  dr_data
}


download_stats <- function(id) {
  temp_logger_url <- "https://logger.ala.org.au/service/"
  this_url <- paste0(temp_logger_url,
                     "reasonBreakdown?eventId=1002&entityUid=", id)
  data <- cached_get(URLencode(this_url), type="json", verbose=verbose)
  return(data$all$records)
  
}

# Return lifeform stats for data resource. 
# It should also be possible to break down by other ranks in future 
lifeform_stats <- function(id, rank = "lifeform") {
  this_url <- paste0(getOption("ALA4R_server_config")$base_url_biocache,
                     "breakdown/dataResources/",id,"?rank=",rank)
  
  data <- cached_get(URLencode(this_url), type="json", verbose=verbose)
 
  kingdoms <- c("Animalia","Bacteria", "Plantae","Chromista",
                   "Fungi","Protista","Protozoa","Virus","Unknown")
  
  # label count with no kingdom
  data$taxa$label <- sub("^$", "Unknown", data$taxa$label)
  counts <- data$taxa$count
  names(counts) <- data$taxa$label
  
  
  # add 0 counts for kingdoms not present
  zero_counts <- rep(0,length(kingdoms[!(kingdoms %in% names(counts))]))
  names(zero_counts) <- kingdoms[!(kingdoms %in% names(counts))]
  
  return(as.list(append(counts,zero_counts)))
}
