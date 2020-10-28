#' Download occurrence data from the ALA
#' 
#' @param taxon_id string: single species ID or vector of species ids. Use 
#' `ala_taxa()` to get lookup species id. 
#' @param filters string: a list to narrow down the search, in the form
#' `list(field = value)`. To limit the query to a year range, use
#' `list(year = c(1990, 2000))`. This will return records between and including
#' the years provided.
#' @param area string or sf object: restrict the search to an area. Can provide
#' sf object, or a wkt string. WKT strings longer than 10000 characters will
#' not be accepted by the ALA- see the vignette for how to work around this.
#' @param data_quality_profile string: a data quality profile to apply to the
#' records. See `ala_data_profiles()` for valid profiles. Defaults to "general"
#' @param columns string: vector of columns to return in download. 
#' @param generate_doi logical: by default no DOI will be generated. Set to
#' true if you intend to use the data in a publication or similar
#' @param email string: the email address of the user performing the download
#' (required unless \code{record_count_only = TRUE}
#' @param email_notify logical: set to `FALSE` by default, set to true if you
#' would like an email notification for the download
#' @export ala_occurrences


ala_occurrences <- function(taxon_id, filters, area,
                            data_quality_profile = "ALA",
                            columns = "default",
                            email = "ala4r@ala.org.au", generate_doi = FALSE, 
                            email_notify = FALSE) {
  
  assert_that(is.flag(generate_doi))
  assert_that(is.flag(email_notify))
  assert_that(is.character(email))

  # is it worth validating the email with a regex? this won't 
  # be able to tell if the email is registered or not
  
  query <- list()
  
  if (missing(taxon_id) & missing(filters) & missing(area)) {
    # stop or allow users to download the whole ALA?
    stop("Need to provide one of `taxon id`, `filters` or `area`")
  }

  if(!missing(taxon_id)) {
    # should species id be validated?
    assert_that(is.character(taxon_id))
    taxa_query <- build_taxa_query(taxon_id)
  } else {
    taxa_query <- NULL
  }

  # validate filters
  if (!missing(filters)) {
    assert_that(is.list(filters))
    validate_filters(filters)
  } else {
    filters <- NULL
  }
  # there must be a better way to do this
  query$fq <- build_filter_query(filters, data_quality_profile,
                                      taxa_query)
  
  # not yet released
  #query$qualityProfile <- 'ALA'
  
  # Two issues with area validation: 
  # wellknown validates wkt correctly?? 
  # 504 error is returned from the server if a polygon is too complicated
  if (!missing(area)) {
    # convert area to wkt if not already
    query$wkt <- build_area_query(area)
  }

  # do a occurrence count query first? to warn the user
  
  # handle caching
  if (ala_config()$caching == "on") {
    # look for file
    
    # if file doesn't exist, continue as before
    
  }
  
  if (sum(nchar(query$fq), nchar(query$wkt), na.rm = TRUE) > 1948) {
    qid <- cache_params(query)
    query <- list(q = paste0("qid:",qid))
  }

  count <- record_count(query)
  message("This query will return ", count, " records")
  if (count == 0) {
    # should this return something?
    return()
  }

  # Add columns after getting record count
  if (missing(columns)) {
    message("No columns specified, default columns will be returned.")
    columns <- "default"
  }
  query$fields <- build_fields(columns)
  
  if (generate_doi) {
    query$mintDoi <- "true"
  }
  
  if (isFALSE(email_notify)) {
    query$emailNotify <- "false"
  }
  
  # Get data
  url <- getOption("ALA4R_server_config")$base_url_biocache
  query <- c(query, email = email, reasonTypeId = 10, dwcHeaders = "true",
                 qa = "none")
  
  status <- ala_GET(url, "ws/occurrences/offline/download",
                             params = query)

  status_url <- parse_url(status$statusUrl)
  status <- ala_GET(url, path = status_url$path)
  while (tolower(status$status) %in% c("inqueue", "running")) {
    status <- ala_GET(url, path = status_url$path)
    Sys.sleep(2)
  }
  
  download_url <- getOption("ALA4R_server_config")$base_url_biocache_download
  data <- ala_download(url = download_url,
                       path = parse_url(status$downloadUrl)$path,
                       file_type = "zip")
  
  return(data)
}

# check validity of fields?
# maybe only warn because it is possible to get results for fields not in ala_fields
# should the fields you have in the filters also be included?

build_fields <- function(cols) {
  presets <- c("default")
  default_columns <- c("latitude", "longitude","occurrence_date","taxon_name",
                       "taxon_concept_lsid", "id", "data_resource")
  additional_cols <- cols[!cols %in% presets]
  preset_selected <- cols[cols %in% presets]
    
  if (length(preset_selected) > 1) {
    stop("Only one preset option can be specified in `columns`")
  }
  else if (length(preset_selected) == 1) {
    preset_cols <- switch(preset_selected,
           "default" = default_columns)
    fields <- c(preset_cols, additional_cols)
  }
  else {
    fields <- cols
  }
  paste0(fields, collapse = ",")
}






# what to do about returned columns? it doesn't make sense that the 
# names of the columns you request are not necessarily the same as
# the ones asked for

