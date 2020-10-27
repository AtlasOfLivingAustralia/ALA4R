#' Counts for ALA records
#' 
#' Takes filters in the same format as `ala_occurrences`, with an 
#' additional break down by `breakdown`
#'
#' @param taxon_id string: single species ID or vector of species ids. Use 
#' `ala_taxa()` to lookup species id. 
#' @param filters string: a list to narrow down the search, in the form
#' `list(field = value)`.
#' @param area string or sf object: restrict the search to an area. Can provide
#' sf object, or a wkt string. If the wkt string is too long, it may be
#' simplified.
#' @param data_quality_profile string: a data quality profile to apply to the
#' records. See `ala_data_profiles()` for valid profiles. Defaults to "general"
#' @param breakdown field to breakdown the counts by
#' @export ala_counts

ala_counts <- function(taxon_id, filters, area,
                       data_quality_profile = "ALA", breakdown) {
  
  query <- list()

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
  
  if (!missing(area)) {
    # convert area to wkt if not already
    query$wkt <- build_area_query(area)
  }
  
  
  if(missing(breakdown)) {
    if (sum(nchar(query$fq), nchar(query$wkt), na.rm = TRUE) > 1948) {
      qid <- cache_params(query)
      query <- list(q = paste0("qid:",qid))
    }
    return(record_count(query))
  }
  
  # check facet is valid
  validate_facet(breakdown)
  query$facets <- breakdown
  
  url <- getOption("ALA4R_server_config")$base_url_biocache
  resp <- ala_GET(url, "ws/occurrence/facets", params = query)
  
  resp$fieldResult[[1]][c("label", "count", "fq")]
}

# get just the record count for a query
# handle too long queries in here?
record_count <- function(query) {
  query$pageSize <- 0
  url <- getOption("ALA4R_server_config")$base_url_biocache
  resp <- ala_GET(url, "ws/occurrences/search", query)
  resp$totalRecords
}

validate_facet <- function(facet) {
  if (!facet %in% ala_fields()$name) {
    stop("\"", facet, "\" is not a valid breakdown field. ",
         "Use `ala_fields()` to get a list of valid options")
  }
}
 