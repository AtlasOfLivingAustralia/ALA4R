#' Counts for ALA records
#' 
#' Takes filters in the same format as `ala_occurrences`, with an 
#' additional break down by `breakdown`
#'
#' @param taxon_id string: single species ID or vector of species ids. Use 
#' `ala_taxa()` to get lookup species id. 
#' @param filters string: a list to narrow down the search, in the form `list(field = value)`.
#'
#' @param area string or sf object: restrict the search to an area. Can provide
#' sf object, or a wkt string. If the wkt string is too long, it may be simplified.
#' @param breakdown field to breakdown the counts by
#' @export ala_counts

ala_counts <- function(taxon_id, filters, area, breakdown) {
  
  query <- list()
  
  if(!missing(taxon_id)) {
    # should species id be validated?
    query$fq <- build_taxa_query(taxon_id)
  }
  
  if (!missing(area)) {
    # convert area to wkt if not already
    if (is.character(area)) {
      # maybe give a more helpful error message here?
      stopifnot(wellknown::lint(area))
    } else if (identical(class(area), c("sf", "data.frame"))) {
      area <- build_wkt(area)
    } else {
      stop("Area must be either a wkt string or an sf spatial object.")
    }
    query$wkt <- area
  }
  
  if (!missing(filters)) {
    filters <- as.list(filters)
    validate_filters(filters)
    # there must be a better way to do this
    if (!is.null(query$fq)) {
      query$fq <- paste0("(",query$fq, ' AND ',
                         fq = build_filter_query(filters), ")")
    }
    else {
      query$q <- build_filter_query(filters)
    }
  }
  
  if(missing(breakdown)) {
    return(record_count(query))
  }
  
  # check facet is valid
  validate_facet(breakdown)
  query$facets <- breakdown
  
  url <- parse_url(getOption("ALA4R_server_config")$base_url_biocache)
  url$path <- c("ws", "occurrence", "facets")
  url$query <- query
  
  resp <- fromJSON(build_url(url))
  resp$fieldResult[[1]][c("label", "count", "fq")]
}

validate_facet <- function(facet) {
  if (!facet %in% ala_fields()$name) {
    stop("\"", facet, "\" is not a valid breakdown field. ",
         "Use `ala_fields()` to get a list of valid options")
  }
}
 