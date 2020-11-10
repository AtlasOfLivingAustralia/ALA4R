#' List species matching criteria
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
#' @param caching string: should the results be cached? Either "on" or "off"
#' @return dataframe of matching species, with optional additional details.
#' @export ala_checklist

# If the facet search download worked properly, this should also return counts. But, as this
# function is likely to be used to download long species lists, for now we will make do
# without the counts- otherwise will require lots of pagination.
ala_checklist <- function(taxon_id, filters, area, caching = FALSE) {
  # Use facet search with additional options

  url <- getOption("ALA4R_server_config")$base_url_biocache

  query <- list()

  if (missing(taxon_id) & missing(filters) & missing(area)) {
    warning("This query will return a list of all species in the ALA")
  }

  if(!missing(taxon_id)) {
    # should species id be validated?
    if (inherits(taxon_id, "data.frame") &&
        "taxon_concept_id" %in% colnames(taxon_id)) {
      taxon_id <- taxon_id$taxon_concept_id
    }
    assert_that(is.character(taxon_id))
    taxa_query <- build_taxa_query(taxon_id)
  } else {
    taxa_query <- NULL
  }
  
  # validate filters
  if (!missing(filters)) {
    assert_that(is.data.frame(filters))
    filters$name <- dwc_to_ala(filters$name)
    filter_query <- build_filter_query(filters)
  } else {
    filter_query <- NULL
  }
  if (!is.null(filter_query) || !is.null(taxa_query)) {
    query$fq <- paste0("(", paste(c(taxa_query, filter_query), collapse = ' AND '), ")")
  }
  
  

  if (!missing(area)) {
    # convert area to wkt if not already
    query$wkt <- build_area_query(area)
  }

  query$facets <- "species_guid"
  query$lookup  <- "true"
  query$counts <- "true"
  
  path <- "ws/occurrences/facets/download"
  
  cache_file <- cache_filename(c(url, path, unlist(query)), ext = ".csv")
  
  if (caching && file.exists(cache_file)) {
    message("Using cached file")
    return(read.csv(cache_file))
  }

  data <- ala_download(url, path = path, params = query,
                       cache_file = cache_file)
  # overwrite file with fixed names
  names(data) <- rename_columns(names(data), type = "checklist")
  write.csv(data, cache_file)
  return(data)
}
