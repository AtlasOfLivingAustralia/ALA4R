#' Get facet counts for occurrence data
#'
#'
#' @param facet string: facet to be used. Currently only one facet is supported
#' @param query string: query of the form field:value e.g. q = genus:Macropus
#' or a free text search e.g. q = Macropus
#' @param start numeric: (optional) (positive integer) start offset for the
#' results
#' @param page_size numeric: (optional) (positive integer) maximum number of
#' records to return. Defaults to the server-side value - currently 30
#' @param verbose logical: show additional progress information?
#' [default is set by ala_config()]
#'
#' @references Associated ALA web service for facet counts \itemize{
#' \item \url{https://api.ala.org.au/#ws3}
#' }
#' @export occurrence_facets


# Notes: offline method does not appear to work, defaulting to non-offline
# method
occurrence_facets <- function(facet, query, start, page_size,
                              verbose = ala_config()$verbose) {
 this_query <- list()
 if (!missing(query)) {
   if (is.factor(query)) {
     query <- as.character(query)
   }
   assert_that(is.notempty.string(query))
   this_query$q <- query

 }
 assert_that(is.character(facet))
 assert_that(length(facet) == 1)

 valid_facets <- ala_fields("occurrence", as_is = TRUE)$name
 facet <- fields_name_to_id(fields = facet, fields_type = "occurrence")
 if (!(facet %in% valid_facets)) {
   stop("invalid facets requested: ", facet,
        ". See ", getOption("ALA4R_server_config")$fields_function,
        "(\"", "occurrence", "\",as_is=TRUE)")
 }
 this_query$facets <- facet


 if (!missing(start)) {
    assert_that(is.numeric(start))
    this_query$foffset <- start
 }

 if (!missing(page_size)) {
    assert_that(is.numeric(page_size))
    this_query$flimit <- page_size
 }

 this_url <- build_url_from_parts(
    getOption("ALA4R_server_config")$base_url_biocache,
    "occurrence/facets", query = this_query)
 x <- cached_get(this_url, type = "json", verbose = verbose)


 out <- list(meta = x[!names(x) %in% c("fieldName", "fieldResult")])
 out$meta$facet <- x$fieldName
 data <- x$fieldResult[[1]]

 out$data <- data[names(data) %in% c("label", "count")]
 return(out)

}
