#' List valid options for a categorical field
#'
#' Use for checking filters for `ala_occurrences` and `ala_counts` are valid
#' @param field string: field to return the categories for. Use `ala_fields`
#' to view valid fields.
#' @param limit numeric: maximum number of categories to return. 20 by default.
#' @return a dataframe of field name and category name (for now)
#' @examples
#' ala_categories("basis_of_record")
#' ala_categories("state")
#' @export ala_categories


# this should return the term you need to search with?
# which is not included in the facet results- perhaps something to ask about?
# Note: facet search often seems to be slow- should use this as little as possible?
# What is the max number of fields that should be returned? 50?
# should this be cached?
# what format should this return?
# how to handle 504 errors
# can we modify /occurrences/search/ to return the number of facet values? It
# seems quicker or use /ws/occurrences/facets/download?
# should this allow
ala_categories <- function(field, limit = 20) {
  if (missing(field)) {
    stop("`ala_categories` requires a field to search for")
  }
  field <- dwc_to_ala(field)
  if (!(field %in% all_fields()$name)) {
    stop("\"", field,
         "\" is not a valid field. See valid fields with `ala_fields()`.")
  }
  assert_that(is.numeric(limit))
  url <- getOption("ALA4R_server_config")$base_url_biocache
  resp <- ala_GET(url, "ws/occurrence/facets",
                    params = list(facets = field, flimit = limit))

  if (resp$count > limit) {
    warning("This field has ", resp$count, " possible values. Only the first ",
    limit, " will be returned. Change `limit` to return more values.")
  }
  category <- sapply(resp$fieldResult[[1]]$fq, function(n) {
    extract_category_value(n)
  }, USE.NAMES = FALSE)
  cbind(field = field, as.data.frame(category))
}

# function to extract value which for some reason isn't returned
extract_category_value <- function(name) {
  str_split(name, '"')[[1]][2]
}
