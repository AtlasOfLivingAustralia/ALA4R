

biocache_url <- "https://biocache-ws.ala.org.au/"

# this should return the term you need to search with?
# which is not included in the facet results- perhaps something to ask about?
# Note: facet search often seems to be slow- should use this as little as possible?
# What is the max number of fields that should be returned? 50?
# should this be cached?
# what format should this return?
# how to handle 504 errors
# can we modify /occurrences/search/ to return the number of facet values? It seems quicker
# or use /ws/occurrences/facets/download?
# should this allow 
ala_categories <- function(field) {
  max_categories <- 20
  if (missing(field)) {
    stop("`ala_categories` requires a field to search for")
  }
  url <- parse_url(biocache_url)
  url$path <- c("ws", "occurrence", "facets")
  url$query <- list(facets = field, flimit = max_categories)
  
  tryCatch(
    resp <- fromJSON(build_url(url)),
    error = function(e) {
      e$message <- paste0("\"", field, "\" is not a valid field. Use `ala_fields` for a list of valid fields.")
      stop(e)
    }
  )

  if (resp$count > max_categories) {
    warning("This field has ", resp$count, " possible values. Only the first ",
    max_categories, " will be returned.")
  }
  categories <- sapply(resp$fieldResult[[1]]$fq, function(n) {
    extract_category_value(n)
  }, USE.NAMES = FALSE)
  categories
}

# function to extract value which for some reason isn't returned
extract_category_value <- function(name) {
  str_split(name, '"')[[1]][2]
}

