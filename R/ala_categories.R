

biocache_url <- "https://biocache-ws.ala.org.au/"

# this should return the term you need to search with?
# which is not included in the facet results- perhaps something to ask about?
# Note: facet search often seems to be slow- should use this as little as possible?
# What is the max number of fields that should be returned?
# should this be cached?
# what format should this return?
ala_categories <- function(field) {
  url <- parse_url(biocache_url)
  url$path <- c("ws", "occurrence", "facets")
  url$query <- list(facets = field)
  resp <- fromJSON(build_url(url))
  categories <- sapply(resp$fieldResult[[1]]$fq, function(n) {
    extract_category_value(n)
  }, USE.NAMES = FALSE)
  categories
}

# function to extract value which for some reason isn't returned
extract_category_value <- function(name) {
  str_split(name, '"')[[1]][2]
}

