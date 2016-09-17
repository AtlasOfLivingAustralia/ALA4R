#' Returns the name of the cache file associated with the given URL. Note that this file 
#' may not actually exist, this function just provides the mapping from URL to filename
#' 
#' @references \url{http://api.ala.org.au/}
#' @seealso \code{ala_config} for cache settings, particularly the cache directory
#'  
#' @param url string: the URL
#' @return string: the file path and name
#' 
#' @examples
#' ala_cache_filename("http://biocache.ala.org.au/ws/index/fields")
#' 
#' @export ala_cache_filename
ala_cache_filename <- function(url) {
    assert_that(is.string(url))
    ## returns the cache filename associated with the given url
    ## note that this file may not actually exist, this function just provides the mapping from URL to filename
    
    ## make sure that URL query parms are sorted, so that the same URL with different query ordering hits the same cache file
    this_url <- parse_url(url) ## decompose URL into components
    if (! is.null(this_url$query)) {
        this_url$query <- this_url$query[order(names(this_url$query))] ## sort query terms by name
        ## may also wish to sort items within some query parms, e.g. fl is a comma-separated list of fields
        if (! is.null(this_url$query$fl)) {
            this_url$query$fl <- str_c(sort(str_split(this_url$query$fl,",")[[1]]),collapse=",")
        }
        ## Note that this means that the ordering of columns coming back from the query may not match the ordering that the user provided the list of fields in. Need to decide if this is acceptable
    }
    url <- build_url(this_url) ## reconstruct URL string
    
    x <- digest(url) ## use md5 hash of url as the cache filename, because it's an easy way of generating unique filenames based on the URL
    ## the downside is that we can't easily go from a cache filename back to its URL
    file.path(ala_config()$cache_directory,x)
}
