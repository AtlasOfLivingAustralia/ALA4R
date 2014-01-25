ala_cache_filename=function(url) {
    ## returns the cache filename associated with the given url
    ## note that this file may not actually exist
    
    ## make sure that URL query parms are sorted, so that the same URL with different query ordering hits the same cache file
    this_url=parse_url(url) ## decompose URL into components
    this_url$query=this_url$query[order(names(this_url$query))] ## sort query terms by name
    url=build_url(this_url) ## reconstruct URL string
    
    x=digest(url) ## use md5 hash of url as the cache filename, because it's an easy way of generating unique filenames based on the URL
    ## the downside is that we can't easily go from a cache filename back to its URL
    file.path(ala_config()$cache_directory,x)
}
