ala_cache_filename=function(url) {
    ## returns the cache filename associated with the given url
    ## note that this file may not actually exist
    
    x=digest(url) ## use md5 hash of url as the cache filename, because it's an easy way of generating unique filenames based on the URL
    ## the downside is that we can't easily go from a cache filename back to its URL
    file.path(ala_config()$cache_directory,x)
}
