# Internal function used to download results to a file 
# 
# @author Atlas of Living Australia \email{support@@ala.org.au}
# 
# @references \itemize{ \item ALA web service API: \url{http://api.ala.org.au/}


download_to_file=function(url,outfile=NULL,caching=ala_config()$caching,verbose=ala_config()$verbose,on_redirect=NULL,on_client_error=NULL,on_server_error=NULL,...) {
    assert_that(is.string(url))
    ## download from a URL using RCurl to a file
    ## we do this directly using RCurl to file, rather than reading into R memory and then dumping to file
    if (is.null(outfile)) {
        outfile=ala_cache_filename(url)
    } else {
        assert_that(is.string(outfile),is.dir(dirname(outfile))) ## check that outfile is a string and that it points to a valid directory
    }
    assert_that(is.flag(verbose))

    ## first check for zero-size cached files
    if (file.exists(outfile) && !(file.info(outfile)$size>0)) {
        ## file exists but is zero sized
        unlink(outfile)
    }

    ## are we using cached results?
    if ((caching %in% c("off","refresh")) || (! file.exists(outfile))) {
        if (verbose && (caching != "off")) { cat(sprintf("  ALA4R: caching %s to file %s\n",url,outfile)) }
        ## either we are not using caching, or we want to refresh the cache, or the file doesn't exist in the cache
        f = CFILE(outfile, mode="w")
        h=basicHeaderGatherer()
        curlPerform(url=url,writedata = f@ref,useragent=ala_config()$user_agent,verbose=verbose,headerfunction=h$update,...) ## can pass verbose=TRUE here for debug info if needed
        close(f)
        ## check http status here
        ## if unsuccessful, delete the file from the cache first, after checking if there's any useful info in the file body
        diag_message=""
        if ((substr(h$value()[["status"]],1,1)=="5") || (substr(h$value()[["status"]],1,1)=="4")) {
            content_length=as.numeric(h$value()["Content-Length"])
            if (!is.na(content_length) && content_length<10000) {
                ## if the file body is not too big, check to see if there's any useful diagnostic info in it
                diag_message=get_diag_message(readLines(thisfile))
            }
            unlink(outfile)
        }
        ## check status code of response. Note that we execute the on_redirect etc functions, but we don't capture the output. might wish to implement this differently?
        check_status_code(h$value()[["status"]],on_redirect=on_redirect,on_client_error=on_client_error,on_server_error=on_server_error,extra_info=diag_message)
    } else {
        if (verbose) { cat(sprintf("  ALA4R: using cached file %s for %s\n",outfile,url)) }
    }
    outfile
}

get_diag_message=function(thing) {
    ## attempt to extract message field from JSON-encoded thing
    diag_message=""
    try(diag_message <- jsonlite::fromJSON(thing)$message, silent=TRUE)
    if (is.null(diag_message)) { diag_message="" }
    diag_message
}
