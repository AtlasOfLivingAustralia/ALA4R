download_to_file=function(url,outfile=NULL,verbose=ala_config()$verbose,on_redirect=NULL,on_client_error=NULL,on_server_error=NULL,...) {
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
    if ((ala_config()$caching %in% c("off","refresh")) || (! file.exists(outfile))) {
        if (verbose && (ala_config()$caching != "off")) { cat(sprintf("  ALA4R: caching %s to file %s\n",url,outfile)) }
        ## either we are not using caching, or we want to refresh the cache, or the file doesn't exist in the cache
        f = CFILE(outfile, mode="w")
        h=basicHeaderGatherer()
        curlPerform(url=url,writedata = f@ref,useragent=ala_config()$user_agent,verbose=verbose,headerfunction=h$update,...) ## can pass verbose=TRUE here for debug info if needed
        close(f)
        ## check http status here
        ## if unsuccessful, delete the file from the cache first
        if ((substr(h$value()[["status"]],1,1)=="5") || (substr(h$value()[["status"]],1,1)=="4")) {
            unlink(outfile)
        }
        ## check status code of response. Note that we execute the on_redirect etc functions, but we don't capture the output
        ## might wish to implement this differently?
        check_status_code(h$value()[["status"]],on_redirect=on_redirect,on_client_error=on_client_error,on_server_error=on_server_error)
        
    } else {
        if (verbose) { cat(sprintf("  ALA4R: using cached file %s for %s\n",outfile,url)) }
    }
    outfile
}
