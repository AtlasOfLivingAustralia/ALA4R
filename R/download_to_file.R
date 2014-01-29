download_to_file=function(url,outfile=NULL,verbose=ala_config()$verbose,...) {
    ## download from a URL using RCurl to a file
    ## we do this directly using RCurl to file, rather than reading into R memory and then dumping to file
    if (is.null(outfile)) {
        outfile=ala_cache_filename(url)
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
        check_status_code(h$value()[["status"]])
    } else {
        if (verbose) { cat(sprintf("  ALA4R: using cached file %s for %s\n",outfile,url)) }
    }
    outfile
}
