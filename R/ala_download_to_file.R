ala_download_to_file=function(url,...) {
    ## download from a URL using RCurl to a file
    ## we do this directly using RCurl to file, rather than reading into R memory and then dumping to file
    ## TODO: provide caching functionality
    outfile=tempfile() ## this to be replaced with proper caching mechanism for file naming
    f = RCurl::CFILE(outfile, mode="w")
    RCurl::curlPerform(url=url,writedata = f@ref,useragent=ala_config()$user_agent,...) ## can pass verbose=TRUE here for debug info if needed
    RCurl::close(f)
    ## TODO: check error status
    outfile
}
