# HTTP POST with caching
# 
# Convenience wrapper for web POST operations. Caching, setting the user-agent string, and basic checking of the result are handled.
# 
# @param url string: the url of the page to retrieve
# @param body string: the body to POST
# @param type string: the expected content type. Either "text" (default), "json", or "filename" (this caches the content directly to a file and returns the filename without attempting to read it in)
# @param caching string: caching behaviour, by default from ala_config()$caching
# @param ... additional arguments passed to curlPerform
# @return for type=="text" the content is returned as text. For type=="json", the content is parsed using jsonlite::fromJSON. For "filename", the name of the stored file is returned.
# @details Depending on the value of caching, the page is either retrieved from the cache or from the url, and stored in the cache if appropriate. The user-agent string is set according to ala_config()$user_agent. The returned response (if not from cached file) is also passed to check_status_code().
# @author Atlas of Living Australia \email{support@@ala.org.au}
# @references \url{http://api.ala.org.au/}
# @examples
#
# # production server (this service not running at the time of writing - see test server example below)
# # out = cached_post(url="http://biocache.ala.org.au/ws/species/lookup/bulk",body=jsonlite::toJSON(list(names=c("Macropus rufus","Grevillea"))),type="json")
# # test server
# out = cached_post(url="http://118.138.243.151/bie-service/ws/species/lookup/bulk",body=jsonlite::toJSON(list(names=c("Macropus rufus","Bilbo baggins"))),type="json")


cached_post=function(url,body,type="text",caching=ala_config()$caching,verbose=ala_config()$verbose,...) {
    assert_that(is.string(url))
    assert_that(is.string(body))
    assert_that(is.string(type))
    type=match.arg(tolower(type),c("text","json","filename"))
    assert_that(is.string(caching))
    caching=match.arg(tolower(caching),c("on","off","refresh"))
    assert_that(is.flag(verbose))

    if (identical(caching,"off") && !identical(type,"filename")) {
        ## if we are not caching, retrieve our page directly without saving to file at all
        if (verbose) { cat(sprintf("  ALA4R: POSTing URL %s",url)) }
        x=POST(url=url,body=body,user_agent(ala_config()$user_agent))
        check_status_code(x)
        x=content(x,as="text")
        if (identical(type,"json")) {
            x=jsonlite::fromJSON(x) ## do text-then-conversion, rather than content(as="parsed") to avoid httr's default use of RJSONIO::fromJSON
        }
        x
    } else {
        ## use caching
        thisfile=digest(paste(url,body)) ## use md5 hash of url plus body as cache filename
        thisfile=file.path(ala_config()$cache_directory,thisfile)
        ## check if file exists
        if ((caching %in% c("refresh")) || (! file.exists(thisfile))) {
            ## file does not exist, or we want to refresh it, so go ahead and get it and save to thisfile
            if (verbose) { cat(sprintf("  ALA4R: caching %s POST to file %s\n",url,thisfile)) }
            f = CFILE(thisfile, mode="w")
            h=basicHeaderGatherer()
            curlPerform(url=url,postfields=body,post=1L,writedata=f@ref,useragent=ala_config()$user_agent,verbose=verbose,headerfunction=h$update,httpheader=c("Content-Type" = "application/json"),...)
            close(f)
            ## check http status here
            ## if unsuccessful, delete the file from the cache first, after checking if there's any useful info in the file body
            diag_message=""
            if ((substr(h$value()[["status"]],1,1)=="5") || (substr(h$value()[["status"]],1,1)=="4")) {
                if (as.numeric(h$value()["Content-Length"])<10000) {
                    ## if the file body is not too big, check to see if there's any useful diagnostic info in it
                    temp=readLines(thisfile)
                    try(diag_message <- jsonlite::fromJSON(temp)$message, silent=TRUE)
                    if (is.null(diag_message)) { diag_message="" }
                }
             #   unlink(thisfile)
            }            
            check_status_code(h$value()[["status"]])
        } else {
            if (verbose) { cat(sprintf("  ALA4R: using cached file %s for POST to %s\n",thisfile,url)) }
        }            
        ## now return whatever is appropriate according to type
        if (type %in% c("json","text")) {
            if (!(file.info(thisfile)$size>0)) {
                NULL
            } else {
                fid=file(thisfile, "rt")
                out=readLines(fid,warn=FALSE)
                close(fid)
                if (identical(type,"json")) {
                    jsonlite::fromJSON(out)
                } else {
                    out
                }
            }
        } else if (identical(type,"filename")) {
            thisfile
        } else {
            ## should not be here! did we add an allowed type to the arguments without adding handler code down here?
            stop(sprintf("unrecognized type %s in cached_post",type))
        }
    }
}


