# HTTP GET with caching
# 
# Convenience wrapper for web GET operations. Caching, setting the user-agent string, and basic checking of the result are handled.
# 
# @param url string: the url of the page to retrieve
# @param type string: the expected content type. Either "text" (default), "json", or "filename" (this caches the content directly to a file and returns the filename without attempting to read it in)
# @param caching string: caching behaviour, by default from ala_config()$caching
# @param on_redirect, on_server_error, on_client_error function: passed to check_status_code()
# @return for type=="text" the content is returned as text. For type=="json", the content is parsed using jsonlite::fromJSON. For "filename", the name of the stored file is returned.
# @details Depending on the value of caching, the page is either retrieved from the cache or from the url, and stored in the cache if appropriate. The user-agent string is set according to ala_config()$user_agent. The returned response (if not from cached file) is also passed to check_status_code().
# @author Atlas of Living Australia \email{support@@ala.org.au}
# @references \url{http://api.ala.org.au/}
# @examples
#
# out = cached_get(url="http://biocache.ala.org.au/ws/index/fields",type="json")
# 

cached_get=function(url,type="text",caching=ala_config()$caching,verbose=ala_config()$verbose,on_redirect=NULL,on_client_error=NULL,on_server_error=NULL) {
    assert_that(is.string(url))
    assert_that(is.string(type))
    type=match.arg(tolower(type),c("text","json","filename"))
    assert_that(is.string(caching))
    caching=match.arg(tolower(caching),c("on","off","refresh"))
    assert_that(is.flag(verbose))
    
    if (identical(caching,"off") && !identical(type,"filename")) {
        ## if we are not caching, get this directly without saving to file at all
        if (verbose) { cat(sprintf("  ALA4R: GETting URL %s",url)) }
        x=GET(url=url,user_agent(ala_config()$user_agent))
        check_status_code(x,on_redirect=on_redirect,on_client_error=on_client_error,on_server_error=on_server_error)
        x=content(x,as="text")
        if (identical(type,"json")) {
            x=jsonlite::fromJSON(x) ## do text-then-conversion, rather than content(as="parsed") to avoid httr's default use of RJSONIO::fromJSON
        }
        x
    } else {
        ## use caching
        thisfile=download_to_file(url,on_redirect=on_redirect,on_client_error=on_client_error,on_server_error=on_server_error)
        if (!file.exists(thisfile)) {
            ## file does not exist
            NULL
        } else {
            if (type %in% c("json","text")) {
                ## for json, read directly as string first, then pass this to fromJSON. This is compatible with either RJSON's or rjsonlite's version of fromJSON (only RJSON's version can take a filename directly)
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
}
