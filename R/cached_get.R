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
# @references \url{https://api.ala.org.au/}
# @examples
#
# out <- cached_get(url="https://biocache-ws.ala.org.au/ws/index/fields", type="json")
#

cached_get <- function(url, type="text", caching=ala_config()$caching, verbose=ala_config()$verbose, on_redirect=NULL, on_client_error=NULL, on_server_error=NULL, encoding=ala_config()$text_encoding) {
    assert_that(is.notempty.string(url))
    assert_that(is.string(type))
    type <- match.arg(tolower(type), c("text", "json", "filename", "binary_filename"))
    assert_that(is.string(caching))
    caching <- match.arg(tolower(caching), c("on", "off", "refresh"))
    assert_that(is.flag(verbose))

    ## strip newlines or multiple spaces from url: these seem to cause unexpected behaviour
    url <- str_replace_all(url, "[\r\n ]+"," ")
    if (nchar(url)>getOption("ALA4R_server_config")$server_max_url_length) warning("URL length may be longer than is allowed by the server")

    ## Originally the code was different for caching=="off" vs caching "on" or "refresh"
    ## The former previously read data direct to memory without downloading to a file first; however,
    ## JSON parsing of text behaves differently to parsing a file
    ## So for consistency and simplicity we always download to file. When caching is "off" or "refresh" the cached
    ##  file will be re-downloaded each time.
    
    if (identical(caching, "off") && !(type %in% c("filename", "binary_filename"))) {
        if (verbose) message(sprintf("GETting URL %s", url))
        this_outfile <-  tempfile()
    } else {
        this_outfile <- ala_cache_filename(url)
    }
    ## use caching
    thisfile <- download_to_file(url, outfile=this_outfile, binary_file=identical(type, "binary_filename"), verbose=verbose, on_redirect=on_redirect, on_client_error=on_client_error, on_server_error=on_server_error)
    if (!file.exists(thisfile)) {
        ## file does not exist
        NULL
    } else {
        if (type %in% c("json", "text")) {
            if (!(file.info(thisfile)$size>0)) {
                NULL
            } else {
                ## for json, previously we read as string first, then passed this to fromJSON. This was compatible with either RJSON's or rjsonlite's version of fromJSON (only RJSON's version can take a filename directly). However, it introduced issues with readLines and handling of encoding. Now we pass the file directly to jsonlite::fromJSON
                if (type=="json") {
                    ## convert directly from file - this also allows jsonlite to handle encoding issues
                    jsonlite::fromJSON(thisfile)
                } else {
                    fid <- file(thisfile, "rt")
                    out <- readLines(fid, warn=FALSE, encoding=encoding)
                    close(fid)
                    out
                }
            }
        } else if (type %in% c("filename", "binary_filename")) {
            thisfile
        } else {
            ## should not be here! did we add an allowed type to the arguments without adding handler code down here?
            stop(sprintf("unrecognized type %s in cached_get", type))
        }
    }
}
