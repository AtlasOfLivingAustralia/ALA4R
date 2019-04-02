# HTTP POST with caching
#
# Convenience wrapper for web POST operations. Caching, setting the user-agent string, and basic checking of the result are handled.
#
# @param url string: the url of the page to retrieve
# @param body string: the body to POST
# @param type string: the expected content type. Either "text" (default), "json", or "filename" (this caches the content directly to a file and returns the filename without attempting to read it in)
# @param caching string: caching behaviour, by default from ala_config()$caching
# @param content_type string: set the Content-Type header to a specific value (needed for e.g. search_names), default is unset
# @param ... additional arguments passed to curlPerform
# @return for type=="text" the content is returned as text. For type=="json", the content is parsed using jsonlite::fromJSON. For "filename", the name of the stored file is returned.
# @details Depending on the value of caching, the page is either retrieved from the cache or from the url, and stored in the cache if appropriate. The user-agent string is set according to ala_config()$user_agent. The returned response (if not from cached file) is also passed to check_status_code().
# @references \url{https://api.ala.org.au/}
# @examples
#
# out <- cached_post(url="https://bie.ala.org.au/ws/species/lookup/bulk", body=jsonlite::toJSON(list(names=c("Heleioporus australiacus", "Grevillea"))), type="json")


#orig: cached_post <- function(url, body, type="text", caching=ala_config()$caching, verbose=ala_config()$verbose, ...) {
cached_post <- function(url, body, type="text", caching=ala_config()$caching, verbose=ala_config()$verbose, content_type, encoding=ala_config()$text_encoding, ...) {
    assert_that(is.notempty.string(url))
    assert_that(is.string(body))
    assert_that(is.string(type))
    type <- match.arg(tolower(type), c("text", "json", "filename", "binary_filename"))
    assert_that(is.string(caching))
    caching <- match.arg(tolower(caching), c("on", "off", "refresh"))
    assert_that(is.flag(verbose))

    ## strip newlines or multiple spaces from url: these seem to cause unexpected behaviour
    url <- str_replace_all(url, "[\r\n ]+", " ")
    if (nchar(url)>getOption("ALA4R_server_config")$server_max_url_length) warning("URL length may be longer than is allowed by the server")

    ## Originally the code was different for caching=="off" vs caching "on" or "refresh"
    ## The former previously read data direct to memory without downloading to a file first; however,
    ## JSON parsing of text behaves differently to parsing a file
    ## So for consistency and simplicity we always download to file. When caching is "off" or "refresh" the cached
    ##  file will be re-downloaded each time.
    
    ## use caching
    thisfile <- digest(paste(url, body)) ## use md5 hash of url plus body as cache filename
    thisfile <- file.path(ala_config()$cache_directory, thisfile)
    ## check if file exists
    if ((caching %in% c("off", "refresh")) || (! file.exists(thisfile))) {
        ## file does not exist, or we want to refresh it, so go ahead and get it and save to thisfile
        if (verbose) message(sprintf("Caching %s POST to file %s", url, thisfile))
        if (missing(content_type)) { content_type = type }
        if (verbose) {
          post <- POST(url, httr::content_type(content_type), body=body, write_disk(thisfile, overwrite=TRUE),user_agent(ala_config()$user_agent),config(followlocation = 0L),verbose()) } 
        else { 
          post <- POST(url, httr::content_type(content_type), body=body, write_disk(thisfile, overwrite=TRUE),user_agent(ala_config()$user_agent),config(followlocation = 0L)) }
        status_code <- status_code(post)
        ## check http status here
        ## if unsuccessful, delete the file from the cache first, after checking if there's any useful info in the file body
        diag_message <- ""
        if ((substr(status_code, 1, 1)=="5") || (substr(status_code, 1, 1)=="4")) {
          headers <- headers(post)
          if (exists("content-length",where=headers) && (as.numeric(headers["content-length"][1])<10000)) {
            ## if the file body is not too big, check to see if there's any useful diagnostic info in it
            suppressWarnings(temp <- readLines(thisfile))
            try(diag_message <- jsonlite::fromJSON(temp)$message, silent=TRUE)
            if (is.null(diag_message)) diag_message <- ""
          }
          unlink(thisfile)
        }
        check_status_code(status_code, extra_info=diag_message)
    } else {
        if (verbose) message(sprintf("Using cached file %s for POST to %s", thisfile, url))
    }
    ## now return whatever is appropriate according to type
    if (type %in% c("json", "text")) {
        if (!(file.info(thisfile)$size>0)) {
            NULL
        } else {
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
        stop(sprintf("unrecognized type %s in cached_post", type))
    }
}
