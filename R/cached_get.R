#' HTTP GET with caching
#' 
#' Convenience wrapper for web GET operations. Caching, setting the user-agent string, and basic checking of the result are handled.
#' 
#' @param url string: the url of the page to retrieve
#' @param type string: the expected content type. Either "text" (default), "json", or "filename"
#' @return for type=="text" the content is returned as text. For type=="json", the content is parsed using fromJSON. For "filename", the name of the stored file is returned.
#' @details Depending on the value of ala_config()$caching, the page is either retrieved from the cache or from the url, and stored in the cache if appropriate. The user-agent string is set according to ala_config()$user_agent. The returned response (if not from cached file) is also passed to check_http_status().
#' @author Ben Raymond \email{ben@@theraymonds.org}, Jeremy VanDerWal \email{jjvanderwal@@gmail.com}
#' @examples
#'
#' out = cached_get(url="http://biocache.ala.org.au/ws/index/fields",type="json")
#' 

cached_get=function(url,type="text",...) {
    type=tolower(type)
    match.arg(type,c("text","json","filename"))
    
    if (identical(ala_config()$caching,"off") && !identical(type,"filename")) {
        ## if we are not caching, get this directly without saving to file at all
        x=GET(url=url,user_agent(ala_config()$user_agent))
        check_http_status(x)
        if (identical(type,"json")) {
            x=content(x,as="parsed")
        } else {
            x=content(x,as="text")
        }
        x
    } else {
        ## use caching
        thisfile=download_to_file(url)
        switch(type,
               "json"={
                   fromJSON(file=thisfile)
               },
               "filename"={
                   thisfile
               },
               "text"={
                   fid=file(thisfile, "rt")
                   out=readLines(fid,warn=FALSE)
                   close(fid)
                   out
               }
           )
        }
    }

