# Check HTTP status code
# 
# Generic check function that checks HTTP status codes coming back from ALA requests.
# 
# @param x string: a status code, or an object of class "response" (from e.g. GET)
# @param on_redirect function: optional function to evaluate in the case of a redirect (3xx) code. By default a warning is issued.
# @param on_client_error function: optional function to evaluate in the case of a client error (4xx) code. By default an error is thrown.
# @param on_server_error function: optional function to evaluate in the case of a server error (5xx) code. By default an error is thrown.
# @return integer: simplified status code (0=success (2xx codes), 1=warning (3xx codes))
# @references \url{http://www.w3.org/Protocols/HTTP/HTRESP.html}
# @author Atlas of Living Australia \email{support@@ala.org.au}
# @examples
#
# out = GET(url="http://www.ala.org.au/")
# check_status_code(out)
# check_status_code(out$headers$status)
# 

check_status_code=function(x,on_redirect=NULL,on_client_error=NULL,on_server_error=NULL) {
    if (identical(class(x),"response")) {
        ## if this is a response object, extract the status code
        x=x$headers$status
    }
    switch (substr(x,1,1),
            "2"={ ## 2xx are all success codes
                return(0) },
            "3"={ ## 3xx are redirection codes
                ## probably indicates that we are using the wrong URL for a service
                ## the code may or may not work depending on whether e.g. a redirect was followed
                if (! is.null(on_redirect)) {
                    return(on_redirect(x))
                } else {
                    ## just issue a warning for now
                    warning("ALA4R: HTTP status code ",x," received. If there are problems, please notify the package maintainers.")
                    return(1)
                }
            },
            "4"={ ## 4xx are client errors, e.g. 404 not found
                ## an error here probably indicates a problem in the ALA4R code
                ## ? should this be an error or a warning?
                if (! is.null(on_client_error)) {
                    return(on_client_error(x))
                } else {
                    stop("ALA4R: HTTP status code ",x," received. This may indicate an error in the ALA4R package.")
                }
            },
            "5"={ ## 5xx are server errors
                if (! is.null(on_server_error)) {
                    return(on_server_error(x))
                } else {
                    stop("ALA4R: HTTP status code ",x," received. The ALA service may be down, try again later.")
                }
            }
        )
    warning("ALA4R: unexpected HTTP status code ",x," received. If there are problems, please notify the package maintainers.")
}
