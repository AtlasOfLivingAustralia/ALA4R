# Check HTTP status code
# 
# Generic check function that checks HTTP status codes coming back from ALA requests.
# 
# @param x string: a status code, or an object of class "response" (from e.g. httr's GET)
# @param on_redirect function: optional function to evaluate in the case of a redirect (3xx) code. By default a warning is issued.
# @param on_client_error function: optional function to evaluate in the case of a client error (4xx) code. By default an error is thrown.
# @param on_server_error function: optional function to evaluate in the case of a server error (5xx) code. By default an error is thrown.
# @param extra_info string: additional diagnostic info that will be shown to the user for 4xx or 5xx codes, where x is not a full response object
# @return integer: simplified status code (0=success (2xx codes), 1=warning (3xx codes))
# @references \url{http://www.w3.org/Protocols/HTTP/HTRESP.html}
# @examples
# \dontrun{
# require(httr)
# out <- GET(url="http://www.ala.org.au/")
# check_status_code(out) ## pass the whole response object
# check_status_code(out$headers$status) ## or pass the status code explicitly
# }

check_status_code <- function(x,on_redirect=NULL,on_client_error=NULL,on_server_error=NULL,extra_info="") {
    assert_that(is.string(extra_info))
    was_full_response <- FALSE
    if (inherits(x,"response")) {
        ## if this is a response object, extract the status code
        ## we may also be able to get meaningful diagnostic info out of the message body in some cases
        was_full_response <- TRUE
        xstatus <- x$headers$status
        if (is.null(xstatus)) {
            ## newer httr has changed, try this
            xstatus <- as.character(x$status_code) 
            was_full_response <- FALSE
        }
        ## check again
        if (is.null(xstatus)) {
                warning("error in http status checking: skipped. ",getOption("ALA4R_server_config")$notify)
                was_full_response <- FALSE
                xstatus <- "200" ## default to OK
        }
    } else {
        ## expect either string (e.g. "500") or integer
        if (!see_if(is.string(x))) {
            if (!see_if(is.count(x))) {
                stop("expecting either http response object, or status code as string or numeric")
            } else {
                ## is integer - convert to string
                x <- as.character(x)
            }
        }
        xstatus <- x
    }
    switch (substr(xstatus,1,1),
            "2"={ ## 2xx are all success codes
                return(0) },
            "3"={ ## 3xx are redirection codes
                ## probably indicates that we are using the wrong URL for a service
                ## the code may or may not work depending on whether e.g. a redirect was followed
                if (!is.null(on_redirect)) {
                    assert_that(is.function(on_redirect))
                    return(on_redirect(xstatus))
                } else {
                    ## just issue a warning for now
                    warning("HTTP status code ",xstatus," received.\nThis may be OK: if there are problems, please notify the package maintainers.")
                    return(1)
                }
            },
            "4"={ ## 4xx are client errors, e.g. 404 not found
                ## an error here probably indicates a problem in the ALA4R code
                ## ? should this be an error or a warning?
                if (!is.null(on_client_error)) {
                    assert_that(is.function(on_client_error))
                    return(on_client_error(xstatus))
                } else {
                    diag_msg <- paste0("  Either there was an error with your request or in the ",getOption("ALA4R_server_config")$brand," package, or the servers are down. ",getOption("ALA4R_server_config")$notify)
                    if (was_full_response) {
                        x <- jsonlite::fromJSON(content(x,type="text"))
                        if (!is.null(x$message)) {
                            diag_msg <- paste(diag_msg,"\nThe error message was:",x$message,sep=" ")
                        }
                    } else {
                        if (nchar(extra_info)>0) {
                            diag_msg <- paste(diag_msg,"\n  Some additional diagnostic information that might help:",extra_info,sep=" ")
                        }
                    }
                    stop("HTTP status code ",xstatus," received.\n",diag_msg)
                }
            },
            "5"={ ## 5xx are server errors
                if (!is.null(on_server_error)) {
                    assert_that(is.function(on_server_error))
                    return(on_server_error(xstatus))
                } else {
                    diag_msg <- paste0("  Either there was an error with the request, or the servers may be down (try again later). ",getOption("ALA4R_server_config")$notify)
                    if (was_full_response) {
                        x <- jsonlite::fromJSON(content(x,type="text"))
                        if (!is.null(x$message)) {
                            diag_msg <- paste(diag_msg,"\nThe error message was:",x$message,sep=" ")
                        }
                    } else {
                        if (nchar(extra_info)>0) {
                            diag_msg <- paste(diag_msg,"\n  Some additional diagnostic information that might help:",extra_info,sep=" ")
                        }
                    }                        
                    stop("HTTP status code ",xstatus," received.\n",diag_msg)
                }
            }
        )
    warning("Unexpected HTTP status code ",x," received.\n  ",getOption("ALA4R_server_config")$notify)
}
