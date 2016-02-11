#' Get or set configuration options that control ALA4R behaviour
#'
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/}
#' @references \url{http://spatial.ala.org.au/layers-service/} this will eventually move to the api link
#'
#' Invoking \code{ala_config()} with no arguments returns a list with the current values of the options.
#'
#' \code{ala_reasons()} returns a data frame with information describing the valid options for \code{download_reason_id}
#'
#' @param \dots Options can be defined using name=value. Valid options are:
#' \itemize{
#'   \item reset: \code{ala_config("reset")} will reset the options to their default values
#'   \item caching string: caching can be
#'     "on" (results will be cached, and any cached results will be re-used),
#'     "refresh" (cached results will be refreshed and the new results stored in the cache), or
#'     "off" (no caching, default).
#'   \item cache_directory string: the directory to use for the cache.
#'     By default this is a temporary directory, which means that results will only be cached
#'     within an R session and cleared automatically when the user exits R. The user may wish to set this to a non-temporary directory for
#'     caching across sessions. The directory must exist on the file system.
#'   \item verbose logical: should ALA4R give verbose output to assist debugging?  (default=FALSE)
#'   \item warn_on_empty logical: should a warning be issued if a request returns an empty result set? (default=FALSE)
#'   \item user_agent string: the user-agent string used with all web requests to the ALA servers.
#'     Default = "ALA4R" with version number, R version and date and user platform
#'   \item text_encoding string: text encoding assumed when reading cached files from local disk (default="UTF-8")
#'   \item download_reason_id numeric or string: the "download reason" required by some ALA services, either as a numeric ID (currently 0--11)
#'   or a string (see \code{ala_reasons()} for a list of valid ID codes and names). By default this is NA. Some ALA services require a valid
#'   download_reason_id code, either specified here or directly to the associated R function.
#'   \item base_url_spatial string: the base url for spatial web services (default="http://spatial.ala.org.au/ws/")
#'   \item base_url_bie string: the base url for BIE web services (default="http://bie.ala.org.au/ws/")
#'   \item base_url_biocache string: the base url for biocache web services (default="http://biocache.ala.org.au/ws/")
#'   \item base_url_alaspatial string: the base url for older ALA spatial services (default="http://spatial.ala.org.au/alaspatial/ws/")
#'   \item base_url_images string: the base url for the images database (default="http://images.ala.org.au/"). Set to NULL or empty string if not available
#'   \item base_url_logger string: the base url for usage logging webservices (default="http://logger.ala.org.au/service/logger/")
#' }
#'
#' @return For ala_config(), a list of all options. When ala_config(...) is
#' called with arguments, nothing is returned but the configuration is set.
#'
#' @examples
#'
#' ala_config()
#' ala_config(caching="off")
#' ala_reasons()
#' ala_config(download_reason_id=9,verbose=TRUE)
#' ala_config("reset")
#'
#' @export ala_config
#'
ala_config=function(...) {
    ## get or set options that control ALA4R behaviour
    ## options are stored as a global option with the name defined in ala_option_name
    ala_option_name="ALA4R_config"
    user_options=list(...) ## the options passed by the user

    ## default user-agent string
    version_string="version unknown"
    suppressWarnings(try(version_string<-utils::packageDescription('ALA4R')[["Version"]],silent=TRUE)) ## get the ALA4R version, if we can
    user_agent_string=paste("ALA4R ",version_string," (",R.Version()$version.string,"/",R.Version()$platform,")",sep="")

    ## set default options
    default_options=list(
        caching="on",
        cache_directory=tempdir(),
        user_agent=user_agent_string,
        download_reason_id=NA,
        verbose=FALSE,
        warn_on_empty=FALSE,
        text_encoding="UTF-8",
        base_url_spatial="http://spatial.ala.org.au/ws/",
        base_url_bie="http://bie.ala.org.au/ws/",
        base_url_biocache="http://biocache.ala.org.au/ws/",
        base_url_alaspatial="http://spatial.ala.org.au/alaspatial/ws/",
        base_url_images="http://images.ala.org.au/",
        base_url_logger="http://logger.ala.org.au/service/logger/"
    )

    ## define allowed options, for those that have restricted values
    allowed_options=list(caching=c("on","off","refresh"),download_reason_id=c(1:10))
    ## ideally, the valid download_reason_id values should be populated dynamically from the ala_reasons() function. However if that is called (from here) before the AL4R_config option has been set, then we get infinite recursion. To be addressed later ...

    ## has the user asked to reset options to defaults?
    if (identical(tolower(user_options),"reset")) {
        temp=list(default_options)
        names(temp)=ala_option_name
        options(temp)
    } else {
        names(user_options)=tolower(names(user_options))
        ## has the user specified something we don't recognize?
        known_options=names(default_options)
        unknown=setdiff(names(user_options),known_options)
        if (length(unknown)>0) {
            stop("unknown ALA4R options: ", str_c(unknown,collapse=", "))
        }

        current_options=getOption(ala_option_name)
        if (is.null(current_options)) {
            ## ALA4R options have not been set yet, so set them to the defaults
            current_options=default_options;
            ## set the global option
            temp=list(current_options)
            names(temp)=ala_option_name
            options(temp)
        }

        ## convert reason from char to numeric if needed
        if (!is.null(user_options$download_reason_id)) {
            user_options$download_reason_id=convert_reason(user_options$download_reason_id)
        }

        ## override any defaults with user-specified options
        if (length(user_options)>0) {
            for (i in 1:length(user_options)) {
                this_option_name=names(user_options)[i]
                if (! is.null(allowed_options[[this_option_name]])) {
                    ## there are restrictions on the allowed values for this option
                    ## could use match.arg here but the output is a bit obscure
                    if (! (user_options[[i]] %in% allowed_options[[this_option_name]])) {
                        stop("value \"",user_options[[i]],"\" is not a valid choice for ",this_option_name," (should be one of ", str_c(allowed_options[[this_option_name]],collapse=", "),")")
                    }
                }
                ## any other specific checks ...
                if (identical(this_option_name,"cache_directory")) {
                    if (!see_if(is.notempty.string(user_options[[i]]))) {
                        stop("cache_directory should be a string")
                    }
                    ## strip trailing file separator, if there is one
                    user_options[[i]]=sub("[/\\]+$","",user_options[[i]])
                    if (! (file.exists(user_options[[i]]) && file.info(user_options[[i]])$isdir)) {
                        ## cache directory does not exist. We could create it, but this is probably better left to the user to manage
                        stop("cache directory ",user_options[[i]]," does not exist");
                    }
                }
                if (identical(this_option_name,"user_agent")) {
                    if (!see_if(is.string(user_options[[i]]))) {
                        stop("user_agent should be a string")
                    }
                }
                if (identical(this_option_name,"verbose")) {
                    if (!see_if(is.flag(user_options[[i]]))) {
                        stop("verbose should be TRUE or FALSE")
                    }
                }
                if (identical(this_option_name,"warn_on_empty")) {
                    if (!see_if(is.flag(user_options[[i]]))) {
                        stop("warn_on_empty should be TRUE or FALSE")
                    }
                }

                current_options[this_option_name]=user_options[[i]]
            }
            ## set the global option
            temp=list(current_options)
            names(temp)=ala_option_name
            options(temp)
        } else {
            ## no user options were provided, so user is asking for current options to be returned
            current_options
        }
    }
}


#' @rdname ala_config
#' @export
ala_reasons=function() {
    ## return list of valid "reasons for use" codes

    ## Values at 8-Nov-2015:
    ##[{"id":0,"name":"conservation management/planning","rkey":"logger.download.reason.conservation"},
    ##{"id":1,"name":"biosecurity management, planning","rkey":"logger.download.reason.biosecurity"},
    ##{"id":2,"name":"environmental impact, site assessment","rkey":"logger.download.reason.environmental"},
    ##{"id":3,"name":"education","rkey":"logger.download.reason.education"},
    ##{"id":4,"name":"scientific research","rkey":"logger.download.reason.research"},
    ##{"id":5,"name":"collection management","rkey":"logger.download.reason.collection.mgmt"},
    ##{"id":6,"name":"other","rkey":"logger.download.reason.other"},
    ##{"id":7,"name":"ecological research","rkey":"logger.download.reason.ecological.research"},
    ##{"id":8,"name":"systematic research","rkey":"logger.download.reason.systematic.research"},
    ##{"id":9,"name":"other scientific research","rkey":"logger.download.reason.other.scientific.research"},
    ##{"id":10,"name":"testing","rkey":"logger.download.reason.testing"}]
    cached_get(build_url_from_parts(ala_config()$base_url_logger,path="reasons"),type="json")
}


convert_reason=function(reason) {
    ## unexported function to convert string reason to numeric id
    if (is.character(reason)) {
        valid_reasons=ala_reasons()
        tryCatch({ reason<-match.arg(tolower(reason),valid_reasons$name)
                   reason<-valid_reasons$id[valid_reasons$name==reason]
               },
                 error=function(e){ stop("could not match download_reason_id string \"",reason,"\" to valid reason string: see ala_reasons()") }
                 )
    }
    reason
}
