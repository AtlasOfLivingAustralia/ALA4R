#' Get or set configuration options that control ALA4R behaviour
#'
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/}
#' 
#' Invoking ala_config() with no arguments returns a list with the current values of the options. 
#' Invoking ala_config("reset") will reset all options to their default values.
#' 
#' Valid options are: \itemize{
#' \item caching: caching can be "on" (results will
#' be cached, and any cached results will be re-used), "refresh" (cached
#' results will be refreshed and the new results stored in the cache), or "off"
#' (no caching, default).
#' \item cache_directory: the directory to use for the
#' cache. By default this is a temporary directory, which means that results
#' will only be cached within an R session. The user may wish to set this to a
#' non-temporary directory for caching across sessions. The directory must
#' exist on the file system.
#' \item verbose: should ALA4R give verbose output to
#' assist debugging?  (logical, default=FALSE)
#' \item user_agent: the user-agent
#' string used with all web requests to the ALA servers.
#' \item download_reason_id: the ID code of the "download reason" required by some ALA services. By default this is NA. Some ALA services require a valid download_reason_id code, either specified here or directly to the associated R function. See ala_reasons() for a list of valid ID codes.
#' \item base_url_spatial: the base url for spatial web services (default="http://spatial.ala.org.au/ws/")
#' \item base_url_bie: the base url for BIE web services (default="http://bie.ala.org.au/ws/")
#' \item base_url_biocache: the base url for biocache web services (default="http://biocache.ala.org.au/ws/")
#' }
#'
#' ala_reasons() returns a data frame with information describing the valid options for "download_reason"
#' 
#' @param \dots Options can be defined using name=value. Valid option names are
#' listed above
#' @return For ala_config(), a list of all options. When ala_config(...) is
#' called with arguments, nothing is returned but the configuration is set.
#' 
#' @examples
#' 
#' ala_config()
#' ala_config(caching="off")
#' ala_reasons()
#' ala_config(download_reason_id="9")
#' 
#' @export ala_config
ala_config=function(...) {
    ## get or set options that control ALA4R behaviour
    ## options are stored as a global option with the name defined in ala_option_name
    ala_option_name="ALA4R_config"
    user_options=list(...) ## the options passed by the user

    ## default user-agent string
    version_string="version unknown"
    suppressWarnings(
        try( version_string<-utils::packageDescription('ALA4R')[["Version"]],silent=TRUE) ## get the ALA4R version, if we can
        )
    user_agent_string=paste("ALA4R ",version_string," (",R.Version()$version.string,"/",R.Version()$platform,")",sep="")

    default_options=list(caching="on",cache_directory=tempdir(),user_agent=user_agent_string,download_reason_id=NA,verbose=FALSE,base_url_spatial="http://spatial.ala.org.au/ws/",base_url_bie="http://bie.ala.org.au/ws/",base_url_biocache="http://biocache.ala.org.au/ws/",base_url_alaspatial="http://spatial.ala.org.au/alaspatial/ws/")
    ## caching can be "on" (results will be cached, and any cached results will be re-used), "refresh" (cached results will be refreshed and the new results stored in the cache), or "off"
    ## cache_directory is the directory to use for the cache. By default this is a temporary directory, which means that results will only be cached within an R session. The user may wish to set this to a non-temporary directory for caching across sessions

    ## define allowed options, for those that have restricted values
    allowed_options=list(caching=c("on","off","refresh"),download_reason_id=c(1:10)) ## ideally, the valid download_reason_id values should be populated dynamically from the ala_reasons() function. However if that is called (from here) before the AL4R_config option has been set, then we get infinite recursion. To be addressed later ...

    ## has the user asked to reset options to defaults?
    if (identical(user_options,list("reset"))) {
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
                    if (!see_if(is.string(user_options[[i]]))) {
                        stop("cache_directory should be a string")
                    }
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
    
    ## Values at 6-Feb-2014:
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
    out=cached_get("http://logger.ala.org.au/service/logger/reasons",type="json")
    ldply(out,as.data.frame)
}
