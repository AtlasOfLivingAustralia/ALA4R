#' Get or set ALA4R configuration options
#' 
#' Get or set configuration options that control ALA4R behaviour
#' 
#' Invoking ala_config() with no arguments returns a list with the current
#' values of the options. Invoking ala_config("reset") will reset all options
#' to their default values.
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
#' \item base_url_spatial: the base url for spatial web services (default="http://spatial.ala.org.au/ws/")
#' \item base_url_bie: the base url for BIE web services (default="http://bie.ala.org.au/ws/")
#' \item base_url_biocache: the base url for biocache web services (default="http://biocache.ala.org.au/ws/")
#' }
#' 
#' @param \dots Options can be defined using name=value. Valid option names are
#' listed above
#' @return For ala_config(), a list of all options. When ala_config(...) is
#' called with arguments, nothing is returned but the configuration is set.
#' @author Ben Raymond \email{ben@@theraymonds.org}, Jeremy VanDerWal
#' \email{jjvanderwal@@gmail.com}
#' @examples
#' 
#' ala_config()
#' ala_config(caching="off")
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

    default_options=list(caching="on",cache_directory=tempdir(),user_agent=user_agent_string,verbose=FALSE,base_url_spatial="http://spatial.ala.org.au/ws/",base_url_bie="http://bie.ala.org.au/ws/",base_url_biocache="http://biocache.ala.org.au/ws/")
    ## caching can be "on" (results will be cached, and any cached results will be re-used), "refresh" (cached results will be refreshed and the new results stored in the cache), or "off"
    ## cache_directory is the directory to use for the cache. By default this is a temporary directory, which means that results will only be cached within an R session. The user may wish to set this to a non-temporary directory for caching across sessions

    ## define allowed options, for those that have restricted values
    allowed_options=list(caching=c("on","off","refresh"))

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
                    if (! file.exists(user_options[[i]])) {
                        ## cache directory does not exist. We could create it, but this is probably better left to the user to manage
                        stop("cache directory ",user_options[[i]]," does not exist");
                    }
                }
                if (identical(this_option_name,"user_agent")) {
                    if (! is.character(user_options[[i]])) {
                        stop("user_agent should be a string");
                    }
                }
                if (identical(this_option_name,"verbose")) {
                    if (! is.logical(user_options[[i]])) {
                        stop("verbose should be logical");
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
