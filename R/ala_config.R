#' Get or set configuration options that control ALA4R behaviour
#'
#' @references \url{https://api.ala.org.au/}
#' @references \url{https://spatial.ala.org.au/layers-service/} this will
#' eventually move to the api link
#'
#' Invoking \code{ala_config()} with no arguments returns a list with the
#' current values of the options.
#'
#' \code{ala_reasons()} returns a data frame with information describing the
#' valid options for \code{download_reason_id}
#' 
#' @param preserve logical: store config for future sessions?
#'
#' @param \dots Options can be defined using name = value. Valid options are:
#' \itemize{
#'   \item reset: \code{ala_config("reset")} will reset the options to their
#'   default values
#'   \item caching string: caching can be
#'     "on" (results will be cached, and any cached results will be re-used),
#'     "refresh" (cached results will be refreshed and the new results stored
#'     in the cache), or
#'     "off" (no caching, default).
#'   \item cache_directory string: the directory to use for the cache.
#'     By default this is a temporary directory, which means that results will
#'     only be cached
#'     within an R session and cleared automatically when the user exits R.
#'     The user may wish to set this to a non-temporary directory for
#'     caching across sessions. The directory must exist on the file system.
#'   \item verbose logical: should ALA4R give verbose output to assist
#'   debugging? (default = FALSE)
#'   \item warn_on_empty logical: should a warning be issued if a request
#'   returns an empty result set? (default = FALSE)
#'   \item user_agent string: the user-agent string used with all web requests
#'   to the ALA servers.
#'     Default = "ALA4R" with version number
#'   \item text_encoding string: text encoding assumed when reading cached
#'   files from local disk (default = "UTF-8")
#'   \item download_reason_id numeric or string: the "download reason" required
#'   by some ALA services, either as a numeric ID (currently 0--11)
#'   or a string (see \code{ala_reasons()} for a list of valid ID codes and
#'   names). By default this is NA. Some ALA services require a valid
#'   download_reason_id code, either specified here or directly to the
#'   associated R function.
#' }
#'
#' @return For ala_config(), a list of all options. When ala_config(...) is
#' called with arguments, nothing is returned but the configuration is set.
#'
#' @examples
#' \dontrun{
#'  ala_config()
#'  ala_config(caching = "off")
#'  ala_reasons()
#'  ala_config(download_reason_id = 0,verbose = TRUE)
#'  ala_config("reset")
#' }
#' @export ala_config

ala_config <- function(..., preserve = FALSE) {
  ala_option_name <- "ALA4R_config"
  current_options <- getOption(ala_option_name)
  
  assert_that(is.logical(preserve))
  user_options <- list(...)
  
  default_options <- list(
    caching = FALSE,
    cache_directory = tempdir(),
    download_reason_id = 4,
    email = "",
    send_email = FALSE
  )
  
  current_options <- getOption(ala_option_name)
  if (length(user_options) == 0) {
    return(current_options)
  }
  if (is.null(current_options)) {
    ## ALA4R options have not been set yet, so set them to the defaults
    current_options <- default_options
    ## set the global option
    temp <- list(current_options)
    names(temp) <- ala_option_name
    options(temp)
  }
  
  # check all the options are valid, if so, set as options
  for (x in names(user_options)) {
    validate_option(x, user_options[[x]])
    current_options[[x]] <- user_options[[x]]
  }
  
  # for backwards compatibility
  if (!is.null(user_options$download_reason_id)) {
    user_options$download_reason_id <-
      convert_reason(user_options$download_reason_id)
  }
  
  ## set the global option
  temp <- list(current_options)
  names(temp) <- ala_option_name
  options(temp)
  
  if (preserve) {
    profile_path <- file.path(Sys.getenv("HOME"), ".Rprofile")
    message("The variables set will be stored in ", profile_path)
    #write_options 
  } else {
    msg <- "These configuration options will only be saved for this session. 
    Set `preserve = TRUE` to preserve them for future sessions."
    # message("These configuration options will only be saved for this session.",
    #        " Set `preserve = TRUE` to preserve them for future sessions.")
  }
  options(temp)
}

validate_option <- function(name, value) {
  if (name == "caching") {
    if (!is.logical(value) && !(value %in% c("on", "off", "reset"))) {
      stop("\"", name, "\"", " must be TRUE or FALSE")
    }
  } else if (name == "send_email" || name == "warn_on_empty") {
    if (!is.logical(value)) {
      stop("\"", name, "\"", " must be TRUE or FALSE")
    }
  } else if (name == "cache_directory") {
    if (!dir.exists("cache_directory")) {
      stop("Cache directory does not exist, please create it and try again.")
    }
  } else if (name == "ala_email") {
    if (!is.character(value)) {
      stop("Email must be a string")
    }
  } else if (name == "download_reason_id") {
    if (!(name %in% c(ala_reasons()$name, ala_reasons()$id))) {
      stop("Download reason must be a valid reason id or name ",
           "See `ala_reasons()` for valid reasons.")
    }
  } else {
    stop("\"", name, "\"", "is not a valid option name.")
  }
}

#' @rdname ala_config
#' @export
ala_reasons <- function() {
    ## return list of valid "reasons for use" codes
    out <- ala_GET(getOption("ALA4R_server_config")$base_url_logger,
                           path = "service/logger/reasons")
    if (any(names(out) == "deprecated")) out <- out[!out$deprecated, ]
    out <- out[wanted_columns("reasons")]
    # sort by id to make it less confusing
    row.names(out) <- out$id
    out[order(out$id),]
}

convert_reason <- function(reason) {
  ## unexported function to convert string reason to numeric id
  if (is.character(reason)) {
    valid_reasons <- ala_reasons()
    tryCatch({
      reason <- match.arg(tolower(reason), valid_reasons$name)
      reason <- valid_reasons$id[valid_reasons$name == reason]
    },
    error = function(e) {
      stop("could not match download_reason_id string \"",
           reason, "\" to valid reason string: see ",
           getOption("ALA4R_server_config")$reasons_function,
           "()")
    }
    )
  }
  reason
}