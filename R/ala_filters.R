#' Build a dataframe from a filter list
#'
#' @param filters string: a list to narrow down the search, in the form
#' `list(field = value)`.
#' @param data_quality_profile string: a data quality profile to apply to the
#' records. See `ala_data_profiles()` for valid profiles. By default, no
#' profile is applied.
#' @return dataframe of filter values
#' @export ala_filters

ala_filters <- function(filters, data_quality_profile = NULL) {
  if (!is.null(data_quality_profile)) {
    dq_filters <- ala_quality_filters(data_quality_profile)
  }
  filter_rows <- data.table::rbindlist(lapply(names(filters), function(x) {
    row <- data.frame(name = x, include = !is(filters[[x]], "exclude"))
    row$value <- list(filter_value(filters[[x]]))
    row
  }))
  filter_rows
}

# takes a dataframe and returns a built filter query
build_filter_query <- function(filters) {
  if (is.null(filters)) {
    return(NULL)
  }
  paste(mapply(query_term, filters$name, filters$value, filters$include,
               USE.NAMES = FALSE), collapse = " AND ")
}

query_term <- function(name, value, include) {
  # add quotes around value
  value <- lapply(value, function(x) {
    paste0("\"", x, "\"")
  })
  if (include) {
    value_str <- paste(name, value, collapse = " OR ", sep = ":")
  } else {
    value_str <- paste(paste0("-", name), value, collapse = ' AND ', sep = ':')
  }
  paste0("(", value_str, ")")
}


filter_value <- function(val) {
  # replace loigcal values with strings
  if (is.logical(val)) {
    ifelse(val, "true", "false")
  }
  
  as.vector(val)
}

# negate a filter 
exclude <- function(value) {
  class(value) <- "exclude"
  value
}