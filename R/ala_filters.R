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
  assertions <- ala_fields("assertion")$name
  filter_rows <- data.table::rbindlist(lapply(names(filters), function(x) {
    if (x %in% assertions) {
      row <- data.frame(name = "assertion", include = TRUE, value = x,
                        stringsAsFactors = FALSE)
    } else {
      row <- data.frame(name = x, include = !inherits(filters[[x]], "exclude"),
                        value = I(list(filter_value(filters[[x]]))),
                        stringsAsFactors = FALSE)
    }
    row
  }))
  filter_rows
}


# filters vs. fields terminology
# should handle miscased things?
# should try to fuzzy match?
# should also validate facets?
validate_filters <- function(filters) {
  # filters are provided in a dataframe
  # key should be a valid field name and value should be a valid category for that field
  # valid options is a combination of ala_layers and ala_fields?
  
  invalid_filters <- filters$name[!filters$name %in% c(ala_fields()$name,
                                                       "assertion", all_fields()$name)]
  
  if (length(invalid_filters) > 0) {
    stop("The following filters are invalid: ",
         paste(invalid_filters, collapse = ", "),
         ". Use `ala_fields()` to get a list of valid options")
  }
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
  val
}

# negate a filter 
exclude <- function(value) {
  class(value) <- "exclude"
  value
}