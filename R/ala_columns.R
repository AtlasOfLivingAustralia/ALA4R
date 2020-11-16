#' Build dataframe of columns to keep
#' @param group string: name of column group to include
#' @param extra string: additional column names to return
#' @export ala_columns
ala_columns <- function(group, extra) {
  if (!missing(group)) {
    group_cols <- data.table::rbindlist(lapply(group, function(x) {
      data.frame(name = preset_cols(x), type = "field",
                 stringsAsFactors = FALSE)
    }))} else {
      group_cols <- NULL
    }
  
  assertions <- ala_fields("assertion")$name
  if (!missing(extra)) {
    extra_cols <- data.table::rbindlist(lapply(extra, function(x) {
      type <- ifelse(x %in% assertions, "assertions", "field")
      data.frame(name = x, type = type, stringsAsFactors = FALSE)
    }))} else {
      extra_cols <- NULL
    }
  all_cols <- rbind(group_cols, extra_cols)
  # remove duplicates
  all_cols[!duplicated(all_cols$name), ]
}


preset_cols <- function(type) {
  valid_groups <- c("basic", "event")
  # use ALA version of taxon name to avoid ambiguity (2 fields map to dwc name)
  cols <- switch(type,
                 "basic" = c("decimalLatitude", "decimalLongitude", "eventDate",
                             "taxon_name", "taxonConceptID", "recordID", "data_resource"),
                 "event" = c("eventRemarks", "eventTime", "eventID", "eventDate",
                             "samplingEffort", "samplingProtocol"),
                 stop("\"", type, "\" is not a valid column group. Valid groups are: ",
                      paste(valid_groups, collapse = ", "))
  )
  cols
}
