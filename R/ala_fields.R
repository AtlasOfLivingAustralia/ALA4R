#' Retrieves a list of all field names that can be used with data
#' retrieval functions
#'
#' The field names are in Darwin Core format, apart from the fields
#' which have no DwC equivalent, such as record assertions.
#'
#' @references Relevant ALA web services: \itemize{
#' \item for occurrence fields: \url{https://api.ala.org.au/#ws72}
#' \item for assertions \url{https://api.ala.org.au/#ws81}
#' }
#' @seealso \code{\link{ala_layers}} to search for spatial layers
#' @param class string: class of fields to return e.g. "Assertion"
#' @return data.frame of all fields. 
#' @examples
#' \dontrun{
#'  l <- ala_fields()
#' }
#' @export ala_fields

ala_fields <- function(class = "all") {
  # Difference in behaviour from original ALA fields: 
  # don't need to return layer information- this is handled by `ala_layers`
  # assertions and other fields are treated the same- but the type for assertions is 'logical'
  # if there is a DwC term for a field, this will be returned: not the ALA name
  
  # To do: check if a user has provided params/ used `field_info` and warn if so
  
  # for backwards compatibility, should allow a user to get the ALA names for a field?
  
  
  # keep for backwards compatibility
  if (class == "general") {
    fields <- ala_GET(getOption("ALA4R_server_config")$base_url_bie,
                      path = "ws/admin/indexFields")
    return(fields[!fields$name %in% unwanted_columns("general"), ])
  }
  
  
  
  url <- getOption("ALA4R_server_config")$base_url_biocache
  
  fields <- ala_GET(url, path = "ws/index/fields")
  
  # replace name with dwc term if it exists
  fields$name <- ifelse(!is.na(fields$dwcTerm), fields$dwcTerm, fields$name)
  
  names(fields) <- rename_columns(names(fields), type = "fields")
  fields <- fields[wanted_columns("fields")]
  
  # add assertions
  assertions <- ala_GET(url, path = "ws/assertions/codes")
  assertions$data_type <- "logical"
  assertions$class <- "Assertion"
  names(assertions) <- rename_columns(names(assertions), type = "assertions")
  assertions <- assertions[wanted_columns("assertions")]
  all_fields <- rbind(fields, assertions)
  if (class == "all") {
    return(all_fields)
  }
  all_fields[tolower(all_fields$class) == tolower(class) &
               !is.na(all_fields$class),]
}


# function to keep backwards compatibility
# takes field list and converts back to ALA name
dwc_to_ala <- function(dwc_names) {
  fields <- all_fields()
  # get relevant cols
  sapply(dwc_names, function(n) {
    if (n %in% fields$dwcTerm) {
      return(fields[fields$dwcTerm == n & !is.na(fields$dwcTerm), ]$name)
    } else {
      return(n)
    }
  }, USE.NAMES = FALSE)
}

ala_to_dwc <- function() {
  
}

all_fields <- function() {
  url <- getOption("ALA4R_server_config")$base_url_biocache
  ala_GET(url, path = "ws/index/fields")
}

## Old functions to be deleted
## private function to replace any full field names (descriptions) with
## their id values
## e.g. "Radiation - lowest period (Bio22)" to id "el871"
fields_name_to_id <- function(fields, fields_type, make_names = FALSE) {
  assert_that(is.character(fields))
  assert_that(is.string(fields_type))
  ## if TRUE, apply make.names to variable names before matching
  assert_that(is.flag(make_names))
  fields_type <- match.arg(tolower(fields_type), c("occurrence", "general",
                                                   "layers", "assertions"))
  
  if (fields_type == "layers") {
    valid_fields <- ala_layers()
  } else {
    valid_fields <- all_fields()
  }
  
  ## merge differently for "layers" fields, because those column names differ
  ## from other fields_type
  ## for layers, the long name is in "desc", with the id in "id" (and "name"
  ## is something different)
  ## ** as of 17-Jun-2014, "desc" is now renamed "description"
  ## for "occurrence" and "assertions", long name is in "description" and id
  ## is in "name"
  ## for general, there is no long name (description)
  ## for each one, warn if multiple matches on long name are found
  if (make_names) {
    valid_fields$description <- switch(fields_type,
                                       "layers" =,
                                       "occurrence" =,
                                       "assertions" =
                                         make.names(valid_fields$description)
    )
  }
  switch(fields_type,
         "layers" = vapply(fields, function(z) {
           if (z %in% valid_fields$description & (!z %in% valid_fields$id)) {
             if (sum(valid_fields$description == z, na.rm = TRUE) > 1) {
               if (nchar(z) > 0) { ## don't warn if field name is degenerate ""
                 warning(" multiple ", fields_type,
                         " fields match the name \"", z, "\", using first")
               }
             }
             valid_fields$id[which(valid_fields$description == z)[1]]
           }
           else {
             z
           }
         }, FUN.VALUE = "", USE.NAMES = FALSE),
         "occurrence" =,
         "assertions" = vapply(fields, function(z) {
           if (z %in% valid_fields$description & (!z %in% valid_fields$name)) {
             if (sum(valid_fields$description == z, na.rm = TRUE) > 1) {
               if (nchar(z) > 0) {
                 warning(" multiple ", fields_type, " fields match the name \"",
                         z, "\", using first")
               }
             }
             valid_fields$name[which(valid_fields$description == z)[1]]
           }
           else {
             z
           }
         }
         , FUN.VALUE = "", USE.NAMES = FALSE),
         fields ## default to just returning the fields as supplied
  )
}

## private function to replace any id values with their full field
## names (descriptions)
fields_id_to_name <- function(fields, fields_type) {
  assert_that(is.character(fields))
  assert_that(is.string(fields_type))
  fields_type <- match.arg(tolower(fields_type),
                           c("occurrence", "general", "layers", "assertions"))
  if (fields_type == "layers") {
    valid_fields <- ala_layers()
  } else {
    valid_fields <- all_fields()
  }
  
  ## merge differently for "layers" fields, because those column names differ
  ## from other fields_type
  ## for layers, the long name is in "desc", with the id in "id" (and "name"
  ## is something different)
  ## ** as of 17-Jun-2014, "desc" is now renamed "description"
  ## for "occurrence" and "assertions", long name is in "description" and id is
  ## in "name"
  ## for general, there is no long name (description)
  ## for each one, warn if multiple matches on long name are found
  switch(fields_type,
         "layers" = vapply(fields, function(z) {
           if (z %in% valid_fields$layer_id & (!z %in% valid_fields$description)) {
             valid_fields$description[which(valid_fields$layer_id == z)[1]]
           }
           else {
             z
           }
         },
         FUN.VALUE = "", USE.NAMES = FALSE),
         "occurrence" =,
         "assertions" = vapply(fields, function(z) {
           if (z %in% valid_fields$name & (!z %in% valid_fields$description)) {
             valid_fields$name[which(valid_fields$name == z)[1]]
           }
           else {
             z
           }
         }, FUN.VALUE = "", USE.NAMES = FALSE),
         fields ## default to just returning the fields as supplied
  )
}

