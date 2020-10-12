#' Download occurrence data from the ALA
#' 
#' @param taxon_id string: single species ID or vector of species ids. Use 
#' `ala_taxa()` to get lookup species id. 
#' @param area string or sf object: restrict the search to an area. Can provide
#' sf object, or a wkt string. If the wkt string is too long, it may be simplified.
#' @param columns string: vector of columns to return in download. 
#' @param filters string: a list to narrow down the search, in the form `c(field = value)`
#' @param generate_doi logical: by default no DOI will be generated. Set to
#' true if you intend to use the data in a publication or similar
#' @param email string: the email address of the user performing the download
#' (required unless \code{record_count_only = TRUE}
#' @param email_notify logical: set to `FALSE` by default, set to true if you
#' would like an email notification for the download
#' @export ala_occurrences


ala_occurrences <- function(taxon_id, filters, area, columns = "default",
                            email = "ala4r@ala.org.au", generate_doi = FALSE, 
                            email_notify = FALSE) {
  
  biocache_url <- "https://biocache-ws.ala.org.au"
  assert_that(is.flag(generate_doi))
  assert_that(is.flag(email_notify))
  
  # is it worth validating the email with a regex? this won't 
  # be able to tell if the email is registered or not
  
  query <- list()
  
  if (missing(taxon_id) & missing(filters) & missing(area)) {
    # stop or allow users to download the whole ALA?
    stop("Need to provide one of `taxon id`, `filters` or `area`")
  }
  
  taxa_query <- NULL
  if(!missing(taxon_id)) {
    # should species id be validated?
    query$fq <- build_taxa_query(taxon_id)
  }

  # validate filters
  if (!missing(filters)) {
    validate_filters(filters)
    # there must be a better way to do this
    if (!is.null(query$fq)) {
      query$fq <- paste0("(",query$fq, ' AND ', fq = build_filter_query(filters), ")")
    }
    else {
      query$fq <- build_filter_query(filters)
    }
  }
  

  if (!missing(area)) {
    # convert area to wkt if not already
    if (is.character(area)) {
      # maybe give a more helpful error message here?
      stopifnot(wellknown::lint(area))
    } else if (identical(class(area), c("sf", "data.frame"))) {
      area <- build_wkt(area)
    } else {
      stop("Area must be either a wkt string or an sf spatial object.")
    }
    query$wkt <- area
  }
  # do a occurrence count query first? to warn the user
  
  count_url <- parse_url(biocache_url)
  count_url$path <- c("ws","occurrences","search")
  count_url$query <- c(query, pageSize = 0)

  if (nchar(build_url(count_url)) > 2000) {
    qid <- cache_params(query)
    query <- list(q = paste0("qid:",qid))
  }

  message('This query will return ', record_count(query), " records")

  # Add columns after getting record count
  if (missing(columns)) {
    message("No columns specified, default columns will be returned.")
    columns <- "default"
  }
  query$fields <- build_fields(columns)
  
  if (generate_doi) {
    query$mintDoi <- "true"
  }
  
  if (isFALSE(email_notify)) {
    query$emailNotify <- "false"
  }
  
  # Get data
  url <- parse_url(biocache_url)
  url$path <- c("ws", "occurrences", "offline", "download")
  url$query <- c(query, email = email, reasonTypeId = 10, dwcHeaders = 'true',
                 qa = "none")
  status <- fromJSON(build_url(url))
  
  this_status_url <- status$statusUrl
  status <- fromJSON(this_status_url)
  while (tolower(status$status) %in% c("inqueue", "running")) {
    status <- fromJSON(this_status_url)
    Sys.sleep(2)
  }
  
  temp <- tempfile(fileext = '.zip')
  download.file(status$downloadUrl,temp)
  data <- read.csv(unz(temp, "data.csv"), stringsAsFactors = FALSE)
  unlink(temp)
  
  return(data)
}

# check validity of fields?
# should the fields you have in the filters also be included?

build_fields <- function(cols) {
  presets <- c("default")
  default_columns <- c("latitude", "longitude","occurrence_date","taxon_name",
                       "species_guid",'species','id')
  additional_cols <- cols[!cols %in% presets]
  preset_selected <- cols[cols %in% presets]
    
  if (length(preset_selected) > 1) {
    stop("Only one preset option can be specified in `columns`")
  }
  else if (length(preset_selected) == 1) {
    preset_cols <- switch(preset_selected,
           "default" = default_columns)
    fields <- c(preset_cols, additional_cols)
  }
  else {
    fields <- cols
  }
  paste0(fields, collapse = ",")
}

# this is ugly but does work...
build_filter_query <- function(filters) {
  # add quotes so query behaves as expected
  quoted_filters <- lapply(filters, function(x) {
    paste0("\"", x, "\"")
    })
  paste0("(",paste(names(filters), quoted_filters, sep = ":", collapse = " AND "),")")
}

build_taxa_query <- function(ids) {
  paste0('(lsid:',paste(ids, collapse = ' OR lsid:'),')')
}


# get just the record count for a query
record_count <- function(query) {
  url <- parse_url(biocache_url)
  url$path <- c("ws","occurrences","search")
  url$query <- c(query, pageSize = 0)
  resp <- fromJSON(build_url(url))
  resp$totalRecords
}

# build a valid wkt string from a spatial polygon
build_wkt <- function(polygon) {
  wkt <- st_as_text(st_geometry(polygon)[[1]])
  return(wkt)
}


# POST params to server to get around url length constraint
# POST all the filters here, or just ones that are likely to cause the maximum
# length to be exceeded? POST only if the url is too long, or by default?
# what is the maximum length? around 2000 chars?
cache_params <- function(query) {
  url <- parse_url(biocache_url)
  url$path <- c("ws", "webportal", "params")
  resp <- POST(build_url(url), body = query, encode = 'form')
  qid <- content(resp)
  return(qid)
}


# filters vs. fields terminology
# should handle miscased things?
# should try to fuzzy match?
validate_filters <- function(filters) {
  # filters are provided in a list
  # key should be a valid field name and value should be a valid category for that field
  # valid options is a combination of ala_layers and ala_fields?
  
  invalid_filters <- names(filters)[which(!names(filters) %in%
                                            ala_fields()$name)]
  if (length(invalid_filters) > 0) {
    stop("The following filters are invalid: ",
         paste(invalid_filters, collapse = ', '),
         ". Use `ala_fields()` to get a list of valid options")
  }
  
  # validate categories
  # should we do this? or let users find out for themselves?
  # we can't do it for fields where there are > 1000 options 
  # I think let them check if they get bad results
}

# what to do about returned columns? it doesn't make sense that the 
# names of the columns you request are not necessarily the same as
# the ones asked for

