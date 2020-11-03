## some utility functions used internally within the ALA4R library: not
## exported

##-----------------------------------------------------------------------------

empty <- function(x) is.null(x) || nrow(x) < 1 || ncol(x) < 1

is.notempty.string <- function(x) {
    is.string(x) && !is.na(x) && nchar(x) > 0
}

##-----------------------------------------------------------------------------

## internal function for converting chr data types to numeric or logical
convert_dt <- function(x, test_numeric = TRUE) {
    ## set test_numeric to FALSE to skip checking for numeric columns - might
    ## be a little faster if not needed
    assert_that(is.flag(test_numeric))
    if (see_if(is.character(x))) {
        ux <- unique(x)
        ## non-valid encoding of strings here will cause failure
        encoding_ok <- function(s) {
            ## will be TRUE if successful, or an error message if not
            temp <- try({
                nchar(s); TRUE
                }, silent = TRUE)
            is.logical(temp) && temp
        }
        if (!encoding_ok(ux)) {
            x <- enc2utf8(x) ## force to utf8
            ux <- unique(x)
        }
        if (all(nchar(ux) < 1)) {
            ## all empty strings - leave as is
        } else if (all(ux %in% c("true", "false", "TRUE", "FALSE", "", "NA"))) {
            x <- as.logical(x)
        } else if (test_numeric) {
            if (all(nchar(ux) < 1 | ux == "NA" |
                    !is.na(suppressWarnings(as.numeric(ux))))) {
                x <- as.numeric(x)
            }
        }
    }
    x
}

##-----------------------------------------------------------------------------

clean_string <- function(x) {
    ## only used in search_names and search_partial_name

    ## characters causes problems with hyphenated names and seems likely not to
    ## behave well with internationalisation anyway
    x <- str_trim(x) ## remove leading and trailing whitespaces
    gsub("\\s+", " ", x) ## replace multiple whitespaces with single
}

##-----------------------------------------------------------------------------

##convert to camel case ... modified from help forum example
## not exported for users: internal ALA4R use only
tocamel <- function(x, delim = "[^[:alnum:]]", upper = FALSE, sep = "") {
    assert_that(is.character(x))
    assert_that(is.string(delim))
    s <- strsplit(x, delim)
    tfun <- function(y) {
        if (any(is.na(y))) {
            y
        } else {
            first <- substring(y, 1, 1)
            if (isTRUE(upper))
                first <- toupper(first)
            else first[-1] <- toupper(first[-1])
            paste(first, substring(y, 2), sep = "", collapse = sep)
        }
    }
    vapply(s, tfun, FUN.VALUE = "", USE.NAMES = FALSE)
}

##-----------------------------------------------------------------------------


## define column names that we will remove from the results because we don't
## think they will be useful in the ALA4R context
# TODO: update with new functions
unwanted_columns <- function(type) {
    type <- match.arg(tolower(type), c("general", "layers", "occurrence",
                                       "assertions", "taxa", "media",
                                       "occurrence_stored",
                                       "occurrence_indexed"))
    switch(type,
           "general" = c("rawRank", "rawRankString", "rankId", "rankID",
                         "left", "right", "idxType", "highlight",
                         "linkIdentifier", "isExcluded"),
           "layers" = c("pid", "path", "path_orig", "path_1km", "enabled",
                        "uid", "licence_level", "lookuptablepath", "mdhrlv",
                        "mddatest", "datalang", "grid", "shape", "enabled",
                        "indb", "spid", "sid", "sdesc", "sname",
                        "defaultlayer", "namesearch", "intersect",
                        "layerbranch", "analysis", "addtomap"),
           "occurrence" = c("lft", "rgt", "rankId"),
           "occurrence_stored"=,
           "occurrence_indexed"=,
           c("")
           )
}

##-----------------------------------------------------------------------------



rename_variables <- function(varnames, type, verbose = ala_config()$verbose) {
    if (length(varnames) < 1) {
        ## catch in case names from empty data frame were passed
        return(varnames)
    }
    assert_that(is.character(varnames))
    assert_that(is.string(type))
    ## use "other" to make no variable name substitutions, just enforce
    ## case/separator conventions
    type <- match.arg(tolower(type), c("general", "layers", "occurrence",
                                       "occurrence_stored",
                                       "occurrence_indexed", "assertions",
                                       "other"))

    ## change all to camelCase
    varnames <- tocamel(make.names(varnames))
    ## try to convert some all-lowercase names to camel, e.g.
    ## environmentalvaluemax minlatitude minlongitude
    for (kw in c("longitude", "latitude", "value", "units")) {
        varnames <- str_replace_all(varnames, kw,
                                    paste(toupper(substring(kw, 1, 1)),
                                          substring(kw, 2), sep = ""))
    }
    ## some that only seem to appear at the ends of variable names, so be
    ## conservative with these replacements
    for (kw in c("min", "max", "path")) {
        varnames <- str_replace_all(varnames, paste(kw, "$", sep = ""),
                                    paste(toupper(substr(kw, 1, 1)),
                                          substring(kw, 2), sep = ""))
    }
    ## enforce first letter lowercase
    varnames <- paste(tolower(substr(varnames, 1, 1)),
                      substring(varnames, 2), sep = "")
    ## some global re-naming by data type
    if (type == "general") {
        ## general names, from e.g. name searching
        varnames[varnames == "occCount"] <- "occurrenceCount"
        varnames[varnames == "classs"] <- "class"
        if (!any(varnames == "commonName")) {
            ## taxinfo_download provides "vernacularName", others "commonName"
            varnames[varnames == "vernacularName"] <- "commonName"
            ## search_guids provides "commonNameSingle", others "commonName"
            varnames[varnames == "commonNameSingle"] <- "commonName"
        }
        varnames <- str_replace_all(varnames, "conservationStatusInAustralia",
                                    "conservationStatusAUS")
        varnames <- str_replace_all(varnames, "conservationStatusIn",
                                    "conservationStatus")
        ## taxinfo_download returns the former, but should be the latter for
        ## consistency elsewhere
        varnames <- str_replace_all(varnames,
                                    "scientificNameForAcceptedConcept",
                                    "acceptedConceptName")

        if (any(varnames == "rank") & any(varnames == "rankString")) {
            if (verbose) {
                warning("data contains both \"rank\" and \"rankString\" columns,
                        not renaming \"rankString\"")
            }
        } else {
            ## returned as "rank" by some services and "rankString" by others
            varnames[varnames == "rankString"] <- "rank"
        }
        ## ditto for taxonRank
        if (any(varnames == "rank") & any(varnames == "taxonRank")) {
            if (verbose) {
                warning("data contains both \"rank\" and \"taxonRank\" columns,
                        not renaming \"taxonRank\"")
            }
        } else {
            ## returned as "Taxon.Rank" (camelcased to "taxonRank") by
            ## taxinfo_download
            varnames[varnames == "taxonRank"] <- "rank"
        }
    } else if (type == "layers") {
        varnames[varnames == "desc"] <- "description"
    } else if (type %in% c("occurrence", "occurrence_stored",
                           "occurrence_indexed")) {
        ## old columns: Scientific Name, Matched Scientific Name
        ## new columns: Scientific Name - original, Scientific Name
        varnames[varnames == "recordID"] <- "id"
        varnames[varnames == "xVersion"] <- "version"
        varnames <- str_replace_all(varnames, regex("axonconceptguid",
                                                    ignore_case = TRUE),
                                    "axonConceptLsid")
        varnames <- str_replace_all(varnames, "vernacularName", "commonName")
        varnames <- str_replace_all(varnames, "taxonRank", "rank")
        ## rawSomething to somethingOriginal
        ## first-letter lowercase will be lost here but gets fixed below
        varnames <- str_replace_all(varnames, "^raw(.*)$", "\\1Original")
        ## dump "matched", "processed", and "parsed"
        varnames <- str_replace_all(varnames,
                                    regex("(matched|processed|parsed)",
                                          ignore_case = TRUE), "")
    } else if (type == "assertions") {
        a <- ala_fields("assertions", as_is = TRUE)
        ## want all assertion field names to match those in a$name
        ## but some may be camelCased versions of the description
        ## use "other" here to avoid this renaming code block, just apply
        ## camelCasing etc
        a$description <- rename_variables(a$description, type = "other")
        varnames <- vapply(varnames, function(z) {
            ifelse(z %in% a$name, z, ifelse(sum(z == a$description) == 1,
                                           a$name[a$description == z], z))
            }, FUN.VALUE = "", USE.NAMES = FALSE)
    }
    ## do this again, it may have been lost in the processing: enforce first
    ## letter lowercase
    varnames <- paste(tolower(substr(varnames, 1, 1)), substring(varnames, 2),
                      sep = "")
    if (type %in% c("layers", "occurrence", "occurrence_stored",
                    "occurrence_indexed")) {
        ## but some acronyms in layer names should remain all-uppercase
        ## currently this list is:
        ## c("iBRA", "iMCRA", "aCTTAMS", "gER", "nZ", "nSW", "lGA", "nRM",
        ## "rAMSAR", "nDVI", "nPP", "aSRI", "gEOMACS")
        ## but since these all occur at the start of variable names, we can
        ## catch them with a regular expression and not need to hardcode a list
        idx <- str_detect(varnames, "^[a-z][A-Z]")
        temp <- varnames[idx]
        varnames[idx] <- paste(toupper(substr(temp, 1, 1)), substring(temp, 2),
                               sep = "")
        ## "seaWIFS" to "SeaWIFS"
        varnames <- str_replace_all(varnames, "seaWIFS", "SeaWIFS")
    }

    if (type == "assertions") { ###hardcoded assertion variable name changes
        ## these assertions come back from the ALA service with the wrong names
        if ("coordinatesAreOutOfRangeForSpecies" %in% varnames) {
            varnames[varnames == "coordinatesAreOutOfRangeForSpecies"] <-
                "coordinatesOutOfRange"
        }
        if ("collectionDateMissing" %in% varnames) {
            varnames[varnames == "collectionDateMissing"] <-
                "missingCollectionDate"
        }
        if ("coordinateUncertaintyNotSpecified" %in% varnames) {
            varnames[varnames == "coordinateUncertaintyNotSpecified"] <-
                "uncertaintyNotSpecified"
        }
    }
    ##return the varnames
    varnames
}

## construct url path, taking care to remove multiple forward slashes,
## leading slash
clean_path <- function(..., sep = "/") {
    ## collapse individual arguments
    path1 <- vapply(list(...), function(z) paste(z, sep = sep, collapse = sep),
                    FUN.VALUE = "", USE.NAMES = FALSE)
    ## workaround to avoid replacing "http://" with "http:/", since this is
    ## now used in GUID strings (July 2016)
    path <- paste(path1, sep = sep, collapse = sep) ## paste parts together
    path <- gsub("http://", "http:@@", path, fixed = TRUE)
    path <- gsub(paste0("[", sep, "]+"), sep, path) ## remove multiple slashes
    path <- gsub("http:@@", "http://", path, fixed = TRUE)
    sub(paste0("^", sep), "", path) ## remove leading slash
}

## convenience function for building urls
## pass path in one of several ways
##  as single string: build_url_from_parts(base_url,"path/to/thing")
##  as a character vector or list: build_url_from_parts(base_url,
## c("path", "to", "thing"))
##  or a combination
build_url_from_parts <- function(base_url, path = NULL, query = list()) {
    this_url <- parse_url(base_url)
    this_url$path <- clean_path(this_url$path, path)
    if (length(query) > 0) {
        this_url$query <- query
    }
    build_url(this_url)
}


## wrapper around read.csv but suppressing "incomplete final line" warning
read_csv_quietly <- function(...) {
    read_warnings <- NULL
    w_handler <- function(w) {
        if (!grepl("incomplete final line", as.character(w),
                   ignore.case = TRUE)) {
            read_warnings <<- c(read_warnings, list(w))
                            invokeRestart("muffleWarning")
        }
    }
    out <- withCallingHandlers({
        read.csv(...)
        }, warning = w_handler)
    ## now throw any warnings that got collected, because they weren't about a
    ## final missing line break
    for (w in read_warnings) warning(w)
    out
}

replace_nonbreaking_spaces <- function(s)
    gsub("\ua0", " ", s)


##----------------------Helper functions for v2--------------------------------

wanted_columns <- function(type) {
    switch(type,
           "taxa" = c("search_term", "scientific_name",
                      "scientific_name_authorship", "taxon_concept_id", "rank",
                      "match_type", "kingdom", "phylum", "class", "order",
                      "family", "genus", "species", "issues"),
           "profile"= c("id", "name", "shortName", "description"),
           "media" = c("rightsHolder", "imageIdentifier", "format",
                       "occurrenceID", "recognisedLicence", "license",
                       "creator", "title", "rights", "mimeType",
                       "media_id"),
           "layer" = c("source_link", "display_name", "id", "type",
                       "description"),
           "quality_filter" = c("description", "filter"))
}

# rename specific columns, and convert camelCase to snake_case
rename_columns <- function(varnames, type) {
    if (type == "media") {
        varnames[varnames == "mimeType"] <- "format"
        varnames[varnames == "imageIdentifier"] <- "media_id"
    }
    else if (type == "taxa") {
        varnames[varnames == "classs"] <- "class"
    } else if (type == "layer") {
        varnames[varnames == "displayname"] <- "display_name"
    }
    # change all to snake case?
    if (type == "taxa") {
        varnames <- tolower(gsub("([a-z])([A-Z])", "\\1_\\L\\2", varnames,
                         perl = TRUE))
    } else if (type == "checklist") {
      varnames <- tolower(gsub('\\.', '_', varnames))
    }
    varnames
}


build_quality_query <- function(dq_profile) {
    if (is.null(dq_profile)) {
        return(NULL)
    }
    paste(ala_quality_filters(dq_profile)$filter, collapse = ' AND ')
}

build_date_query <- function(filters) {
    paste0(names(filters), ":[", filters[[1]][1],
           " TO ", filters[[1]][2], "]")
}

# rename?
build_general_query <- function(filters) {
    if (length(filters) == 0) {
        return(NULL)
    }
    # order filters so cached file can be found
    filters <- filters[order(names(filters))]
    quoted_filters <- lapply(filters, function(x) {
        paste0("\"", x, "\"")
    })
    paste(names(filters), quoted_filters, sep = ":", collapse = " AND ")
}

build_taxa_query <- function(ids) {
    # order filters so cached file can be found
    ids <- ids[order(ids)]
    paste0("(lsid:",paste(ids, collapse = " OR lsid:"),")")
}

build_area_query <- function(area) {
    # check wkt
    if (is.character(area)) {
        # maybe give a more helpful error message here?
        validate_wkt(area)
        # should this also take other area types?
    } else {
      tryCatch(area <- build_wkt(area),
               error = function(e) {
                 e$message <- "Area must be either a wkt string or an sf spatial object."
                 stop(e)
               })
  }
  message("returning, ", area)
  area
}


validate_wkt <- function(wkt) {
    max_char <- 10000
    if (nchar(wkt) > max_char) {
        stop("The WKT string provided is greater than ", max_char,
             " characters , please simplify and try again.")
    }
    if (!wellknown::lint(wkt)) {
      stop("The WKT provided is invalid.")
    }
}

# build a valid wkt string from a spatial polygon
build_wkt <- function(polygon) {
    wkt <- st_as_text(st_geometry(polygon))
    if (nchar(wkt) > 10000) {
        stop("The area provided is too complex. Please simplify it and try again.")
    }
    wkt
}

# filters vs. fields terminology
# should handle miscased things?
# should try to fuzzy match?
# should also validate facets?
validate_filters <- function(filters) {
    # filters are provided in a dataframe
    # key should be a valid field name and value should be a valid category for that field
    # valid options is a combination of ala_layers and ala_fields?
    
    invalid_filters <- filters$name[!filters$name %in% ala_fields()$name]
    
    if (length(invalid_filters) > 0) {
        stop("The following filters are invalid: ",
             paste(invalid_filters, collapse = ", "),
             ". Use `ala_fields()` to get a list of valid options")
    }
}

# POST params to server to get around url length constraint
# POST all the filters here, or just ones that are likely to cause the maximum
# length to be exceeded? POST only if the url is too long, or by default?
# what is the maximum length? around 2000 chars?
cache_params <- function(query) {
    #url <- parse_url(getOption("ALA4R_server_config")$base_url_biocache)
    url <- "https://biocache-ws.ala.org.au"
    resp <- ala_POST(url, path = "ws/webportal/params", body = query)
    return(resp)
}

