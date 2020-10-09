#' Function to lookup species information from ALA, given names or unique
#' identifiers. 
#' 
#' Uses ALA name matching service. Species ids from this function can be
#' used to search `ala_occurrences`
#' 
#' @param term string: search term
#' @param term_type string: specifies which type of terms are provided in
#' `term`. One of name `c('name', 'identifier')`
#' @param rank string: (optional) taxonomic rank to search at. If not provided, will
#' conduct a free text search
#' @param return_children logical: return child concepts for the provided term(s)?
#' @param include_counts logical: return occurrence counts for all species returned?
#' 
#' @export ala_taxa


# if no match is found, should a row for the name still be included in the output?
# should a message and/or warning be displayed?
# should vernacular name searching be supported?

ala_taxa <- function(term, term_type = "name", rank = NULL,
                     return_children = FALSE, include_counts = FALSE) {
  
  taxa_url <- 'https://namematching-ws-test.ala.org.au/'
  bie_url <- 'https://bie-ws.ala.org.au/'
  
  assert_that(is.flag(return_children))
  assert_that(term_type %in% c("name", "identifier"),
              msg = "`term_type` must be one of `c('name', 'identifier')`")
  
  if (missing(term)) {
    stop("`ala_taxa` requires a term to search for")
  }
  
  if (!missing(rank)) {
    rank <- validate_rank(rank)
  }
  
  matches <- data.table::rbindlist(lapply(term, function(t) {
    if (term_type == "identifier") {
      result <- identifier_lookup(t)
    } else {
      result <- name_lookup(t, rank)
    }
    if (isFALSE(result$success)) {
      message("No taxon matches were found for \"", t, "\"")
      return(as.data.frame(list(search_term = t), stringsAsFactors = FALSE))
    }
    out_data <- adjust_col_names(as.data.frame(result, stringsAsFactors = FALSE))
    if (return_children) {
      # look up the child concepts for the identifier
      children <- child_concepts(result$taxonConceptID)
      # add children to df
      out_data <- rbind(out_data, children, fill = TRUE)
    }
    cbind(search_term = t, out_data)
  }), fill = TRUE)

  if (include_counts) {
    matches$count <- lapply(matches$taxonConceptID, function(id) {
      record_count(list(fq = paste0("lsid:",id)))
    }) 
  }
  matches
  
}

name_lookup <- function(name, rank = NULL) {
  taxa_url <- 'https://namematching-ws-test.ala.org.au/'
  url <- parse_url(taxa_url)
  if (is.null(rank)) {
    # search by scientific name
    url$path <- "api/search"
    url$query <- list(q = name)
  } else {
    # search by classification
    url$path <- "api/searchByClassification"
    url$query <- paste0(rank, "=" , URLencode(name))
  }
  fromJSON(build_url(url))
}

identifier_lookup <- function(identifier) {
  taxa_url <- 'https://namematching-ws-test.ala.org.au/'
  url <- parse_url(taxa_url)
  url$path <- "/api/getByTaxonID"
  url$query <- list(taxonID = identifier)
  fromJSON(build_url(url))
}

# make sure rank provided is in accepted list
validate_rank <- function(rank) {
  valid_ranks <- c('kingdom', 'phylum', 'class', 'order',
                   'family', 'genus', 'species', 'scientificName')
  # allow user to provide species in place of scientific name
  if (rank == 'species') {
    rank <- 'scientificName'
  }
  if (!(rank %in% valid_ranks)) {
    stop('The provided rank must be one of: ', paste0(valid_ranks,
                                                     collapse = ', '))
  }
  rank
}

child_concepts <- function(identifier) {
  bie_url <- 'https://bie-ws.ala.org.au/'
  url <- parse_url(bie_url)
  url$path <- c("ws/childConcepts", URLencode(identifier, reserved = TRUE))
  children <- fromJSON(build_url(url))
  if (length(children) == 0) {
    message("No child concepts found for taxon id \"", identifier, "\"")
    return()
  }
  
  # lookup details for each child concept
  child_info <- suppressWarnings(data.table::rbindlist(lapply(children$guid, function(id) {
    result <- identifier_lookup(id)
    # keep child even if it can't be found?
    adjust_col_names(as.data.frame(result))
  }), fill = TRUE))
  child_info
}

# fix up species info columns 
# can take any output and make the col names as expected
adjust_col_names <- function(data, type = 'species_info') {
  # this is dodgy but will do for now
  if (nrow(data) > 1) {
    data <- data[1,]
  }
  if (type == 'species_info') {
    cols_to_keep <- c('scientificName', 'scientificNameAuthorship',
                      'taxonConceptID', 'rank','rankID','matchType',
                      'kingdom','kingdomID','phylum','phylumID',
                      'classs','classID','order','orderID','family','familyID',
                      'genus','genusID','species','speciesID','issues')
  }
  
  # keep only required columns
  data <- data[, names(data) %in% cols_to_keep]
  
  # replace 'classs' with 'class' 
  names(data)[names(data) == "classs"] <- "class"
  
  return(data)
}
