#' Partial-name search
#' 
#' A partial-name search for species names & identifiers used at the ALA. If searching for a taxon name, and the scientific name or common name of the taxon are known, use \code{\link{search_names}} instead.
#' 
#' @references Associated ALA web service: \url{http://api.ala.org.au/#ws25}
#' @seealso \code{\link{search_names}} for searching known scientific or common taxonomic names
#' @param taxon string: part of the scientific, common name of the taxa
#' @param geo_only logical: if TRUE, only results that have geospatial occurrence records will be included
#' @param output_format string: controls the print method for the returned object. Either "complete" (the complete data structure is displayed), or "simple" (a simplified version is displayed). Note that the complete data structure exists in both cases: this option only controls what is displayed when the object is printed to the console. The default output format is "simple"
#' @param index_type string: the index type to limit. Values include: TAXON REGION COLLECTION INSTITUTION DATASET. By default, no index_type limit is applied
#' @param limit numeric: the maximum number of matches returned (defaults to the server-side value - currently 10)
#' @return A dataframe of results. The contents (column names) of the data frame will vary depending on the details of the search and the results.
#' @examples
#' \dontrun{
#' # find any names containing "allaba"
#' search_partial_name("allaba",output_format="simple")
#' 
#' # retrieve only species that have geolocated occurrence records
#' search_partial_name("Gallaba",geo_only=TRUE)
#' }
#' @export
search_partial_name <- function(taxon,geo_only=FALSE,output_format="simple",index_type,limit) {
    assert_that(is.notempty.string(taxon))
    output_format <- match.arg(tolower(output_format),c("simple","complete"))
    taxon <- clean_string(taxon) #clean up the taxon name
    taxon <- gsub(' ','+',taxon) #replace spaces with + to force both terms in the search
	
    this_query <- list(q=taxon)
    if (!missing(limit)) {
        assert_that(is.count(limit))  #check limit is integer >0 and single value
        this_query$limit <- limit
    }
    assert_that(is.flag(geo_only),noNA(geo_only))
    if (geo_only) {
        this_query$geoOnly <- "true" #Check for taxa that have locations (some have no location)
    }
    if (!missing(index_type)) {
        assert_that(is.string(index_type))
        index_type <- match.arg(toupper(index_type),c("TAXON","REGION","COLLECTION","INSTITUTION","DATASET"))
        this_query$idxType <- index_type
    }
    this_url <- build_url_from_parts(getOption("ALA4R_server_config")$base_url_bie,c("search","auto.json"),this_query)
    out <- cached_get(url=this_url,type="json") #get the data
    out <- out[[1]] #looking at the data
	
    if (length(out)<1) {
        ## no results
        if (ala_config()$warn_on_empty) {
            warning('no matched taxa')
        }
        return(data.frame())
    } else {
        ## matchedNames, commonNameMatches, and scientificNameMatches are all lists of strings
        ## convert each list to single string
        for (ii in 1:nrow(out)) {
            out$matchedNames[ii] <- paste(out$matchedNames[[ii]],collapse=", ")
            out$scientificNameMatches[ii] <- paste(out$scientificNameMatches[[ii]],collapse=", ")
            out$commonNameMatches[ii] <- paste(out$commonNameMatches[[ii]],collapse=", ")
        }
        out$matchedNames <- unlist(out$matchedNames)
        out$scientificNameMatches <- unlist(out$scientificNameMatches)
        out$commonNameMatches <- unlist(out$commonNameMatches)
        ## rename columns
        names(out) <- rename_variables(names(out),type="general")
        ## remove some columns that are unlikely to ever be of value to R users
        xcols <- setdiff(names(out),unwanted_columns("general"))
        ## type check
        if (is.logical(out$commonName)) out$commonName <- as.character(out$commonName)
        ## reorder columns, for minor convenience
        firstcols <- intersect(c("name","commonName","guid","rankString"),xcols)
        xcols <- c(firstcols,setdiff(xcols,firstcols))
        out <- subset(out,select=xcols)
        ## some names have non-breaking spaces
        for (n in intersect(names(out),c("name","matchedNames","scientificNameMatches")))
            out[,n] <- replace_nonbreaking_spaces(out[,n])
    }
    class(out) <- c('search_partial_name',class(out)) #add the search_partial_name class
    attr(out,"output_format") <- output_format
    out
}

#' @method print search_partial_name
#' @export
"print.search_partial_name" <- function(x, ...)
{
    cols <- names(x)
    if (identical(attr(x,"output_format"),"simple")) {
        cols <- intersect(c("name","commonName","matchedNames","rankString","guid"),cols)
    }
    m <- as.matrix(format.data.frame(x[,cols], na.encode = FALSE))
    print(m)
    invisible(x)
}


