#' Lookup of species identifiers
#' 
#' Provides names, taxonomic classification, and other information for a list of GUIDs.
#'
#' @references The associated ALA web service: \url{http://api.ala.org.au/#ws87}
#' 
#' @param guids string: a single GUID or vector of GUIDs
#' @param occurrence_count logical: if TRUE then also return the number of occurrences of each matched GUID.
#' Note that this requires one extra web call for each GUID, and so may be slow.
#' @param output_format string: controls the print method for the returned object. Either "complete" (the complete data structure is displayed), or "simple" (a simplified version is displayed). Note that the complete data structure exists in both cases: this option only controls what is displayed when the object is printed to the console. The default output format is "simple"
#' @return A data frame, which should include one entry (i.e. one data.frame row or one list element) per input GUID. The columns in the data.frame output may vary depending on the results returned by the ALA server, but should include searchTerm, name, rank, and guid.
#' 
#' @examples
#' \dontrun{
#' search_guids(c("urn:lsid:biodiversity.org.au:afd.taxon:95773568-053d-44de-a624-5699f0ac4a59",
#'   "http://id.biodiversity.org.au/node/apni/2890970","this_is_not_a_valid_guid"))
#' }
#' 
#' @export

search_guids <- function(guids=c(),occurrence_count=FALSE,output_format="simple") {
    ## input argument checks
    if (is.list(guids)) {
        guids <- unlist(guids)
    }
    if (is.factor(guids)) {
        guids <- as.character(guids)
    }
    assert_that(is.character(guids))
    if (any(nchar(guids)<1)) {
        stop("input contains empty string")
    }
    if (length(guids)<1) {
        stop("empty input")
    }
    assert_that(is.flag(occurrence_count))
    assert_that(is.character(output_format))
    output_format <- match.arg(tolower(output_format),c("simple","complete"))
    this_url <- build_url_from_parts(getOption("ALA4R_server_config")$base_url_bie,c("species","guids","bulklookup"))
    temp <- jsonlite::toJSON(guids)
    x <- cached_post(url=this_url,body=temp,type="json",content_type="application/json")
    ## x is a named list with one element
    x <- x[[1]]
    if (length(guids)==1 & (identical(x,NA) || identical(x,as.character(NA)))) {
        ## if a single non-matched GUID is supplied, we get NA back
        x <- NULL
    }
    if (! is.data.frame(x)) {
        ## if we pass multiple names and none of them match, we get a vector of NAs back
        if (!is.null(x) && all(is.na(x))) {
            x <- data.frame()
        }
    }
        if (! empty(x)) {
            ## column names within the data matrix are returned as camelCase
            ## add searchTerm, so user can more easily see what each original query was
            x$searchTerm <- guids
            ## remove some columns that are unlikely to be of value here
            xcols <- setdiff(names(x),unwanted_columns("general"))
            ## reorder columns, for minor convenience
            firstcols <- intersect(c("searchTerm","name","commonNameSingle","guid","rank"),xcols)
            ## note commonName now seems to be commonNameSingle
            xcols <- c(firstcols,setdiff(xcols,firstcols))
            x <- subset(x,select=xcols)            
            attr(x,"output_format") <- output_format            
        } else {
            if (ala_config()$warn_on_empty) warning("no records found")
            x <- data.frame(searchTerm=guids,name=NA,commonName=NA,rank=NA,guid=NA)
            attr(x,"output_format") <- output_format
        }

    names(x) <- rename_variables(names(x),type="general")
    if (occurrence_count) {
        x$occurrenceCount <- NA
        non_na <- !is.na(x$guid)
        if (any(non_na)) x$occurrenceCount[non_na] <- sapply(x$guid[non_na],function(z) occurrences(taxon=paste0("lsid:",z),record_count_only=TRUE))
    }
    class(x) <- c("search_guids",class(x)) ## add the search_names class
    x
}

#' @method print search_guids
#' @export
"print.search_guids" <- function(x, ...)
{
    if (inherits(x,"list")) {
        ## from guids_only seach
        print(format(x))
    } else {
        cols <- names(x)
        if (identical(attr(x,"output_format"),"simple")) {
            cols <- intersect(c("searchTerm","name","commonName","rank","guid","occurrenceCount"),cols)
        }
        print(format.data.frame(x[,cols],na.encode=FALSE))
    }
    invisible(x)
}
