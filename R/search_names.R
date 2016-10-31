#' Lookup of taxonomic names
#' 
#' Provides GUID, taxonomic classification, and other information for a list of names. 
#' Case-insensitive but otherwise exact matches are used.
#'
#' @references The associated ALA web service: \url{http://api.ala.org.au/#ws87}
#' 
#' @param taxa string: a single name or vector of names
#' @param vernacular logical: if TRUE, match on common names as well as scientific names, otherwise match only on scientific names
#' @param guids_only logical: if TRUE, a named list of GUIDs will be returned. Otherwise, a data frame with more comprehensive information for each name will be returned.
#' @param occurrence_count logical: if TRUE (and if \code{guids_only} is FALSE) then also return the number of occurrences of each matched name.
#' Note that this requires one extra web call for each name, and so may be slow. Only applicable if \code{guids_only} is FALSE.
#' @param output_format string: controls the print method for the returned object (only has an effect when \code{guids_only} is FALSE). Either "complete" (the complete data structure is displayed), or "simple" (a simplified version is displayed). Note that the complete data structure exists in both cases: this option only controls what is displayed when the object is printed to the console. The default output format is "simple"
#' @return A data frame of results, or named list of GUIDs if \code{guids_only} is TRUE. The results should include one entry (i.e. one data.frame row or one list element) per input name. The columns in the data.frame output may vary depending on the results returned by the ALA server, but should include searchTerm, name, rank, and guid.
#' 
#' @examples
#' \dontrun{
#' search_names(c("Grevillea humilis","Grevillea humilis subsp. maritima",
#'   "Macropus","Thisisnot aname"))
#' 
#' search_names(c("Grevillea humilis","Grevillea humilis subsp. maritima",
#'   "Macropus","Thisisnot aname"),guids_only=TRUE)
#' 
#' search_names("kookaburra",vernacular=FALSE)
#' 
#' search_names("kookaburra",vernacular=TRUE)
#' 
#' ## occurrence counts for matched names
#' search_names(c("Grevillea humilis","Grevillea humilis subsp. maritima",
#'   "Macropus","Thisisnot aname"),occurrence_count=TRUE)
#'
#' ## no occurrence counts because guids_only is TRUE
#' search_names(c("Grevillea humilis","Grevillea humilis subsp. maritima",
#'   "Macropus","Thisisnot aname"),occurrence_count=TRUE,guids_only=TRUE)
#' }
#' @export search_names

search_names <- function(taxa=c(),vernacular=FALSE,guids_only=FALSE,occurrence_count=FALSE,output_format="simple") {
    ## input argument checks
    if (is.list(taxa)) {
        taxa <- unlist(taxa)
    }
    if (is.factor(taxa)) {
        taxa <- as.character(taxa)
    }
    assert_that(is.character(taxa))
    if (any(nchar(taxa)<1)) {
        stop("input contains empty string")
    }
    if (length(taxa)<1) {
        stop("empty input")
    }
    assert_that(is.flag(vernacular))
    assert_that(is.flag(guids_only))
    assert_that(is.flag(occurrence_count))
    assert_that(is.character(output_format))
    output_format <- match.arg(tolower(output_format),c("simple","complete"))
    taxa_original <- taxa
    taxa <- sapply(taxa,clean_string,USE.NAMES=FALSE) ## clean up the taxon name
    ## re-check names, since clean_string may have changed them
    if (any(nchar(taxa)<1)) {
        stop("input contains empty string after cleaning (did the input name contain only non-alphabetic characters?)")
    }    
    this_url <- build_url_from_parts(getOption("ALA4R_server_config")$base_url_bie,c("species","lookup","bulk"))
    temp <- jsonlite::toJSON(list(names=taxa,vernacular=vernacular))
    ## toJSON puts vernacular as a single-element array, which causes failures. Need to convert to scalar logical
    temp <- str_replace(temp,"\\[[ ]*false[ ]*\\]","false")
    temp <- str_replace(temp,"\\[[ ]*true[ ]*\\]","true")
    x <- cached_post(url=this_url,body=temp,type="json",content_type="application/json")
    if (identical(x,NA) || identical(x,as.character(NA))) {
        ## if a single non-matched name is supplied, we get NA back
        x <- NULL
    }
    if (guids_only) {
        if (! is.data.frame(x)) {
            ## if we pass multiple names and none of them match, we get a vector of NAs back
            if (!is.null(x) && all(is.na(x))) {
                x <- data.frame()
            }
        }
        if (empty(x)) {
            if (ala_config()$warn_on_empty) {
                warning("no records found");
            }
            x <- as.list(rep(NA,length(taxa_original)))
        } else {
            x <- as.list(x$guid)
        }
        names(x) <- make.names(taxa_original)
    } else {
        if (! is.data.frame(x)) {
            ## if we pass multiple names and none of them match, we get a vector of NAs back
            if (!is.null(x) && all(is.na(x))) {
                x <- data.frame()
            }
        }
        if (! empty(x)) {
            ## column names within the data matrix are returned as camelCase
            ## add searchTerm, so user can more easily see what each original query was
            x$searchTerm <- taxa_original
            ## rename some columns
            names(x)[names(x)=="classs"] <- "class"
            ## remove some columns that are unlikely to be of value here
            xcols <- setdiff(names(x),unwanted_columns("general"))
            ## also remove hasChildren, since it always seems to be false
            xcols <- setdiff(xcols,c("hasChildren"))
            ## reorder columns, for minor convenience
            firstcols <- intersect(c("searchTerm","name","commonName","guid","rank"),xcols)
            xcols <- c(firstcols,setdiff(xcols,firstcols))
            x <- subset(x,select=xcols)            
            attr(x,"output_format") <- output_format            
        } else {
            if (ala_config()$warn_on_empty) {
                warning("no records found");
            }
            x <- data.frame(searchTerm=taxa_original,name=NA,commonName=NA,rank=NA,guid=NA)
            attr(x,"output_format") <- output_format
        }
        ## some names have non-breaking spaces
        for (n in intersect(names(x),c("name","nameComplete","acceptedConceptName")))
            x[,n] <- replace_nonbreaking_spaces(x[,n])
    }
    names(x) <- rename_variables(names(x),type="general")
    if (occurrence_count & !guids_only) {
        ## do this after renaming variables, so that column name guid is predictable
        x$occurrenceCount <- NA
        non_na <- !is.na(x$guid)
        if (any(non_na)) x$occurrenceCount[non_na] <- sapply(x$guid[non_na],function(z) occurrences(taxon=paste0("lsid:",z),record_count_only=TRUE))
    }
    class(x) <- c("search_names",class(x)) ## add the search_names class
    x
}

#' @method print search_names
#' @export
"print.search_names" <- function(x, ...)
{
    if (inherits(x,"list")) {
        ## from guids_only seach
        print(format(x))
    } else {
        cols <- names(x)
        if (identical(attr(x,"output_format"),"simple")) {
            cnn <- if ("commonNameSingle" %in% cols) "commonNameSingle" else "commonName"
            cols <- intersect(c("searchTerm","name",cnn,"rank","guid","occurrenceCount"),cols)
        }
        print(format.data.frame(x[,cols],na.encode=FALSE))
    }
    invisible(x)
}
