#' Lookup of taxonomic names
#' 
#' Provides GUID, taxonomic classification, and other information for a list of names. 
#' Case-insensitive but otherwise exact matches are used.
#'
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/}
#' 
#' @param taxa string: a single name or vector of names
#' @param vernacular logical: if TRUE, match on common names as well as scientific names, otherwise match only on scientific names
#' @param guids_only logical: if TRUE, a named list of GUIDs will be returned. Otherwise, a data frame with more comprehensive information for each name will be returned.
#' @param output_format string: controls the print method for the returned object (only applicable when guids_only is FALSE). Either "complete" (the complete data structure is displayed), or "simple" (a simplified version is displayed). Note that the complete data structure exists in both cases: this option only controls what is displayed when the object is printed to the console. The default output format is "simple"
#' @return A data frame of results, or named list of GUIDs if \code{guids_only} is TRUE
#' 
#' @examples
#' 
#' search_names(c("Grevillea humilis","Grevillea humilis subsp. maritima","Macropus","Thisisnot aname"))
#' search_names(c("Grevillea humilis","Grevillea humilis subsp. maritima","Macropus","Thisisnot aname"),guids_only=TRUE)
#' search_names("Grevillea",vernacular=FALSE) ## should return the genus Grevillea
#' x=search_names("Grevillea",vernacular=TRUE) ## should return the species Grevillea banksii, because it has the common name ``Grevillea"
#' str(x) ## see the complete data structure
#' 
#' @export search_names

# TODO: Should #occurrences be returned to help identification? (low priority)
# Note that there were issues with single-word all-lower-case names or other variants of unexpected lower/upper-case (see issue #649)
# This is now resolved, although some other odd case-related behaviour still seems to occur, for example:
# "Gallirallus australis" matches this species, "Gallirallus australi" matches nothing, yet "Gallirallus Australi" matches Gallirallus genus


search_names=function(taxa=c(),vernacular=FALSE,guids_only=FALSE,output_format="simple") {
    ## input argument checks
    if (identical(class(taxa),"list")) {
        taxa=unlist(taxa)
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
    assert_that(is.character(output_format))
    output_format=match.arg(tolower(output_format),c("simple","complete"))
    taxa_original=taxa
    taxa = sapply(taxa,clean_string,USE.NAMES=FALSE) ## clean up the taxon name
    ## re-check names, since clean_string may have changed them
    if (any(nchar(taxa)<1)) {
        stop("input contains empty string after cleaning (did the input name contain only non-alphabetic characters?)")
    }    
    base_url=paste(ala_config()$base_url_bie,"species/lookup/bulk",sep="")
    temp=jsonlite::toJSON(list(names=taxa,vernacular=vernacular))
    ## toJSON puts vernacular as a single-element array, which causes failures. Need to convert to scalar logical
    temp=str_replace(temp,"\\[[ ]*false[ ]*\\]","false")
    temp=str_replace(temp,"\\[[ ]*true[ ]*\\]","true")
    x=cached_post(url=base_url,body=temp,type="json")
    if (identical(x,NA)) {
        ## if a single non-matched name is supplied, we get NA back
        x=NULL
    }
    if (guids_only) {
        if (empty(x)) {
            x=list()
        } else {
            x=as.list(x$guid)
            names(x)=make.names(taxa_original)
        }
    } else {
        if (! empty(x)) {
            ## column names within the data matrix are returned as camelCase
            ## add searchTerm, so user can more easily see what each original query was
            x$searchTerm=taxa_original
            ## rename some columns
            names(x)[names(x)=="classs"]="class"
            ## remove some columns that are unlikely to be of value here
            xcols=setdiff(names(x),unwanted_columns("general"))
            ## reorder columns, for minor convenience
            firstcols=intersect(c("searchTerm","name","commonName","guid","rank"),xcols)
            xcols=c(firstcols,setdiff(xcols,firstcols))
            x=x[,xcols]
            attr(x,"output_format")=output_format
            
        } else {
            x=data.frame()
            attr(x,"output_format")=output_format
        }
    }
    class(x)=c("search_names",class(x)) ## add the search_names class
    x
}

#' @S3method print search_names
"print.search_names" <- function(x, ...)
{
    if (any(class(x)=="list")) {
        ## from guids_only seach
        print(format(x))
    } else {
        cols=names(x)
        if (identical(attr(x,"output_format"),"simple")) {
            cols=intersect(c("searchTerm","name","commonName","rank","guid"),cols)
        }
        m=as.matrix(format.data.frame(x[,cols],na.encode=FALSE))
        print(m)
    }
    invisible(x)
}
