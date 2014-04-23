#' Bulk lookup of taxonomic names
#' 
#' Provides GUID, taxonomic classification, and other information for a list of names. 
#' Case-insensitive but otherwise exact matches are used.
#' 
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/}
#' 
#' @param taxa string: a single name or vector of names
#' @param vernacular logical: if TRUE, match on common names as well as scientific names, otherwise match only on scientific names
#' @return A data frame of results

#' @examples
#' 
#' bulklookup(c("Grevillea humilis","Grevillea humilis subsp. maritima","Macropus","Thisisnot aname"))
#' bulklookup("Grevillea",vernacular=FALSE) ## should return the genus Grevillea
#' bulklookup("Grevillea",vernacular=TRUE) ## should return the species Grevillea banksii, because it has the common name ``Grevillea"
#' x=bulklookup("Alaba",vernacular=FALSE) ## should return info on the genus "Alaba"
#' str(x) ## tidy list of Alaba details
#' 
#' TODO: Should #occurrences be returned to help identification?
#' 
#' @export bulklookup

bulklookup=function(taxa=c(),vernacular=FALSE) {
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
    taxa = sapply(taxa,clean_string,USE.NAMES=FALSE) ## clean up the taxon name
    base_url=paste(ala_config()$base_url_bie,"species/lookup/bulk",sep="")
    temp=jsonlite::toJSON(list(names=taxa,vernacular=vernacular))
    ## toJSON puts vernacular as a single-element array, which causes failures.
    temp=str_replace(temp,"\\[[ ]*false[ ]*\\]","false")
    temp=str_replace(temp,"\\[[ ]*true[ ]*\\]","true")
    x=cached_post(url=base_url,body=temp,type="json")
    x
}
