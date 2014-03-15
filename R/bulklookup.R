#' Bulk lookup of taxonomic names
#' 
#' Provides GUID, taxonomic classification, and other information for a list of names. Fuzzy matching is used to find the best match for each supplied name, which may be a partial or incorrectly spelled name
#' 
#' 
#' @param taxa string: a single name (string) or vector of names
#' @return A data frame of results
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/}
#' @examples
#' \dontrun{
#' bulklookup(c("Grevillea humilis","Grevillea humilis subsp. maritima"))
#' }
#' @export bulklookup

## **NOTE** this function will be replaced by one using the new service, once it is deployed

bulklookup=function(taxa=c()) {
    ## input argument checks
    if (identical(class(taxa),"list")) {
        taxa=unlist(taxa)
    }
    if (! identical(class(taxa),"character")) {
        stop("expecting string or vector of strings as input")
    }
    if (any(nchar(taxa)<1)) {
        stop("input contains empty string")
    }
    if (length(taxa)<1) {
        stop("empty input")
    }
    taxa = lapply(taxa,clean_string) ## clean up the taxon name
    base_url=paste(ala_config()$base_url_bie,"species/bulklookup.json",sep="")
    x=cached_post(url=base_url,body=jsonlite::toJSON(taxa),type="json")
    #if using jsonlite {
        x=x[[1]]
    #} else {
    #    x=rbind.fill(lapply(content(x)[[1]],as.data.frame)) ## convert each element of content(x)[[1]] into data frame, then combine
    #}
    x
}

