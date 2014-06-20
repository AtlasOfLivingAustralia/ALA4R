#' Get list of taxa and their occurrence counts
#'
#' Retrieve a list of taxa matching a search query, within a spatial search area, or both
#' 
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/} 
#' \url{http://www.geoapi.org/3.0/javadoc/org/opengis/referencing/doc-files/WKT.html}
#' 
#' @param taxon string: Text of taxon, e.g. "Macropus rufus" or "macropodidae"
#' @param wkt string: WKT (well-known text) defining a polygon within which to limit taxon search, e.g. "POLYGON((140 -37,151 -37,151 -26,140 -26,140 -37))"
#' @param fq string: character string or vector of strings, specifying filters to be applied to the original query. These are of the form "INDEXEDFIELD:VALUE" e.g. "kingdom:Fungi". See \code{ala_fields("occurrence")} for all the fields that are queryable. NOTE that fq matches are case-sensitive, but sometimes the entries in the fields are not consistent in terms of case (e.g. kingdom names "Fungi" and "Plantae" but "ANIMALIA"). fq matches are ANDed by default (e.g. c("field1:abc","field2:def") will match records that have field1 value "abc" and field2 value "def"). To obtain OR behaviour, use the form c("field1:abc OR field2:def")
#' @return data frame of results, with taxa and their occurrence counts
#' @seealso \code{\link{ala_fields}} for occurrence fields that are queryable via the \code{fq} parameter
#' @examples
#' x=specieslist(taxon="macropus",wkt="POLYGON((140 -37,151 -37,151 -26,140 -26,140 -37))")
#' x=specieslist(wkt="POLYGON((140 -37,151 -37,151 -26,140 -26,140 -37))")
#' x=specieslist(wkt="POLYGON((147.62 -42.83,147.60 -42.86,147.65 -42.87,147.70 -42.86,147.62 -42.83))",fq="rank:species")
#' @export specieslist

## TODO avoid warning on empty results (see below)
## TODO? use data.table if available, similarly to occurrences()

specieslist=function(taxon,wkt,fq) {
    this_query=list()
    if (!missing(taxon)) {
        assert_that(is.string(taxon))
        # taxon = clean_string(taxon) ## clean up the taxon name ## no, don't: this can be of the form index:value
        this_query$q=taxon
    }
    if (!missing(wkt)) {
        assert_that(is.string(wkt))
        if (! check_wkt(wkt)) {
            warning("WKT string appears to be invalid: ",wkt)
        }
        this_query$wkt=wkt
    }    
    if (length(this_query)==0) {
        ## not a valid request!
        stop("invalid request: need either fq or wkt parameter to be specified")
    }
    if (!missing(fq)) {
        assert_that(is.character(fq))
        ## can have multiple fq parameters, need to specify in url as fq=a:b&fq=c:d&fq=...
        check_fq(fq,type="occurrence") ## check that fq fields are valid
        fq=as.list(fq)
        names(fq)=rep("fq",length(fq))
        this_query=c(this_query,fq)
    }
    base_url=paste(ala_config()$base_url_biocache,"occurrences/facets/download",sep="")
    this_query$facets="taxon_concept_lsid" ## or "species_guid" to avoid genus and higher records
    ##this_query$lookup="true" ## "set to true if you would like the download include the scientific names and higher classification for the supplied guids. Downloads that include this param will take extra time as a lookup need to be performed"
    this_query$count="true"
    
    this_url=parse_url(base_url)
    this_url$query=this_query
    ## these downloads can potentially be large, so we want to download directly to file and then read the file
    thisfile=cached_get(url=build_url(this_url),type="filename")
    ## check for zero-sized file
    if (file.info(thisfile)$size>0) {
        x=read.table(thisfile,sep=",",header=TRUE,comment.char="")
        ## rename "X..Occurrences" (which was "# Occurrences" in the csv file), or "Number.of.records" (later webservice) or "count" (current web service!)
        names(x)[names(x) %in% c("count","X..Occurrences","Number.of.records")]="occurrenceCount"
        names(x)=rename_variables(names(x),type="occurrence")
    } else {
        x=NULL
    }
    x
}
