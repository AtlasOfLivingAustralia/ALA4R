#' Get list of taxa and their occurrence counts, with/without a matching
#' taxonomic query within an optionally-defined WKT-defined polygon
#' 
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/} 
#' \url{http://www.geoapi.org/3.0/javadoc/org/opengis/referencing/doc-files/WKT.html}
#' 
#' @param taxon: Text of taxon, e.g. "Macropus rufus" or "macropodidae"
#' @param wkt: Numeric vector string of WKT (well-known text) defining a polygon within
#' which to limit taxon search, e.g. "POLYGON((140 -37,151 -37,151 -26,140 -26,140 -37))"
#' @param page_size Maximum number of records to return (may not be honoured by
#' the ALA server). Default=NA, meaning that the server default value (currently 10) will be used.
#' @return Data frame with (at least) the columns
#' \item {family} \item{Scientific.name} \item{Common.name} \item{Taxon.rank}
#' \item{GUID} \item{N.occurrences}
#' @examples
#' 
#' x=specieslist(taxon="macropus",wkt="POLYGON((140 -37,151 -37,151 -26,140 -26,140 -37))")
#' x=specieslist(wkt="POLYGON((140 -37,151 -37,151 -26,140 -26,140 -37))")
#' x=specieslist(wkt="POLYGON((147.62 -42.83,147.60 -42.86,147.65 -42.87,147.70 -42.86,147.62 -42.83))",page_size=30)
#' 
#' @export specieslist
specieslist=function(taxon="",wkt="",page_size=NA) {
    ## TODO: add filtering functionality (fq parm passed in URL), assuming that it is relevant here
    ## check input parms are sensible
    if (!is.na(page_size)) {
        if (!(page_size>0)) {
            warning("page_size should a positive integer, ignoring")
            page_size=NA
        }
    }
    
    taxon = clean_string(taxon) ## clean up the taxon name
    base_url=paste(ala_config()$base_url_biocache,"webportal/species.csv",sep="")
    this_query=list()
    ## have we specified a taxon?
    if (str_length(taxon)>0) {
        this_query$q=taxon
    }
    ## wkt string
    if (str_length(wkt)>0) {
        if (! check_wkt(wkt)) {
            stop("invalid WKT string ",wkt)
        }
        this_query$wkt=wkt
    }
    if (length(this_query)==0) {
        ## not a valid request!
        stop("invalid request: need at least taxon or WKT to be specified")
    }
    ## page_size
    if (!is.na(page_size)) {
        this_query$pageSize=page_size
    }
    
    this_url=parse_url(base_url)
    this_url$query=this_query
    
    ## these downloads can potentially be large, so we want to download directly to file and then read the file
    thisfile=cached_get(url=build_url(this_url),type="filename")
    x=read.table(thisfile,sep=",",header=TRUE,comment.char="")
    ## rename "X..Occurrences" (which was "# Occurrences" in the csv file)
    names(x)=str_replace(names(x),"X..Occurrences","N.occurrences")
    ## we also have a column labelled "LSID" --- this should be "GUID" for consistency
    names(x)=str_replace(names(x),"LSID","GUID")
    x
}

