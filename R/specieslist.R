#' Get list of taxa and their occurrence counts, with/without a matching
#' taxonomic query within an optionally-defined WKT-defined polygon
#' 
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/} 
#' \url{http://www.geoapi.org/3.0/javadoc/org/opengis/referencing/doc-files/WKT.html}
#' 
#' @param taxon string: Text of taxon, e.g. "Macropus rufus" or "macropodidae"
#' @param wkt string: WKT (well-known text) defining a polygon within
#' which to limit taxon search, e.g. "POLYGON((140 -37,151 -37,151 -26,140 -26,140 -37))"
#' @param fq string: filters to be applied to the original query. These are of the form "INDEXEDFIELD:VALUE" e.g. "kingdom:Fungi" See ala_fields("occurrence") for all the fields that are queryable
#' @param page_size Maximum number of records to return (may not be honoured by
#' the ALA server). Default=NA, meaning that the server default value (currently 10) will be used.
#' @return Data frame with (at least) the columns
#' \itemize{
#' \item{family} \item{Scientific.name} \item{Common.name} \item{Taxon.rank}
#' \item{GUID} \item{N.occurrences}
#' }
#' @examples
#' \dontrun{
#' x=specieslist(taxon="macropus",wkt="POLYGON((140 -37,151 -37,151 -26,140 -26,140 -37))")
#' x=specieslist(wkt="POLYGON((140 -37,151 -37,151 -26,140 -26,140 -37))")
#' x=specieslist(wkt="POLYGON((147.62 -42.83,147.60 -42.86,147.65 -42.87,147.70 -42.86,147.62 -42.83))",page_size=30)
#' }
#' @export specieslist

## TODO: (As per advice from Adam): move away from webportal/species.csv to http://biocache.ala.org.au/ws/occurrences/facets/download?q=data_resource_uid:dr364&facets=species_guid&lookup=true&count=true

specieslist=function(taxon="",wkt="",fq=NULL,page_size=NA) {
    ## TODO: add filtering functionality (fq parm passed in URL)
    assert_that(is.string(taxon))
    assert_that(is.string(wkt))
    if (!is.na(page_size)) {
        assert_that(is.count(page_size))
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
            warning("WKT string appears to be invalid: ",wkt)
        }
        this_query$wkt=wkt
    }
    if (length(this_query)==0) {
        ## not a valid request!
        stop("invalid request: need at least taxon or WKT to be specified")
    }
    if (!is.null(fq)) {
        assert_that(is.string(fq))
        this_query$fq=fq
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

