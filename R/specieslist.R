#' Get list of taxa and occurrence counts
#' 
#' Get list of taxa and their occurrence counts, matching a taxonomic query
#' and/or within a specified spatial area
#' 
#' 
#' @param taxon Taxonomic query, e.g. "macropus" or "macropodidae"
#' @param wkt A WKT (well-known text) string providing a spatial polygon within
#' which to search, e.g. "POLYGON((140 -37,151 -37,151 -26,140.1310 -26,140
#' -37))"
#' @param page_size Maximum number of records to return (may not be honoured by
#' the ALA server). Default=NA, meaning that the server default value
#' (currently 10) will be used.
#' @return Data frame with (at least) the columns
#' "Scientific.name","Common.name","Taxon.rank","LSID","N.occurrences"
#' @author Ben Raymond \email{ben@@theraymonds.org}, Jeremy VanDerWal
#' \email{jjvanderwal@@gmail.com}
#' @references \url{http://spatial.ala.org.au/layers-service/}
#' @examples
#' 
#' x=specieslist(taxon="macropus",wkt="POLYGON((140 -37,151 -37,151 -26,140.1310 -26,140 -37))")
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

