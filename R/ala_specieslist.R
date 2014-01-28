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
#' x=ala_specieslist(taxon="macropus",wkt="POLYGON((140:-37,151:-37,151:-26,140.1310:-26,140:-37))")
#' 
#' @export ala_specieslist
ala_specieslist=function(taxon="",wkt="",page_size=NA) {
    ## TODO: add filtering functionality (fq parm passed in URL), assuming that it is relevant here
    ## TODO: check validity of wkt? (but it will require an additional library such as rgeos)
    ## check input parms are sensible
    if (!is.na(page_size)) {
        if (!(page_size>0)) {
            warning("page_size should a positive integer, ignoring")
            page_size=NA
        }
    }
    
    taxon = clean_string(taxon) ## clean up the taxon name
    base_url="http://biocache.ala.org.au/ws/webportal/species.csv"
    this_query=list()
    ## have we specified a taxon?
    if (str_length(taxon)>0) {
        this_query$q=taxon
    }
    ## wkt string
    if (str_length(wkt)>0) {
        this_query$wkt=wkt
    }
    if (length(this_query)==0) {
        ## not a valid request!
        stop("invalid request: need at least taxon or wkt to be specified")
    }
    ## page_size
    if (!is.na(page_size)) {
        this_query$pageSize=page_size
    }
    
    this_url=parse_url(base_url)
    this_url$query=this_query
    
    ## these downloads can potentially be large, so we want to download directly to file and then read the file
    thisfile=download_to_file(url=build_url(this_url))
    x=read.table(thisfile,sep=",",header=TRUE,comment.char="")
    ## rename "X..Occurrences" (which was "# Occurrences" in the csv file)
    names(x)=str_replace(names(x),"X..Occurrences","N.occurrences")
    ## we also have a column labelled "LSID" --- should this be "GUID"? (check before enabling the next line)
    #names(x)=str_replace(names(x),"LSID","GUID")
    x
}

