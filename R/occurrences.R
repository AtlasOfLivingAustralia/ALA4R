#' Get occurrence data
#' 
#' Retrieve occurrence data via the "Get occurrences as gzipped CSV" web
#' service
#' 
#' 
#' @param taxon Taxonomic query, e.g. "macropus"
#' @param wkt A WKT (well-known text) string providing a spatial polygon within
#' which to search, e.g. "POLYGON((140 -37,151 -37,151 -26,140.1310 -26,140
#' -37))"
#' @param page_size Maximum number of records to return (may not be honoured by
#' the ALA server). Default=NA, meaning that the server default value
#' (currently 10) will be used.
#' @param fields A vector of field names to return. Note that the columns of
#' the returned data frame are not guaranteed to retain the ordering of the
#' field names given here. See ala_fields("occurrence") for valid field names.
#' @return Data frame
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/}
#' @examples
#' \dontrun{ 
#' x=occurrences(taxon="macropus",fields=c("longitude","latitude","common_name","taxon_name","el807"),page_size=1000)
#' }
#' @export occurrences

## TODO: may need to change base URL to match new API (http://biocache.ala.org.au/ws/occurrences/download, but this is not zipped)
## TODO: support extra params fq, startindex, etc (see API page) 

occurrences=function(taxon="",wkt="",page_size=NA,fields=c()) {
    ## check input parms are sensible
    if (!is.na(page_size)) {
        if (!(page_size>0)) {
            warning("page_size should a positive integer, ignoring")
            page_size=NA
        }
    }
    taxon = clean_string(taxon) ## clean up the taxon name
    base_url="http://biocache.ala.org.au/ws/webportal/occurrences.gz"

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
        stop("invalid request: need at least taxon or wkt to be specified")
    }
    ## page_size
    if (!is.na(page_size)) {
        this_query$pageSize=page_size
    }

    if (length(fields)>0) {
        ## user has specified some fields
        valid_fields=ala_fields(fields_type="occurrence")
        unknown=setdiff(fields,valid_fields$name)
        if (length(unknown)>0) {
            stop("invalid fields requested: ", str_c(unknown,collapse=", "))
        }
        this_query$fl=str_c(fields,collapse=",")
    }
    
    this_url=parse_url(base_url)
    this_url$query=this_query
    
    ## these downloads can potentially be large, so we want to download directly to file and then read the file
    thisfile=cached_get(url=build_url(this_url),type="filename")
    if (!(file.info(thisfile)$size>0)) {
        ## empty file
        x=NULL
    } else {
        x=read.table(thisfile,sep=",",header=TRUE,comment.char="")
    }
    x
}
