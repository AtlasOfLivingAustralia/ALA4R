## Get species list as CSV
## e.g. http://biocache.ala.org.au/ws/webportal/species.csv?q=macropus&wkt=POLYGON((140 -37,151 -37,151 -26,140.1310 -26,140 -37))&pageSize=100

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
    thisfile=ala_download_to_file(url=build_url(this_url))
    x=read.table(thisfile,sep=",",header=TRUE,comment.char="")
    ## rename "X..Occurrences" (which was "# Occurrences" in the csv file)
    names(x)=str_replace(names(x),"X..Occurrences","N.occurrences")
    x
}

