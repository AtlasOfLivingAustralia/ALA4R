## download occurrence data
## http://biocache.ala.org.au/ws/webportal/occurrences.gz?q=macropus&fl=longitude,latitude
ala_occurrences=function(taxon="",wkt="",page_size=NA,fields=c()) {
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
    base_url="http://biocache.ala.org.au/ws/webportal/occurrences.gz"

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
    thisfile=ala_download_to_file(url=build_url(this_url))
    x=read.table(thisfile,sep=",",header=TRUE,comment.char="")
    ## currently this gives a somewhat obscure error if there are no results returned (file is empty)
    x
}
