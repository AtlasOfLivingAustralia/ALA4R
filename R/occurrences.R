#' Get occurrence data
#' 
#' Retrieve ALA occurrence data via the "occurrence download" web service
#' 
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/} 
#' \url{https://docs.google.com/spreadsheet/ccc?key=0AjNtzhUIIHeNdHhtcFVSM09qZ3c3N3ItUnBBc09TbHc}
#' \url{http://www.geoapi.org/3.0/javadoc/org/opengis/referencing/doc-files/WKT.html}
#' 
#' @param taxon string: taxonomic query, e.g. "macropus rufus"
#' @param wkt string: a WKT (well-known text) string providing a spatial polygon within
#' which to search, e.g. "POLYGON((140 -37,151 -37,151 -26,140.1310 -26,140 -37))"
#' @param fields string vector: a vector of field names to return. Note that the columns of the returned data frame 
#' are not guaranteed to retain the ordering of the field names given here. 
#' See ALA4R funtion ala_fields("occurrence") for valid field names.
#' @param download_reason_id integer: a reason code for the download. See ala_reasons() for valid values. The download_reason_id can be passed directly to this function, or alternatively set using ala_config(download_reason_id=...)
#' 
#' @return Data frame
#' 
#' @examples
#' \dontrun{ 
#' x=occurrences(taxon="macropus",fields=c("longitude","latitude","common_name","taxon_name","el807"),download_reason_id=10)
#' x=occurrences(taxon="data_resource_uid:dr356",download_reason_id=10)
#' 
#' y=occurrences(taxon="alaba vibex",fields=c("latitude","longitude"),download_reason_id=10)
#' qv: http://biocache.ala.org.au/ws/occurrences/index/download?reasonTypeId=10&q=Alaba%20vibex&fields=latitude,longitude&qa=none
#' qv: http://biocache.ala.org.au/ws/occurrences/index/download?reasonTypeId=10&q=Eucalyptus%20gunnii&fields=latitude,longitude&qa=none&fq=basis_of_record:LivingSpecimen
#' }
#' @export occurrences

## note: this is a very sketchy first implementation of the new download API
## currently using the TEST SERVER

## TODO: support extra params fq, startindex, etc (see API page)
## fq can query any field from http://biocache.ala.org.au/ws/index/fields, i.e. ala_fields("occurrence")
## TODO: better error message for unfound taxon.
## TODO: Parsing csv remains an issue on Windows


##q *  String	
##Query of the form field:value e.g. q=genus:Macropus or a free text search e.g. q=Macropus
##
##fq	String	
##Filters to be applied to the original query. These are additional params of the form fq=INDEXEDFIELD:VALUE e.g. fq=kingdom:Fungi. See http://biocache.ala.org.au/ws/index/fields for all the fields that a queryable.
##
##lat	Double	
##The decimal latitude to limit records to. Use with lon and radius to specify a "search" circle
##
##lon	Double	
##The decimal latitude to limit records to. Use with lon and radius to specify a "search" circle
##
##radius	Double	
##The radius in which to limit records (relative to the lat, lon point). Use with lat and lon to specify a "search" circle.
##
##wkt	Integer	
##The polygon area in which to limit records. For information on Well known text
##
##file	String	
##The name of the file to produce. This will default to data.
##
##email	String	
##The email address of the user performing the download.
##
##reason	String	
##A user supplied description of the reason for the download
##
##fields	String	
##A CSV list of fields to include in the download. This can be an index field name OR a Darwin Core Field name. There is a default list that is included if no value is supplied.
##
##extra	String	
##a CSV list of fields in include in addition to the "fields". This is useful if you would like to make use of the default fields and include extra fields.
##
##reasonTypeId	Integer	
##A reason code for the download. See reasons for valid values.
##
##fileType	String	
##The file format for the download. Either csv or shp. When no value is supplied csv is assumed.
##
##sourceTypeId	Integer	
##A source type code for the download. This indicates which system requested the download. See sources for valid values.
##
##qa	String	
##A CSV list of record issues to include in the download. See assertions for possible values to include. When no value is supplied all the record issues are included. To include NO record issues set this value to none.

occurrences=function(taxon="",wkt="",fields=c(),download_reason_id=ala_config()$download_reason_id) {
    ## check input parms are sensible
    reason_ok=!is.na(download_reason_id)
    if (reason_ok) {
        valid_reasons=ala_reasons()
        reason_ok=download_reason_id %in% valid_reasons$id
    }
    if (! reason_ok) {
        stop("download_reason_id must be a valid reason_id. See ala_reasons(). Set this value directly here or through ala_config(download_reason_id=...)")
    }
    
    
    assert_that(is.string(taxon))
    assert_that(is.string(wkt))
  
    #taxon = clean_string(taxon) ## clean up the taxon name # no - because this can be an indexed query like field1:value1
    #base_url=paste(ala_config()$base_url_biocache,"occurrences/index/download",sep="")
    base_url="http://biocache-test.ala.org.au/ws/occurrences/index/download" ## until new changes get propagated to live server
    
  
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
  
    ## may wish to specify file=data or data.csv to make sure the file within the zip is consistently named
    
    if (length(fields)>0) {
        assert_that(is.character(fields))
        ## user has specified some fields
        valid_fields=ala_fields(fields_type="occurrence")
        unknown=setdiff(fields,valid_fields$name)
        if (length(unknown)>0) {
            stop("invalid fields requested: ", str_c(unknown,collapse=", "))
        }
        this_query$fields=str_c(fields,collapse=",")
    }
    this_query$reasonTypeId=download_reason_id
    this_query$esc="\\" ## force backslash-escaping of quotes rather than double-quote escaping
    
    this_url=parse_url(base_url)
    this_url$query=this_query
  
    ## these downloads can potentially be large, so we want to download directly to file and then read the file
    thisfile=cached_get(url=build_url(this_url),type="binary_filename")
    if (!(file.info(thisfile)$size>0)) {
        ## empty file
        x=NULL
    } else {
        x=read.table(unz(thisfile,filename="data.csv"),sep=",",header=TRUE,comment.char="",as.is=TRUE)
        ## this is very slow. I'd like to use e.g. data.table with the line below, but the embedded quotes are double-quote escaped rather than backslash-escaped
        ##xdt=fread("../../temp/data.csv",sep=",",stringsAsFactors=FALSE,header=TRUE,verbose=ala_config()$verbose)
        ## use read.table for now, until those issues are resolved
        
        ##Taxon.identification.issue has embedded quotes, e.g. "[""noIssue""]"
        
        ## also read the citation info
        ##xc=read.table(unz(thisfile,"citation.csv"),sep=",",header=TRUE,comment.char="",as.is=TRUE)
        ## nope, that doesn't work because there are line breaks WITHIN text field values
        ##fid=unz(thisfile,filename="citation.csv",open="rt")
        ##suppressWarnings(xc<-scan(fid,what="character",sep=NULL,quiet=TRUE,allowEscapes=TRUE)) ## suppress warnings here, otherwise we get warnings about newlines being embedded within quoted string. Would be better to be more selective about the warnings we are suppressing, or use a method that does not throw such a warning
        ## nope, that gets confused by newline chars
        ## note that citation.csv won't exist if there was no data in the download: need to check that it exists or wrap this in e.g. tryCatch
        fid=unz(thisfile,filename="citation.csv",open="rt")
        xc=readChar(fid,nchars=1e6)        
        close(fid)

        #fid=unz(thisfile,filename="citation.csv",open="rt")
        #suppressWarnings(xc<-scan(fid,what="character",sep=",",quiet=TRUE,allowEscapes=TRUE))
        # embedded quotes 
        
        ## read the header line separately
        xc_hdr=read.table(unz(thisfile,filename="citation.csv"),sep=",",header=FALSE,comment.char="",as.is=TRUE,nrows=1)
        xc=as.data.frame(xc)
        x=list(data=x,meta=xc)
    }
    x
}
