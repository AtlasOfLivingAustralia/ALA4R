#' Get occurrence data
#' 
#' Retrieve ALA occurrence data via the "occurrence download" web service. At least one of \code{taxon}, \code{wkt}, or \code{fq} must be supplied for a valid query.
#' 
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \itemize{
#' \item \url{http://api.ala.org.au/} 
#' \item Field definitions: \url{https://docs.google.com/spreadsheet/ccc?key=0AjNtzhUIIHeNdHhtcFVSM09qZ3c3N3ItUnBBc09TbHc}
#' \item WKT reference: \url{http://www.geoapi.org/3.0/javadoc/org/opengis/referencing/doc-files/WKT.html}
#' }
#' 
#' @param download_reason_id integer: (required) a reason code for the download. See \code{ala_reasons()} for valid values. The download_reason_id can be passed directly to this function, or alternatively set using \code{ala_config(download_reason_id=...)}
#' @param reason string: (optional) user-supplied description of the reason for the download. Providing this information is optional but will help the ALA to better support users by building a better understanding of user communities and their data requests
#' @param taxon string: (optional) taxonomic query of the form field:value (e.g. "genus:Macropus") or a free text search ("Alaba vibex")
#' @param wkt string: (optional) a WKT (well-known text) string providing a spatial polygon within which to search, e.g. "POLYGON((140 -37,151 -37,151 -26,140.131 -26,140 -37))"
#' @param fq string: (optional) character string or vector of strings, specifying filters to be applied to the original query. These are of the form "INDEXEDFIELD:VALUE" e.g. "kingdom:Fungi". 
#' See \code{ala_fields("occurrence")} for all the fields that are queryable. 
#' NOTE that fq matches are case-sensitive, but sometimes the entries in the fields are 
#' not consistent in terms of case (e.g. kingdom names "Fungi" and "Plantae" but "ANIMALIA"). 
#' fq matches are ANDed by default (e.g. c("field1:abc","field2:def") will match records that have 
#' field1 value "abc" and field2 value "def"). To obtain OR behaviour, use the form c("field1:abc 
#' OR field2:def")
#' @param fields string vector: (optional) a vector of field names to return. Note that the columns of the returned data frame 
#' are not guaranteed to retain the ordering of the field names given here. If not specified, a default list of fields will be returned. See \code{ala_fields("occurrence")} for valid field names.
#' @param extra string vector: (optional) a vector of field names to include in addition to those specified in \code{fields}. This is useful if you would like the default list of fields (i.e. when \code{fields} parameter is not specified) plus some additional extras. See \code{ala_fields("occurrence")} for valid field names.
#' @param qa string vector: (optional) list of record issues to include in the download. See \code{ala_fields("assertions")} for valid values, or use "none" to include no record issues
#' @param use_data_table logical: if TRUE, attempt to read the data.csv file using the fread function from the data.table package. Requires data.table to be available. If this fails with an error or warning, or if use_data_table is FALSE, then read.table will be used (which may be slower)
#' 
#' @return Data frame
#' 
#' @examples
#' \dontrun{ 
#' x=occurrences(taxon="macropus",fields=c("longitude","latitude","common_name","taxon_name","el807"),download_reason_id=10)
#' x=occurrences(taxon="data_resource_uid:dr356",download_reason_id=10)
#' 
#' y=occurrences(taxon="alaba vibex",fields=c("latitude","longitude","el874"),download_reason_id=10)
#' str(y)
#' # equivalent direct webservice call: http://biocache.ala.org.au/ws/occurrences/index/download?reasonTypeId=10&q=Alaba%20vibex&fields=latitude,longitude,el874&qa=none
#'
#' occurrences(taxon="Eucalyptus gunnii",fields=c("latitude","longitude"),qa="none",fq="basis_of_record:LivingSpecimen",download_reason_id=10)
#' # equivalent direct webservice call: http://biocache.ala.org.au/ws/occurrences/index/download?reasonTypeId=10&q=Eucalyptus%20gunnii&fields=latitude,longitude&qa=none&fq=basis_of_record:LivingSpecimen
#' }
#' @export occurrences


## NOTE - the column names that are returned for assertions are the full names/descriptions, do not match the requested (short) names, e.g.: y=occurrences(taxon="alaba vibex",fields=c("latitude","longitude","el874"),download_reason_id=10,qa=c("unknownCountry","homonymIssue")) gives qa columns "Supplied.country.not.recognised" and "Homonym.issues.with.supplied.name"

## undocumented: field names in "fields" and "extra" can be passed as full names (e.g. "Radiation - lowest period (Bio22)") rather than id ("el871"). I haven't documented this (yet) because it probably ought to be implemented similarly in other functions - BR

## TODO document fq alone as a query
## TODO: more extensive testing, particularly of the csv-conversion process
## TODO LATER: add params: lat, lon, radius (for specifying a search circle)


occurrences=function(taxon,wkt,fq,fields,extra,qa,download_reason_id=ala_config()$download_reason_id,reason,use_data_table=TRUE) {
    ## check input parms are sensible
    assert_that(is.flag(use_data_table))
    reason_ok=!is.na(download_reason_id)
    if (reason_ok) {
        valid_reasons=ala_reasons()
        reason_ok=download_reason_id %in% valid_reasons$id
    }
    if (! reason_ok) {
        stop("download_reason_id must be a valid reason_id. See ala_reasons(). Set this value directly here or through ala_config(download_reason_id=...)")
    }
    #taxon = clean_string(taxon) ## clean up the taxon name # no - because this can be an indexed query like field1:value1
    base_url=paste(ala_config()$base_url_biocache,"occurrences/index/download",sep="")
  
    this_query=list()
    ## have we specified a taxon?
    if (!missing(taxon)) {
        assert_that(is.string(taxon))
        this_query$q=taxon
    }
    ## wkt string
    if (!missing(wkt)) {
        assert_that(is.string(wkt))
        if (! check_wkt(wkt)) {
            stop("invalid WKT string ",wkt)
        }
        this_query$wkt=wkt
    }
    if (!missing(fq)) {
        assert_that(is.character(fq))
        ## can have multiple fq parameters, need to specify in url as fq=a:b&fq=c:d&fq=...
        check_fq(fq,type="occurrence") ## check that fq fields are valid
        fq=as.list(fq)
        names(fq)=rep("fq",length(fq))
        this_query=c(this_query,fq)
    }
    if (length(this_query)==0) {
        ## not a valid request!
        stop("invalid request: need at least one of taxon, fq, or wkt to be specified")
    }
    if (!missing(fields)) {
        assert_that(is.character(fields))
        ## user has specified some fields
        fields=fields_description_to_id(fields=fields,fields_type="occurrence") ## replace long names with ids
        valid_fields=ala_fields(fields_type="occurrence")
        unknown=setdiff(fields,valid_fields$name)
        if (length(unknown)>0) {
            stop("invalid fields requested: ", str_c(unknown,collapse=", "), ". See ala_fields(\"occurrence\")")
        }
        this_query$fields=str_c(fields,collapse=",")
    }
    if (!missing(extra)) {
        assert_that(is.character(extra))
        extra=fields_description_to_id(fields=extra,fields_type="occurrence") ## replace long names with ids
        valid_fields=ala_fields(fields_type="occurrence")
        unknown=setdiff(extra,valid_fields$name)
        if (length(unknown)>0) {
            stop("invalid extra fields requested: ", str_c(unknown,collapse=", "), ". See ala_fields(\"occurrence\")")
        }
        this_query$extra=str_c(extra,collapse=",")
    }
    if (!missing(qa)) {
        assert_that(is.character(qa))
        valid_fields=c("none",ala_fields(fields_type="assertions")$name) ## valid entries for qa
        unknown=setdiff(qa,valid_fields)
        if (length(unknown)>0) {
            stop("invalid qa fields requested: ", str_c(unknown,collapse=", "), ". See ala_fields(\"assertions\")")
        }
        this_query$qa=str_c(qa,collapse=",")
    }
    if (!missing(reason)) {
        assert_that(is.string(reason))
        this_query$reason=reason
    }
    this_query$reasonTypeId=download_reason_id
    this_query$esc="\\" ## force backslash-escaping of quotes rather than double-quote escaping
    this_query$sep="\t" ## tab-delimited
    this_query$file="data" ## to ensure that file is named "data.csv" within the zip file
    
    this_url=parse_url(base_url)
    this_url$query=this_query
  
    ## these downloads can potentially be large, so we want to download directly to file and then read the file
    thisfile=cached_get(url=build_url(this_url),type="binary_filename")
    if (!(file.info(thisfile)$size>0)) {
        ## empty file
        x=NULL
        ## actually this isn't a sufficient check, since even with empty data.csv file inside, the outer zip file will be > 0 bytes. Check again below on the actual data.csv file
    } else {
        ## if data.table is available, first try using this
        read_ok=FALSE
        if (use_data_table & is.element('data.table', installed.packages()[,1])) { ## if data.table package is available
            require(data.table) ## load it
            tryCatch({
                ## first need to extract data.csv from the zip file
                ## this may end up making fread() slower than direct read.table() ... needs testing
                tempsubdir=tempfile(pattern="dir")
                if (ala_config()$verbose) {
                    cat(sprintf(" ALA4R: unzipping downloaded occurrences data.csv file into %s\n",tempsubdir))
                }
                dir.create(tempsubdir)
                unzip(thisfile,files=c("data.csv"),junkpaths=TRUE,exdir=tempsubdir)
                ## first check if file is empty
                if (file.info(file.path(tempsubdir,"data.csv"))$size>0) {
                    x=fread(file.path(tempsubdir,"data.csv"),stringsAsFactors=FALSE,header=TRUE,verbose=ala_config()$verbose)
                    ## make sure names of x are valid, as per data.table
                    setnames(x,make.names(names(x)))
                    ## now coerce it back to data.frame (for now at least, unless we decide to not do this!)
                    x=as.data.frame(x)
                    if (!empty(x)) {
                        ## convert column data types
                        ## ALA supplies *all* values as quoted text, even numeric, and they appear here as character type
                        ## we will convert whatever looks like numeric or logical to those classes
                        x=colwise(convert_dt)(x)
                    }
                    read_ok=TRUE
                } else {
                    x=NULL
                    read_ok=TRUE
                }
            }, warning=function(e) {
                warning("ALA4R: reading of csv as data.table failed, will fall back to read.table (may be slow). The warning message was: ",e)
                read_ok=FALSE
            }
             , error=function(e) {
                warning("ALA4R: reading of csv as data.table failed, will fall back to read.table (may be slow). The error message was: ",e)
                read_ok=FALSE
            })
        }
        if (!read_ok) {
            x=read.table(unz(thisfile,filename="data.csv"),header=TRUE,comment.char="",as.is=TRUE)
            if (!empty(x)) {
                ## convert column data types
                ## read.table handles quoted numerics but not quoted logicals
                x=colwise(convert_dt)(x,test_numeric=FALSE)
            }
        }

        if (!empty(x)) {
            ## also read the citation info
            ## this file won't exist if there are no rows in the data.csv file, so only do it if nrow(x)>0
            xc=read.table(unz(thisfile,"citation.csv"),header=TRUE,comment.char="",as.is=TRUE)
        } else {
            warning("no matching records were returned")
            xc=NULL
        }
        x=list(data=x,meta=xc)
    }
    class(x) <- c('occurrences',class(x)) #add the occurrences class
    x
}

#'@S3method summary occurrences
"summary.occurrences" <- function(object, ...) {
	cat('number of species:',length(unique(object$data$Scientific.Name)),'\n')
	cat('number of taobjectonomically corrected names:',length(unique(object$data$Species...matched)),'\n')
	cat('number of observation records:',nrow(object$data),'\n')
	ass = check_assertions(object) #need to get eobjectisting assertions in occur dataset
	if (nrow(ass)>0) {
		cat('assertions checked:',nrow(ass),'\n')
		for (ii in 1:nrow(ass)) {
			rwi = length(which(as.logical(object$data[,ass$occur.colnames[ii]])==TRUE)) #count the number of records with issues
			if (rwi>0) cat('\t',ass$occur.colnames[ii],': ',rwi,' records ',ifelse(as.logical(ass$fatal[ii]),'-- considered fatal',''),sep='','\n')
		}
	} else { cat('no asserting issues\n') }
	invisible(object)
}


# internal function for converting chr data types to numeric or logical
convert_dt=function(x,test_numeric=TRUE) {
    ## set test_numeric to FALSE to skip checking for numeric columns - might be a little faster if not needed
    assert_that(is.flag(test_numeric))
    if (see_if(is.character(x))) {
        ux=unique(x)
        if (all(nchar(ux)<1)) {
            ## all empty strings - leave as is
        } else if (all(ux %in% c("true","false","TRUE","FALSE","","NA"))) {
            x=as.logical(x)
        } else if (test_numeric) {
            if (all(nchar(ux)<1 | ux=="NA" | !is.na(suppressWarnings(as.numeric(ux))))) {
                x=as.numeric(x)
            }
        }
    }
    x
}
