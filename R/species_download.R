#' Download taxonomic data
#' 
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/}
#' 
#' @param query string: (optional) query of the form field:value (e.g. "genus:Macropus") or a free text search ("Alaba vibex")
#' @param fq string: character string or vector of strings, specifying filters to be applied to the original query. These are of the form "INDEXEDFIELD:VALUE" e.g. "kingdom:Fungi". See \code{ala_fields("occurrence")} for all the fields that are queryable. NOTE that fq matches are case-sensitive, but sometimes the entries in the fields are not consistent in terms of case (e.g. kingdom names "Fungi" and "Plantae" but "ANIMALIA"). fq matches are ANDed by default (e.g. c("field1:abc","field2:def") will match records that have field1 value "abc" and field2 value "def"). To obtain OR behaviour, use the form c("field1:abc OR field2:def")
#' @param fields string vector: (optional) a vector of field names to return. Note that the columns of the returned data frame 
#' are not guaranteed to retain the ordering of the field names given here. If not specified, a default list of fields will be returned. See \code{ala_fields("general")} for valid field names.
#' @param verbose logical: how much progress information to display; default is set by \code{ala_config()}
#' @return data frame
#' @seealso \code{\link{ala_fields}}
#' @examples
#' # Download data for Fabaceae
#' x=species_download("family:Fabaceae",fields=c("guid","parentGuid","kingdom","phylum","class","bioOrder","family","genus","scientificName"))
#' # equivalent direct URL: http://bie.ala.org.au/ws/download?fields=guid,parentGuid,kingdom,phylum,class,order,family,genus,scientificName&q=family:Fabaceae
#' @export
species_download=function(query,fq,fields) {
    base_url=paste(ala_config()$base_url_bie,"download",sep="")
    this_query=list()
    ## have we specified a query?
    if (!missing(query)) {
        assert_that(is.string(query))
        this_query$q=query
    }
    if (length(this_query)==0) {
        ## not a valid request!
        stop("invalid request: query must be specified")
    }
    if (!missing(fq)) {
        assert_that(is.character(fq))
        ## can have multiple fq parameters, need to specify in url as fq=a:b&fq=c:d&fq=...
        check_fq(fq,type="general") ## check that fq fields are valid
        fq=as.list(fq)
        names(fq)=rep("fq",length(fq))
        this_query=c(this_query,fq)
    }
    if (!missing(fields)) {
        assert_that(is.character(fields))
        ## user has specified some fields
        fields=fields_description_to_id(fields=fields,fields_type="general") ## replace long names with ids
        valid_fields=ala_fields(fields_type="general")
        unknown=setdiff(fields,valid_fields$name)
        if (length(unknown)>0) {
            stop("invalid fields requested: ", str_c(unknown,collapse=", "), ". See ala_fields(\"general\")")
        }
        this_query$fields=str_c(fields,collapse=",")
    }
    
    this_url=parse_url(base_url)
    this_url$query=this_query
    
    ## these downloads can potentially be large, so we want to download directly to file and then read the file
    thisfile=cached_get(url=build_url(this_url),type="binary_filename")

    if (!(file.info(thisfile)$size>0)) {
        ## empty file
        x=NULL
    } else {
        ## if data.table is available, first try using this
        read_ok=FALSE
        if (is.element('data.table', installed.packages()[,1])) { ## if data.table package is available
            require(data.table) ## load it
            tryCatch({
                x=fread(thisfile,stringsAsFactors=FALSE,header=TRUE,verbose=ala_config()$verbose)
                ## make sure names of x are valid, as per data.table
                setnames(x,make.names(names(x)))
                ## now coerce it back to data.frame (for now at least, unless we decide to not do this!)
                x=as.data.frame(x)
                read_ok=TRUE
            }, error=function(e) {
                warning("ALA4R: reading of csv as data.table failed, will fall back to read.table (may be slow). The error message was: ",e)
                read_ok=FALSE
            })
        }
        if (!read_ok) {
            x=read.table(thisfile,header=TRUE,comment.char="",as.is=TRUE)
        }

        if (!empty(x)) {
            ## make sure logical columns are actually of type logical
            logicals=which(as.vector(colSums(x=="true"|x=="false")==nrow(x))) ## get the logical columns
            x[,logicals]=apply(x[,logicals],2,function(x) as.logical(x)) ## convert columns to logical            
        } else {
            warning("no matching records were returned")
        }
    }
    #class(x) <- c('species_download',class(x)) #add the custom class
    x
}

#' @export
#"summary.occurrences" <- function(object,...) {
#	cat('number of species:',length(unique(object$data$Scientific.Name)),'\n')
#	cat('number of taobjectonomically corrected names:',length(unique(object$data$Species...matched)),'\n')
#	cat('number of observation records:',nrow(object$data),'\n')
#	ass = check_assertions(object) #need to get eobjectisting assertions in occur dataset
#	if (nrow(ass)>0) {
#		cat('assertions checked:',nrow(ass),'\n')
#		for (ii in 1:nrow(ass)) {
#			rwi = length(which(as.logical(object$data[,ass$occur.colnames[ii]])==TRUE)) #count the number of records with issues
#			if (rwi>0) cat('\t',ass$occur.colnames[ii],': ',rwi,' records ',ifelse(as.logical(ass$fatal[ii]),'-- considered fatal',''),sep='','\n')
#		}
#	} else { cat('no asserting issues\n') }
#	invisible(object)
#}
    
    
