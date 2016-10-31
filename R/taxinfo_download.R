#' Download taxonomic data
#' 
#' @references Associated ALA web service: \url{http://api.ala.org.au/#ws2}
#' 
#' @param query string: (optional) query of the form field:value (e.g. "genus:Macropus") or a free text search ("Alaba vibex")
#' @param fq string: character string or vector of strings, specifying filters to be applied to the original query. 
#' These are of the form "INDEXEDFIELD:VALUE" e.g. "kingdom:Fungi". See \code{ala_fields("general",as_is=TRUE)} for all the 
#' fields that are queryable. NOTE that fq matches are case-sensitive, but sometimes the entries in the fields are 
#' not consistent in terms of case (e.g. kingdom names "Fungi" and "Plantae" but "ANIMALIA"). fq matches are ANDed 
#' by default (e.g. c("field1:abc","field2:def") will match records that have field1 value "abc" and field2 value "def"). 
#' To obtain OR behaviour, use the form c("field1:abc OR field2:def")
#' @param fields string vector: (optional) a vector of field names to return. Note that the columns of the returned data 
#' frame are not guaranteed to retain the ordering of the field names given here. If not specified, a default list of 
#' fields will be returned. See \code{ala_fields("general",as_is=TRUE)} for valid field names. Use \code{fields="all"} to include all available fields
#' @param verbose logical: show additional progress information? [default is set by ala_config()]
#' @param use_data_table logical: if TRUE, attempt to read the data.csv file using the fread function from the 
#' data.table package. Requires data.table to be available. If this fails, or use_data_table is FALSE, then read.table 
#' will be used (which may be slower)
#' @return data frame of results, containing one row per taxon, typically with name, guid, and taxonomic information. The columns returned will depend on the field requested
#' @seealso \code{\link{ala_fields}}, \code{\link{ala_config}}
#' @examples
#' \dontrun{
#' ## simplest usage
#' x <- taxinfo_download("rk_genus:Macropus")
#' 
#' ## Data for Fabaceae with specified fields
#' x <- taxinfo_download("rk_family:Fabaceae",fields=c("guid","parentGuid","rk_kingdom","rk_phylum",
#'   "rk_class","rk_order","rk_family","rk_genus","scientificName"))
#' # equivalent direct URL: http://bie.ala.org.au/ws/download?fields=guid,parentGuid,rk_kingdom,
#' #    rk_phylum,rk_class,rk_order,rk_family,rk_genus,scientificName&q=rk_family:Fabaceae
#' }
#' @export taxinfo_download

taxinfo_download <- function(query,fq,fields,verbose=ala_config()$verbose,use_data_table=TRUE) {
    assert_that(is.flag(use_data_table))
    this_query <- list()
    ## have we specified a query?
    if (!missing(query)) {
        if (is.factor(query)) {
            query <- as.character(query)
        }
        assert_that(is.notempty.string(query))
        this_query$q <- query
    }
    if (length(this_query)==0) {
        ## not a valid request!
        stop("invalid request: query must be specified")
    }
    if (!missing(fq)) {
        assert_that(is.character(fq))
        ## can have multiple fq parameters, need to specify in url as fq=a:b&fq=c:d&fq=...
        check_fq(fq,type="general") ## check that fq fields are valid
        fq <- as.list(fq)
        names(fq) <- rep("fq",length(fq))
        this_query <- c(this_query,fq)
    }
    if (!missing(fields)) {
        assert_that(is.character(fields))
        ## user has specified some fields
        valid_fields <- ala_fields(fields_type="general",as_is=TRUE)
        if (identical(tolower(fields),"all")) fields <- valid_fields$name
        unknown <- setdiff(fields,valid_fields$name)
        if (length(unknown)>0) {
            stop("invalid fields requested: ", str_c(unknown,collapse=", "), ". See ",getOption("ALA4R_server_config")$fields_function,"(\"general\",as_is=TRUE)")
        }
        this_query$fields <- str_c(fields,collapse=",")
    }
    
    this_url <- build_url_from_parts(getOption("ALA4R_server_config")$base_url_bie,"download",this_query)
    
    ## these downloads can potentially be large, so we want to download directly to file and then read the file
    thisfile <- cached_get(url=this_url,type="binary_filename",verbose=verbose)

    if (!(file.info(thisfile)$size>0)) {
        ## empty file
        x <- NULL
    } else {
        ## if data.table is available, first try using this
        read_ok <- FALSE
        if (use_data_table & requireNamespace("data.table",quietly=TRUE)) { ## if data.table package is available
            tryCatch({
                x <- data.table::fread(thisfile,data.table=FALSE,stringsAsFactors=FALSE,header=TRUE,verbose=verbose)
                names(x) <- make.names(names(x))
                if (!empty(x)) {
                    ## convert column data types
                    ## ALA supplies *all* values as quoted text, even numeric, and they appear here as character type
                    ## we will convert whatever looks like numeric or logical to those classes
                    x <- colwise(convert_dt)(x)
                }
                read_ok <- TRUE
            }, error=function(e) {
                warning("reading of csv as data.table failed, will fall back to read.table (may be slow). The error message was: ",e)
                read_ok <- FALSE
            })
        }
        if (!read_ok) {
            x <- read.table(thisfile,sep=",",header=TRUE,comment.char="",as.is=TRUE)
            if (!empty(x)) {
                ## convert column data types
                ## read.table handles quoted numerics but not quoted logicals
                x <- colwise(convert_dt)(x,test_numeric=FALSE)
            }
        }

        if (empty(x)) {
            if (ala_config()$warn_on_empty) {
                warning("no matching records were returned")
            }
        }
        ## apply column renaming, even if data.frame is empty
        xcols <- setdiff(names(x),unwanted_columns(type="general"))
        x <- subset(x,select=xcols)
        names(x) <- rename_variables(names(x),type="general")
        names(x)[tolower(names(x))=="taxonid"] <- "guid" ## guid is called taxonid
    }
    #class(x) <- c('taxinfo_download',class(x)) #add the custom class
    x
}
