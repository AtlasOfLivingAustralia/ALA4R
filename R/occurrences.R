#' Get occurrence data
#' 
#' Retrieve ALA occurrence data via the "occurrence download" web service. At least one of \code{taxon}, \code{wkt}, or \code{fq} must be supplied for a valid query. Note that there is a limit of 500000 records per request when using \code{method="indexed"}. Use the \code{method="offline"} for larger requests. For small requests, \code{method="indexed"} may be faster.
#' 
#' @references \itemize{
#' \item Associated ALA web service for record counts: \url{http://api.ala.org.au/#ws3}
#' \item Associated ALA web service for occurence downloads: \url{http://api.ala.org.au/#ws4}
#' \item Field definitions: \url{https://docs.google.com/spreadsheet/ccc?key=0AjNtzhUIIHeNdHhtcFVSM09qZ3c3N3ItUnBBc09TbHc}
#' \item WKT reference: \url{http://www.geoapi.org/3.0/javadoc/org/opengis/referencing/doc-files/WKT.html}
#' }
#' @param taxon string: (optional) query of the form field:value (e.g. "genus:Macropus") or a free text search ("Alaba vibex")
#' @param wkt string: (optional) a WKT (well-known text) string providing a spatial polygon within which to search, e.g. "POLYGON((140 -37,151 -37,151 -26,140.131 -26,140 -37))"
#' @param fq string: (optional) character string or vector of strings, specifying filters to be applied to the original query. These are of the form "INDEXEDFIELD:VALUE" e.g. "kingdom:Fungi". 
#' See \code{ala_fields("occurrence_indexed",as_is=TRUE)} for all the fields that are queryable. 
#' NOTE that fq matches are case-sensitive, but sometimes the entries in the fields are 
#' not consistent in terms of case (e.g. kingdom names "Fungi" and "Plantae" but "ANIMALIA"). 
#' fq matches are ANDed by default (e.g. c("field1:abc","field2:def") will match records that have 
#' field1 value "abc" and field2 value "def"). To obtain OR behaviour, use the form c("field1:abc 
#' OR field2:def"). See e.g. \url{http://wiki.apache.org/solr/CommonQueryParameters} for more information about filter queries
#' @param fields string vector: (optional) a vector of field names to return. Note that the columns of the returned data frame 
#' are not guaranteed to retain the ordering of the field names given here. If not specified, a default list of fields will be returned. See \code{ala_fields("occurrence_stored")} for valid field names with method \code{indexed}, and \code{ala_fields("occurrence")} for valid field names with method \code{offline}. Field names can be passed as full names (e.g. "Radiation - lowest period (Bio22)") rather than id ("el871"). Use \code{fields="all"} to include all available fields, but note that \code{"all"} will probably cause an error with \code{method="offline"} because the request URL will exceed the maximum allowable length
#' @param extra string vector: (optional) a vector of field names to include in addition to those specified in \code{fields}. This is useful if you would like the default list of fields (i.e. when \code{fields} parameter is not specified) plus some additional extras. See \code{ala_fields("occurrence_stored",as_is=TRUE)} for valid field names. Field names can be passed as full names (e.g. "Radiation - lowest period (Bio22)") rather than id ("el871"). Use \code{extra="all"} to include all available fields, but note that \code{"all"} will probably cause an error with \code{method="offline"} because the request URL will exceed the maximum allowable length
#' @param qa string vector: (optional) list of record issues to include in the download. Use \code{qa="all"} to include all available issues, or \code{qa="none"} to include none. Otherwise see \code{ala_fields("assertions",as_is=TRUE)} for valid values
#' @param method string: "indexed" (default) or "offline". In "offline" mode, more fields are available and larger datasets can be returned
#' @param email string: the email address of the user performing the download (required for \code{method="offline"}
#' @param download_reason_id numeric or string: (required unless record_count_only is TRUE) a reason code for the download, either as a numeric ID (currently 0--11) or a string (see \code{\link{ala_reasons}} for a list of valid ID codes and names). The download_reason_id can be passed directly to this function, or alternatively set using \code{ala_config(download_reason_id=...)}
#' @param reason string: (optional) user-supplied description of the reason for the download. Providing this information is optional but will help the ALA to better support users by building a better understanding of user communities and their data requests
#' @param verbose logical: show additional progress information? [default is set by ala_config()]
#' @param record_count_only logical: if TRUE, return just the count of records that would be downloaded, but don't download them. Note that the record count is always re-retrieved from the ALA, regardless of the caching settings. If a cached copy of this query exists on the local machine, the actual data set size may therefore differ from this record count. \code{record_count_only=TRUE} can only be used with \code{method="indexed"}
#' @param use_layer_names logical: if TRUE, layer names will be used as layer column names in the returned data frame (e.g. "radiationLowestPeriodBio22"). Otherwise, layer id value will be used for layer column names (e.g. "el871")
#' @param use_data_table logical: if TRUE, attempt to read the data.csv file using the fread function from the data.table package. Requires data.table to be available. If this fails with an error or warning, or if use_data_table is FALSE, then read.table will be used (which may be slower)
#' 
#' @return Data frame of occurrence results, with one row per occurrence record. The columns of the dataframe will depend on the requested fields
#' @seealso \code{\link{ala_reasons}} for download reasons; \code{\link{ala_config}}
#' @examples
#' \dontrun{
#' ## count of records from this data provider
#' x <- occurrences(taxon="data_resource_uid:dr356",record_count_only=TRUE)
#' ## download records, with standard fields
#' x <- occurrences(taxon="data_resource_uid:dr356",download_reason_id=10)
#' ## download records, with all fields
#' x <- occurrences(taxon="data_resource_uid:dr356",download_reason_id=10,
#'   fields=ala_fields("occurrence_stored",as_is=TRUE)$name) 
#' ## download records, with specified fields
#' x <- occurrences(taxon="macropus",fields=c("longitude","latitude","common_name",
#'   "taxon_name","el807"),download_reason_id=10)
#'  ## download records in polygon, with no quality assertion information
#' x <- occurrences(taxon="macropus",
#'   wkt="POLYGON((145 -37,150 -37,150 -30,145 -30,145 -37))",
#'   download_reason_id=10,qa="none")
#' 
#' y <- occurrences(taxon="alaba vibex",fields=c("latitude","longitude","el874"),download_reason_id=10)
#' str(y)
#' # equivalent direct webservice call:
#' # http://biocache.ala.org.au/ws/occurrences/index/download?reasonTypeId=10&q=Alaba%20vibex&
#' #    fields=latitude,longitude,el874&qa=none
#'
#' occurrences(taxon="Eucalyptus gunnii",fields=c("latitude","longitude"),
#'   qa="none",fq="basis_of_record:LivingSpecimen",download_reason_id=10)
#' # equivalent direct webservice call:
#' # http://biocache.ala.org.au/ws/occurrences/index/download?reasonTypeId=10&q=Eucalyptus%20gunnii&
#' #    fields=latitude,longitude&qa=none&fq=basis_of_record:LivingSpecimen
#' }
#' @export occurrences

## TODO: more extensive testing, particularly of the csv-conversion process
## TODO LATER: add params: lat, lon, radius (for specifying a search circle)

occurrences <- function(taxon,wkt,fq,fields,extra,qa,method="indexed",email,download_reason_id=ala_config()$download_reason_id,reason,verbose=ala_config()$verbose,record_count_only=FALSE,use_layer_names=TRUE,use_data_table=TRUE) {
    ## check input parms are sensible
    assert_that(is.flag(record_count_only))    
    assert_that(is.string(method))
    method <- match.arg(tolower(method),c("offline","indexed"))
    valid_fields_type <- if (method=="indexed") "occurrence_stored" else "occurrence"
    this_query <- list()
    ## have we specified a taxon?
    if (!missing(taxon)) {
        if (is.factor(taxon)) {
            taxon <- as.character(taxon)
        }
        assert_that(is.notempty.string(taxon))
        this_query$q <- taxon
    }
    ## wkt string
    if (!missing(wkt)) {
        assert_that(is.notempty.string(wkt))
        this_query$wkt <- wkt
    }
    if (!missing(fq)) {
        assert_that(is.character(fq))
        ## can have multiple fq parameters, need to specify in url as fq=a:b&fq=c:d&fq=...
        check_fq(fq,type="occurrence") ## check that fq fields are valid
        fq <- as.list(fq)
        names(fq) <- rep("fq",length(fq))
        this_query <- c(this_query,fq)
    }
    if (length(this_query)==0) {
        ## not a valid request!
        stop("invalid request: need at least one of taxon, fq, or wkt to be specified")
    }
    if (method=="offline") {
        if (record_count_only) stop("record_count_only can only be used with method=\"indexed\"")
        if (missing(email) || !is.string(email) || nchar(email)<1) stop("email is required for method=offline")
    }
    ## check the number of records
    if (record_count_only) {
        ## check using e.g. http://biocache.ala.org.au/ws/occurrences/search?q=*:*&pageSize=0&facet=off
        temp_query <- this_query
        temp_query$pageSize <- 0
        temp_query$facet <- "off"
        this_url <- build_url_from_parts(getOption("ALA4R_server_config")$base_url_biocache,c("occurrences","search"),query=temp_query)
        # ## don't need to check number of records if caching is on and we already have the file
        # cache_file_exists=file.exists(ala_cache_filename(this_url))
        # if ((ala_config()$caching %in% c("off","refresh")) | (!cache_file_exists & ala_config()$caching=="on")) {
            ## check
        #    num_records=cached_get(url=this_url,type="json")$totalRecords
        #    cat(sprintf('occurrences: downloading dataset with %d records',num_records))
        #}
        return(cached_get(url=this_url,type="json",caching="off",verbose=verbose)$totalRecords)
    }
    assert_that(is.flag(use_data_table))
    assert_that(is.flag(use_layer_names))
    reason_ok <- !is.na(download_reason_id)
    if (reason_ok) {
        valid_reasons <- ala_reasons()
        download_reason_id <- convert_reason(download_reason_id) ## convert from string to numeric if needed
        reason_ok <- download_reason_id %in% valid_reasons$id
    }
    if (! reason_ok) {
        stop("download_reason_id must be a valid reason_id. See ",getOption("ALA4R_server_config")$reasons_function,"()")
    }
    if (!missing(fields)) {
        assert_that(is.character(fields))
        ## user has specified some fields
        valid_fields <- ala_fields(fields_type=valid_fields_type,as_is=TRUE)
        if (identical(tolower(fields),"all")) fields <- valid_fields$name
        fields <- fields_name_to_id(fields=fields,fields_type="occurrence") ## replace long names with ids
        unknown <- setdiff(fields,valid_fields$name)
        if (length(unknown)>0) {
            stop("invalid fields requested: ", str_c(unknown,collapse=", "), ". See ",getOption("ALA4R_server_config")$fields_function,"(\"",valid_fields_type,"\",as_is=TRUE)")
        }
        this_query$fields <- str_c(fields,collapse=",")
    }
    if (!missing(extra)) {
        assert_that(is.character(extra))
        valid_fields <- ala_fields(fields_type=valid_fields_type,as_is=TRUE)
        if (identical(tolower(extra),"all")) extra <- valid_fields$name
        extra <- fields_name_to_id(fields=extra,fields_type="occurrence") ## replace long names with ids
        unknown <- setdiff(extra,valid_fields$name)
        if (length(unknown)>0) {
            stop("invalid extra fields requested: ", str_c(unknown,collapse=", "), ". See ",getOption("ALA4R_server_config")$fields_function,"(\"",valid_fields_type,"\",as_is=TRUE)")
        }
        this_query$extra <- str_c(extra,collapse=",")
    }
    if (!missing(qa)) {
        assert_that(is.character(qa))
        if (identical(tolower(qa),"all")) { qa <- ala_fields("assertions",as_is=TRUE)$name }
        valid_fields <- c("none",ala_fields(fields_type="assertions",as_is=TRUE)$name) ## valid entries for qa
        unknown <- setdiff(qa,valid_fields)
        if (length(unknown)>0) {
            stop("invalid qa fields requested: ", str_c(unknown,collapse=", "), ". See ",getOption("ALA4R_server_config")$fields_function,"(\"assertions\",as_is=TRUE)")
        }
        this_query$qa <- str_c(qa,collapse=",")
    }
    if (!missing(reason)) {
        assert_that(is.string(reason))
        this_query$reason <- reason
    }
    if (method=="offline")
        this_query$email <- email
    this_query$reasonTypeId <- download_reason_id
    this_query$sourceTypeId <- ala_sourcetypeid()
    this_query$esc <- "\\" ## force backslash-escaping of quotes rather than double-quote escaping
    this_query$sep <- "\t" ## tab-delimited
    this_query$file <- "data" ## to ensure that file is named "data.csv" within the zip file

    if (method=="indexed")
        this_url <- build_url_from_parts(getOption("ALA4R_server_config")$base_url_biocache,c("occurrences","index","download"),query=this_query)
    else
        this_url <- build_url_from_parts(getOption("ALA4R_server_config")$base_url_biocache,c("occurrences","offline","download"),query=this_query)

    if (method=="offline") {
        thisfile <- ala_cache_filename(this_url) ## the file that will ultimately hold the results (even if we are not caching, it still gets saved to file)
        if ((ala_config()$caching %in% c("off","refresh")) || (! file.exists(thisfile))) {
            status <- cached_get(url=this_url,caching="off",type="json",verbose=verbose)
            if (!"statusUrl" %in% names(status)) stop("reply from server was missing statusUrl. ",getOption("ALA4R_server_config")$notify)
            status <- cached_get(status$statusUrl,caching="off",type="json",verbose=verbose)
            while (tolower(status$status) %in% c("inqueue","running")) {##!= "finished") {
                status <- cached_get(status$statusUrl,caching="off",type="json",verbose=verbose)
                Sys.sleep(2)
            }
            if (status$status!="finished") {
                stop("unexpected response from server. ",getOption("ALA4R_server_config")$notify,". Response was:\n",str(status))
            } else {
                ## finally we have the URL to the data file itself
                download_to_file(status$downloadUrl,outfile=thisfile,binary_file=TRUE,verbose=verbose)
            }
        } else {
            ## we are using the existing cached file
            if (verbose) { cat(sprintf("  using cached file %s for %s\n",thisfile,this_url)) }
        }
    } else {
        thisfile <- cached_get(url=this_url,type="binary_filename",verbose=verbose)
    }
    ## these downloads can potentially be large, so we want to download directly to file and then read the file
    if (!(file.info(thisfile)$size>0)) {
        ## empty file
        x <- NULL
        ## actually this isn't a sufficient check, since even with empty data.csv file inside, the outer zip file will be > 0 bytes. Check again below on the actual data.csv file
    } else {
        ## if data.table is available, first try using this
        read_ok <- FALSE
        if (use_data_table & requireNamespace("data.table",quietly=TRUE)) { ## if data.table package is available
            tryCatch({
                ## first need to extract data.csv from the zip file
                ## this may end up making fread() slower than direct read.table() ... needs testing
                tempsubdir <- tempfile(pattern="dir")
                if (verbose) {
                    cat(sprintf(" unzipping downloaded occurrences data.csv file into %s\n",tempsubdir))
                }
                dir.create(tempsubdir)
                unzip(thisfile,files=c("data.csv"),junkpaths=TRUE,exdir=tempsubdir)
                ## first check if file is empty
                if (file.info(file.path(tempsubdir,"data.csv"))$size>0) {
                    x <- data.table::fread(file.path(tempsubdir,"data.csv"),data.table=FALSE,stringsAsFactors=FALSE,header=TRUE,verbose=verbose,sep="\t")
                    names(x) <- make.names(names(x))
                    if (!empty(x)) {
                        ## convert column data types
                        ## ALA supplies *all* values as quoted text, even numeric, and they appear here as character type
                        ## we will convert whatever looks like numeric or logical to those classes
                        x <- colwise(convert_dt)(x)
                    }
                    read_ok <- TRUE
                } else {
                    x <- data.frame() ## empty result set
                    read_ok <- TRUE
                }
            }, warning=function(e) {
                if (verbose) {
                    warning("reading of csv as data.table failed, will fall back to read.table (may be slow). The warning message was: ",e)
                }
                read_ok <- FALSE
            }
             , error=function(e) {
                if (verbose) {
                    warning("reading of csv as data.table failed, will fall back to read.table (may be slow). The error message was: ",e)
                }
                read_ok <- FALSE
            })
        }
        if (!read_ok) {
            x <- read.table(unz(thisfile,filename="data.csv"),header=TRUE,comment.char="",as.is=TRUE)
            if (!empty(x)) {
                ## convert column data types
                ## read.table handles quoted numerics but not quoted logicals
                x <- colwise(convert_dt)(x,test_numeric=FALSE)
            }
        }

        if (!empty(x)) {
            if (method=="indexed") {
                max_records <- getOption("ALA4R_server_config")$max_occurrence_records
                if (nrow(x)>0.99*max_records) {
                    warning(nrow(x)," data rows were returned from the server, which is close to the maximum allowed. This might not be the full data set you wanted --- consider using method=\"offline\"")
                }
            }
            names(x) <- str_replace_all(names(x),"^(el|cl)\\.([0-9]+)","\\1\\2") ## change e.g. el.xxx to elxxx
            ## TODO what is "cl.1050.b" etc?
            if (use_layer_names) {
                names(x) <- make.names(fields_id_to_name(names(x),fields_type="layers"))
            } else {
                names(x) <- make.names(fields_name_to_id(names(x),fields_type="layers",make_names=TRUE)) ## use make_names because names here have dots instead of spaces (not tested)
            }
            names(x) <- rename_variables(names(x),type="assertions")
            names(x) <- rename_variables(names(x),type="occurrence")
            ## remove unwanted columns
            xcols <- setdiff(names(x),unwanted_columns("occurrence"))
            x <- subset(x,select=xcols)
            ## also read the citation info
            ## this file won't exist if there are no rows in the data.csv file, so only do it if nrow(x)>0
            ## also wrap it in a try(...), so that it won't cause the function to fail if the citation.csv file isn't present
            xc <- "No citation information was returned, try again later"
            found_citation <- FALSE
            try({
                suppressWarnings(xc <- read.table(unz(thisfile,"citation.csv"),header=TRUE,comment.char="",as.is=TRUE))
                found_citation <- TRUE},
                silent=TRUE)
            if (!found_citation) {
                ## as of around July 2016 the citation.csv file appears to have been replaced by README.html
                try({
                    suppressWarnings(xc <- scan(unz(thisfile,"README.html"),what="character",sep="$",quiet=TRUE))
                    xc <- data.frame(citation=paste(xc,collapse=""))
                    found_citation <- TRUE},
                    silent=TRUE)
            }
            if (!found_citation & nrow(x)>0) warning("citation file not found within downloaded zip file")
        } else {
            if (ala_config()$warn_on_empty) {
                warning("no matching records were returned")
            }
            if (!missing(wkt) && !isTRUE(check_wkt(wkt))) warning("WKT string may not be valid: ",wkt)
            xc <- NULL
        }
        x <- list(data=x,meta=xc)
    }
    class(x) <- c('occurrences',class(x)) #add the occurrences class
    x
}
