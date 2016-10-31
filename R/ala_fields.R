#' Retrieves a list of all field names that can be used with data retrieval functions
#'
#' Note for occurrence fields: only fields that are indexed in the ALA database can be queried (e.g. used in the \code{fq} parameter in \code{\link{occurrences}}. These fields are identified by the \code{indexed} column in \code{ala_fields("occurrence")}. Only fields that are stored in the database can be returned as part of an \code{occurrences} call. These fields are identified by the \code{stored} column in \code{ala_fields("occurrence")}. The calling syntaxes \code{ala_fields("occurrence_stored")} and \code{ala_fields("occurrence_indexed")} are for convenience, and are equivalent to \code{subset(ala_fields("occurrence"),stored)} and \code{subset(ala_fields("occurrence"),indexed)}.
#' 
#' @references Relevant ALA web services: \itemize{
#' \item for fields_type "occurrence": http://api.ala.org.au/#ws72
#' \item for fields_type "general": http://api.ala.org.au/#ws88
#' \item for fields_type "layers": http://api.ala.org.au/#ws11 (see also descriptions of the spatial layers: \url{http://spatial.ala.org.au/layers/})
#' \item for fields_type "assertions": http://api.ala.org.au/#ws81
#' }
#' @seealso \code{\link{search_layers}} to search for spatial layers
#' @param fields_type text: one of the following
#' \itemize{
#' \item "general" - for searching taxa, datasets, layers, and collections metadata
#' \item "occurrence" - for species occurrence records
#' \item "occurrence_stored" - can be returned as part of a species occurrence record search (equivalant to \code{subset(ala_fields("occurrences"),stored)})
#' \item "occurrence_indexed" - can be queried as part of a species occurrence record search (equivalant to \code{subset(ala_fields("occurrences"),indexed)})
#' \item "layers" - fields associated with the environmental and contextual layers. For additional information 
#' on layers, including metadata and licensing, see \code{\link{search_layers}}
#' \item "assertions" - potential issues flagged on one or more occurrence record fields
#' }
#' @param as_is logical: if TRUE, leave the field names as they are returned from the ALA web services. Arguments that are passed
#' directly to the ALA's web services (e.g. parameter \code{fq} in \code{\link{occurrences}}) should use field names in this format. If \code{as_is} is FALSE, the returned $names entries will be modified to make them consistent with the corresponding column names in R data.frames returned by e.g. \code{\link{occurrences}}. \code{as_is=FALSE} has no effect when \code{fields_type} is "layers". Note that prior to v1.20, \code{as_is=FALSE} did not work correctly. 
#' @param field_id text: id of environmental/contextual layer field for which to look up information
#' Prepend "el" for "environmental" (gridded) layers and "cl" for "contextual" (polygonal) layers
#' @param maxrows integer: maximum number of records to download. Some contextual layers (those with \code{field_id}s starting with "cl") have a very large number of records and attempting to download the full set can cause R to crash. Specifying -1 for maxrows will download the full set of records for that field
#' @param record_count_only logical: if TRUE, return just the count of records that would be downloaded, but don't download them. This really only makes sense for contextual layers, because environmental layers have only one record per layer
#' @return If \code{record_count_only} is TRUE, the number of records is returned as numeric. Otherwise, a data frame containing the field name and various attributes; an empty data frame is returned if no match is found
#' @examples
#' \dontrun{
#'  l <- ala_fields("layers")
#'  l[,4]
#'  o <- ala_fields("occurrence")
#'  o[1:13,]
#'  a <- ala_fields("assertions")
#'  a$description
#'  field_info("cl22")
#'  field_info("el773")
#' }
#' @export ala_fields

# TODO: Summary of #fields returned
# ids from http://spatial.ala.org.au/ws/layers are NUMERIC but lookup prepends "el" and "cl"! 

ala_fields <- function(fields_type="occurrence",as_is=TRUE) {
    assert_that(is.string(fields_type))
    assert_that(is.flag(as_is))
    fields_type <- match.arg(tolower(fields_type),c("occurrence","occurrence_stored","occurrence_indexed","general","layers","assertions"))
    this_url <- switch(fields_type,
           "general"={
               build_url_from_parts(getOption("ALA4R_server_config")$base_url_bie,c("admin","indexFields"))
           },
           "occurrence_indexed"=,
           "occurrence_stored"=,
           "occurrence"={
               build_url_from_parts(getOption("ALA4R_server_config")$base_url_biocache,c("index","fields"))
           },
           "layers"={
               build_url_from_parts(getOption("ALA4R_server_config")$base_url_spatial,"fields")
           },
           "assertions"={
               build_url_from_parts(getOption("ALA4R_server_config")$base_url_biocache,c("assertions","codes"))
           }
           )
    
    x <- cached_get(this_url,type="json")
    ## we have a list of unwanted columns that get removed from results
    ## since this function returns a list of field names, also remove the unwanted fields from the results list
    x <- x[!x$name %in% unwanted_columns(fields_type),]
    
    ## for "layers", shorter, more manageable names are provided from http://spatial.ala.org.au/ws/layers in API. Add these as an extra column: shortName
    if (identical(fields_type,"layers")) {
        more_x <- cached_get(url=build_url_from_parts(getOption("ALA4R_server_config")$base_url_spatial,"layers"),type="json")
        ## just pull out the bits that we want and construct ids here that match the field names in x
        more_x$id <- paste(substr(tolower(more_x$type),1,1),"l",more_x$id,sep="")
        more_x <- more_x[,c("name","id")]
        names(more_x) <- c("shortName","id")
        x <- merge(x,more_x,by="id")
        x$type[x$type=="c"] <- "Contextual"
        x$type[x$type=="b"] <- "Contextual" ## there is an errant "b" here that should be "c"
        x$type[x$type=="e"] <- "Environmental" ## for consistency with search_layers
    } else if (identical(fields_type,"occurrence_stored")) {
        x <- x[x$stored,]
    } else if (identical(fields_type,"occurrence_indexed")) {
        x <- x[x$indexed,]
    }
    if (!as_is) {
        ## old code ## names(x) <- rename_variables(names(x),type=fields_type)
        ## Nooooo! this should have been applied to x$name not names(x)
        ## as_is now defaults to TRUE (v1.20) to keep default behaviour the same as with previous versions
        if (! fields_type %in% c("layers")) {
            ## don't apply when fields_type is layers, because we want name left as full name 
            x$name <- rename_variables(x$name,type=fields_type)
        }
    }
    ## some other hard-coded name changes
    names(x)[tolower(names(x))=="desc"] <- "description"
    ## drop unwanted columns
    xcols <- setdiff(names(x),unwanted_columns(fields_type))
    x[,xcols]
}


#' @rdname ala_fields
#' @export
field_info  <-  function(field_id,maxrows=50,record_count_only=FALSE) {
    assert_that(is.notempty.string(field_id))
    assert_that(is.count(maxrows) || maxrows==-1)
    assert_that(is.flag(record_count_only))
    if (record_count_only) {
        ## override maxrows setting
        maxrows <- 0
    }
    field_id <- fields_name_to_id(fields=field_id,fields_type="layers")

    this_url <- build_url_from_parts(getOption("ALA4R_server_config")$base_url_spatial,c("field",field_id),query=list(pageSize=maxrows))
    out  <-  cached_get(url=this_url,type="json") ## retrieve a max of 50 objects by default
    if (is.null(out)) {
        ## un-matched field name, return an empty data frame
        if (ala_config()$warn_on_empty) {
            warning("No information returned. Please check field_id is valid using ",getOption("ALA4R_server_config")$fields_function,"(\"layers\").")
        }
        data.frame()
    } else {
        if (substr(field_id,1,2) == 'cl') {
            if (record_count_only) {
                return(out$number_of_objects)
            } else if (nrow(out$objects)<out$number_of_objects) {
                ## we retrieved a subset of the full record set for this field, so let the user know
                warning("field_info retrieved ",nrow(out$objects)," rows out of a total of ",out$number_of_objects," for this field. You may wish to increase the maxrows parameter, but be aware that a very large number of rows may crash R")
            }
            out  <-  out$objects #keep only the content
        } else if (substr(field_id,1,2) == 'el') {
            if (record_count_only) {
                ## user has asked for number of rows, which is always 1 for el layers
                return(1)
            }
            out  <-  as.data.frame(rbind(out)) #bind the data as a dataframe	
            rownames(out)  <-  NULL #reset the row names
        }
        names(out) <- rename_variables(names(out),type="layers")
        out
    }
}	



## private function to replace any full field names (descriptions) with their id values
## e.g. "Radiation - lowest period (Bio22)" to id "el871"
fields_name_to_id <- function(fields,fields_type,make_names=FALSE) {
    assert_that(is.character(fields))
    assert_that(is.string(fields_type))
    assert_that(is.flag(make_names)) ## if TRUE, apply make.names to variable names before matching
    fields_type <- match.arg(tolower(fields_type),c("occurrence","general","layers","assertions"))
    valid_fields <- ala_fields(fields_type=fields_type)
    ## merge differently for "layers" fields, because those column names differ from other fields_type
    ## for layers, the long name is in "desc", with the id in "id" (and "name" is something different)
    ## ** as of 17-Jun-2014, "desc" is now renamed "description"
    ## for "occurrence" and "assertions", long name is in "description" and id is in "name"
    ## for general, there is no long name (description)
    ## for each one, warn if multiple matches on long name are found
    if (make_names) {
        valid_fields$description <- switch(fields_type,
               "layers"=,
               "occurrence"=,
               "assertions"=make.names(valid_fields$description)
           )
    }
    switch(fields_type,
           "layers"=laply(fields,function(z) ifelse(z %in% valid_fields$description & ! z %in% valid_fields$id,{
               if (sum(valid_fields$description==z,na.rm=TRUE)>1) {
                   if (nchar(z)>0) { ## don't warn if field name is degenerate ""
                       warning(" multiple ",fields_type," fields match the name \"",z,"\", using first")
                   }
               }
               valid_fields$id[which(valid_fields$description==z)[1]]
           },z)),
           "occurrence"=,
           "assertions"=laply(fields,function(z) ifelse(z %in% valid_fields$description & ! z %in% valid_fields$name,{
               if (sum(valid_fields$description==z,na.rm=TRUE)>1) {
                   if (nchar(z)>0) {
                       warning(" multiple ",fields_type," fields match the name \"",z,"\", using first")
                   }
               }
               valid_fields$name[which(valid_fields$description==z)[1]]
           },z)),
           fields ## default to just returning the fields as supplied 
       )
}

## private function to replace any id values with their full field names (descriptions)
fields_id_to_name <- function(fields,fields_type) {
    assert_that(is.character(fields))
    assert_that(is.string(fields_type))
    fields_type <- match.arg(tolower(fields_type),c("occurrence","general","layers","assertions"))
    valid_fields <- ala_fields(fields_type=fields_type)
    ## merge differently for "layers" fields, because those column names differ from other fields_type
    ## for layers, the long name is in "desc", with the id in "id" (and "name" is something different)
    ## ** as of 17-Jun-2014, "desc" is now renamed "description"
    ## for "occurrence" and "assertions", long name is in "description" and id is in "name"
    ## for general, there is no long name (description)
    ## for each one, warn if multiple matches on long name are found
    switch(fields_type,
           "layers"=laply(fields,function(z) ifelse(z %in% valid_fields$id & ! z %in% valid_fields$description,{
               if (sum(valid_fields$id==z,na.rm=TRUE)>1) {
                   if (nchar(z)>0) {
                       warning(" multiple ",fields_type," fields match the id \"",z,"\", using first")
                   }
               }
               valid_fields$description[which(valid_fields$id==z)[1]]
           },z)),
           "occurrence"=,
           "assertions"=laply(fields,function(z) ifelse(z %in% valid_fields$name & ! z %in% valid_fields$description,{
               if (sum(valid_fields$name==z,na.rm=TRUE)>1) {
                   if (nchar(z)>0) {
                       warning(" multiple ",fields_type," fields match the id \"",z,"\", using first")
                   }
               }
               valid_fields$description[which(valid_fields$name==z)[1]]
           },z)),
           fields ## default to just returning the fields as supplied 
       )
}
