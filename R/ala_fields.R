#' Retrieves a list of all field names that can be used with data retrieval functions
#' 
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \itemize{
#' \item ALA web service API: \url{http://api.ala.org.au/}
#' \item Descriptions of the spatial layers: \url{http://spatial.ala.org.au/layers/}
#' }
#' @seealso \code{\link{search_layers}} to search for spatial layers
#' @param fields_type text: either
#' \itemize{
#' \item "general" - for searching taxa, datasets, layers, and collections metadata
#' \item "occurrence" - for searching species occurrence records
#' \item "layers" - fields associated with the environmental and contextual layers. For additional information on layers, including metadata and licensing, see \code{\link{search_layers}}
#' \item "assertions" - record issues associated with occurrences
#' }
#' @param field_id text: id of environmental/contextual layer field for which to look up information
#' Prepend "el" for "environmental" (gridded) layers and "cl" for "contextual" (polygonal) layers
#' @return A data frame containing the field names and various attributes
#' @examples
#' l=ala_fields("layers")
#' l[1,]
#' a=ala_fields("assertions")
#' field_info("cl22")
#' field_info("el773")
#' @export ala_fields

# TODO: Summary of #fields returned
# ids from http://spatial.ala.org.au/ws/layers are NUMERIC but lookup prepends "el" and "cl"! 


ala_fields=function(fields_type="occurrence") {
    assert_that(is.string(fields_type))
    fields_type=match.arg(tolower(fields_type),c("occurrence","general","layers","assertions"))
    switch(fields_type,
           "general"={base_url=paste(ala_config()$base_url_bie,"admin/indexFields",sep="")},
           "occurrence"={base_url=paste(ala_config()$base_url_biocache,"index/fields",sep="")},
           "layers"={base_url=paste(ala_config()$base_url_spatial,"fields",sep="")},
           "assertions"={base_url=paste(ala_config()$base_url_biocache,"assertions/codes",sep="")}
           )

    x=cached_get(base_url,type="json")

    ## for "layers", shorter, more manageable names are provided from http://spatial.ala.org.au/ws/layers in API. Add these as an extra column: shortName
    if (identical(fields_type,"layers")) {
        more_x=cached_get(url=paste(ala_config()$base_url_spatial,"layers",sep=""),type="json")
        ## just pull out the bits that we want and construct ids here that match the field names in x
        more_x$id=paste(substr(tolower(more_x$type),1,1),"l",more_x$id,sep="")
        more_x=more_x[,c("name","id")]
        names(more_x)=c("shortName","id")
        x=merge(x,more_x,by="id")
        x$type[x$type=="c"]="Contextual"
        x$type[x$type=="b"]="Contextual" ## there is an errant "b" here that should be "c"
        x$type[x$type=="e"]="Environmental" ## for consistency with search_layers
    }        
    names(x)=rename_variables(names(x),type=fields_type)
    ## drop unwanted columns
    xcols=setdiff(names(x),unwanted_columns(fields_type))
    x[,xcols]
}


#' @rdname ala_fields
#' @export
field_info = function(field_id) {
    assert_that(is.string(field_id))
    field_id=fields_name_to_id(fields=field_id,fields_type="layers")
    base_url = paste(ala_config()$base_url_spatial,"field",sep="")
    ## if we supply an unknown field_id, we get 500 error from the server. But this doesn't make sense, so we mask the server error and simply return an empty data frame in this case
    this_server_error=function(z) NULL
    out = cached_get(url=paste(base_url,field_id,sep='/'),type="json",on_server_error=this_server_error)
    if (is.null(out)) {
        ## if we got a 500 error, the response will be NULL
        ## it was most likely from an un-matched field name (but we do not get any sort of informative error message from the server)
        ## just return an empty data frame with no warning
        data.frame()
    } else {
        ## we might wish to issue a warning for empty responses
        if (substr(field_id,1,2) == 'cl') {
            out = out$objects #keep only the content
        } else if (substr(field_id,1,2) == 'el') {
            out = as.data.frame(rbind(out)) #bind the data as a dataframe	
            rownames(out) = NULL #reset the row names
        }
        names(out)=rename_variables(names(out),type="layers")
        out
    }
}	



## private function to replace any full field names (descriptions) with their id values
## e.g. "Radiation - lowest period (Bio22)" to id "el871"
fields_name_to_id=function(fields,fields_type) {
    assert_that(is.character(fields))
    assert_that(is.string(fields_type))
    fields_type=match.arg(tolower(fields_type),c("occurrence","general","layers","assertions"))
    valid_fields=ala_fields(fields_type=fields_type)
    ## merge differently for "layers" fields, because those column names differ from other fields_type
    ## for layers, the long name is in "desc", with the id in "id" (and "name" is something different)
    ## ** as of 17-Jun-2014, "desc" is now renamed "description"
    ## for "occurrence" and "assertions", long name is in "description" and id is in "name"
    ## for general, there is no long name (description)
    ## for each one, warn if multiple matches on long name are found
    switch(fields_type,
           "layers"=laply(fields,function(z) ifelse(z %in% valid_fields$description & ! z %in% valid_fields$id,{
               if (sum(valid_fields$description==z,na.rm=TRUE)>1)
                   warning(" multiple ",fields_type," fields match the name \"",z,"\", using first")                
               valid_fields$id[which(valid_fields$description==z)]
           },z)),
           "occurrence"=,
           "assertions"=laply(fields,function(z) ifelse(z %in% valid_fields$description & ! z %in% valid_fields$name,{
               if (sum(valid_fields$description==z,na.rm=TRUE)>1)
                   warning(" multiple ",fields_type," fields match the name \"",z,"\", using first") 
               valid_fields$name[which(valid_fields$description==z)]
           },z)),
           fields ## default to just returning the fields as supplied 
       )
}

## private function to replace any id values with their full field names (descriptions)
fields_id_to_name=function(fields,fields_type) {
    assert_that(is.character(fields))
    assert_that(is.string(fields_type))
    fields_type=match.arg(tolower(fields_type),c("occurrence","general","layers","assertions"))
    valid_fields=ala_fields(fields_type=fields_type)
    ## merge differently for "layers" fields, because those column names differ from other fields_type
    ## for layers, the long name is in "desc", with the id in "id" (and "name" is something different)
    ## ** as of 17-Jun-2014, "desc" is now renamed "description"
    ## for "occurrence" and "assertions", long name is in "description" and id is in "name"
    ## for general, there is no long name (description)
    ## for each one, warn if multiple matches on long name are found
    switch(fields_type,
           "layers"=laply(fields,function(z) ifelse(z %in% valid_fields$id & ! z %in% valid_fields$description,{
               if (sum(valid_fields$id==z,na.rm=TRUE)>1)
                   warning(" multiple ",fields_type," fields match the id \"",z,"\", using first")                
               valid_fields$description[which(valid_fields$id==z)]
           },z)),
           "occurrence"=,
           "assertions"=laply(fields,function(z) ifelse(z %in% valid_fields$name & ! z %in% valid_fields$description,{
               if (sum(valid_fields$name==z,na.rm=TRUE)>1)
                   warning(" multiple ",fields_type," fields match the id \"",z,"\", using first") 
               valid_fields$description[which(valid_fields$name==z)]
           },z)),
           fields ## default to just returning the fields as supplied 
       )
}
