#' Retrieves a list of all field names that can be used with data retrieval functions
#' 
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/}
#' 
#' @param fields_type text: either either "general" (for searching taxa, datasets,
#' layers, and collections metadata), "occurrence" (for searching species
#' occurrence records), or "layers" (a list of all fields associated with the environmental and contextual layers)
#' @param field_id text: id of field for which to look up information
#' @return A data frame containing the field names and various attributes

#' @examples
#' \dontrun{ 
#' ala_fields("occurrence")
#' field_info("cl22")
#' field_info("el773")
#' }
#' @export

## "general" is needed for e.g. full-text species searching, but note that this service is not currently part of the API

ala_fields=function(fields_type="occurrence") {
    assert_that(is.string(fields_type))
    fields_type=match.arg(tolower(fields_type),c("occurrence","general","layers"))
    switch(fields_type,
           "general"={base_url=paste(ala_config()$base_url_bie,"admin/indexFields",sep="")},
           "occurrence"={base_url=paste(ala_config()$base_url_biocache,"index/fields",sep="")},
           "layers"={base_url=paste(ala_config()$base_url_spatial,"fields",sep="")}
           )

    x=cached_get(base_url,type="json")
    #if NOT using jsonlite {
    #    x=rbind.fill(lapply(x,as.data.frame)) ## convert each element of content(x)[[1]] into data frame, then combine
    #    ## convert factors to strings
    #    for (col in 1:ncol(x)) {
    #        if (identical(class(x[,col]),"factor")) {
    #            x[,col]=as.character(x[,col])
    #        }
    #    }
    #}

    ## for "layers", shorter, more manageable names are provided from http://spatial.ala.org.au/ws/layers in API. Add these as an extra column: name_short
    if (identical(fields_type,"layers")) {
        more_x=cached_get(url=paste(ala_config()$base_url_spatial,"layers",sep=""),type="json")
        ## just pull out the bits that we want and construct ids here that match the field names in x
        #if using jsonlite {
            more_x$id=paste(substr(tolower(more_x$type),1,1),"l",more_x$id,sep="")
            more_x=more_x[,c("name","id")]
        #} else {
        #    more_x=ldply(more_x,function(z){c(z$name,paste(substr(tolower(z$type),1,1),"l",z$id,sep=""))})
        #}
        names(more_x)=c("name_short","id")            
        x=merge(x,more_x,by="id")
    }
    x
}


#' @rdname ala_fields
#' @export
field_info = function(field_id) {
    assert_that(is.string(field_id))
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
            #if NOT using jsonlite
            #    out=do.call('rbind.fill',lapply(out,as.data.frame)) #bind the data as a dataframe
            #}
            out
        } else if (substr(field_id,1,2) == 'el') {
            out = as.data.frame(rbind(out)) #bind the data as a dataframe	
            rownames(out) = NULL #reset the row names
            out
        }
    }
}	
