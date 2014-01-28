#' Retrieve a list of all fields
#' 
#' Retrieves a list of field names that can be used with the data retrieval
#' functions
#' 
#' @param fields_type text: either "general" (for searching taxa, datasets,
#' layers, and collections metadata), "occurrence" (for searching species
#' occurrence records), or "layers" (a list of all fields associated with the environmental and contextual layers)
#' @param field_id text: id of field for which to look up information
#' @return A data frame containing the field names and various attributes
#' @author Ben Raymond \email{ben@@theraymonds.org}, Jeremy VanDerWal
#' \email{jjvanderwal@@gmail.com}
#' @references For "occurrence",
#' \url{http://biocache.ala.org.au/ws/index/fields}.  For "general",
#' \url{http://bie.ala.org.au/ws/admin/indexFields}. For "layers", \url{http://spatial.ala.org.au/ws/field}
#' @examples
#' 
#' fields("general")
#' field_info('cl22')
#' 
#' @export

fields=function(fields_type="general") {
    fields_type=tolower(fields_type)
    match.arg(fields_type,c("general","occurrence","layers"))
    switch(fields_type,
           "general"={base_url=paste(ala_config()$base_url_bie,"admin/indexFields",sep="")},
           "occurrence"={base_url=paste(ala_config()$base_url_biocache,"index/fields",sep="")},
           "layers"={base_url=paste(ala_config()$base_url_spatial,"fields",sep="")}
           )

    x=cached_get(base_url,type="json")
    x=rbind.fill(lapply(x,as.data.frame)) ## convert each element of content(x)[[1]] into data frame, then combine
        
    ## convert factors to strings
    for (col in 1:ncol(x)) {
        if (identical(class(x[,col]),"factor")) {
            x[,col]=as.character(x[,col])
        }
    }
    x
}


#' @rdname fields
#' @export
field_info = function(field_id) {
    base_url = paste(ala_config()$base_url_spatial,"field",sep="")
    out = cached_get(url=paste(base_url,field_id,sep='/'),type="json")
    ## the http status code is checked as part of cached_get
    ## however, we might wish to issue a warning for empty responses
    if (substr(field_id,1,2) == 'cl') {
        out = out$objects #keep only the content
        do.call('rbind.fill',lapply(out,as.data.frame)) #bind the data as a dataframe
    } else if (substr(field_id,1,2) == 'el') {
        out = as.data.frame(rbind(out)) #bind the data as a dataframe	
        rownames(out) = NULL #reset the row names
        out
    }
}	
