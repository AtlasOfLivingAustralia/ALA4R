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
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/}
#' @examples
#' 
#' ala_fields("general")
#' field_info('cl22')
#' 
#' @export

ala_fields=function(fields_type="general") {
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

    ## for "layers", shorter, more manageable names are provided from http://spatial.ala.org.au/layers.json
    ## add these as an extra column: name_short
    ## TODO are these applicable to our other fields_type as well?
    if (identical(fields_type,"layers")) {
        more_x=cached_get(url="http://spatial.ala.org.au/layers.json",type="json")
        ##more_x=rbind.fill(lapply(more_x,as.data.frame)) ## this is slow
        ## just pull out the bits that we want
        ## and construct ids here that match the field names in x
        more_x=ldply(more_x,function(z){c(z$name,paste(substr(tolower(z$type),1,1),"l",z$id,sep=""))})
        names(more_x)=c("name_short","id")
        x=merge(x,more_x,by="id")
    }
    x
}


#' @rdname ala_fields
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
