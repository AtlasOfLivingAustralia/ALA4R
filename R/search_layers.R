#' Search for environmental and contextual data layers
#' 
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/} \url{http://spatial.ala.org.au/layers}
#'
#' @param type string: either "all" (all possible layers; default), "grids" (gridded environmental layers), or "shapes" (contextual shapefile layers)
#' @param query text string: optional search term against layer metadata. Only layers that include this term in their metadata will be returned.
#' @param output_format string: controls the print method for the returned object. Either "complete" (the complete data structure is displayed), or "simple" (a simplified version is displayed). Note that the complete data structure exists in both cases: this option only controls what is displayed when the object is printed to the console. The default output format is "simple"
#' @return A data frame of results. The contents (column names) of the data frame will vary depending on the details of the search and the results.
#' 
#' @examples
#' search_layers(type="all")
#' search_layers(type="grids",query="income")
#' l=search_layers(type="shapes",query="coral")
#' str(l)
#' @export
search_layers = function(query,type="all",output_format="simple") {
    assert_that(is.string(type))
    type=match.arg(tolower(type),c("all","grids","shapes"))
    if (!missing(query)) {
        assert_that(is.string(query))
    }
    assert_that(is.character(output_format))
    output_format=match.arg(tolower(output_format),c("simple","complete"))    
    base_url = 'http://spatial.ala.org.au/ws/layers' ## define the base url
    if (type %in% c("grids","shapes")) {
        base_url=paste(base_url,type,sep='/')
    }
    out = cached_get(url=base_url,type="json") ## download all data
    if (!missing(query)) {
        out=out[grepl(query,out$name,ignore.case=TRUE) | grepl(query,out$description,ignore.case=TRUE),]
    }
    ## change id from numeric to "elxxx" or "clxxx" as appropriate for environmental/contextual
    if (!empty(out)) {
        out$id=paste(substr(tolower(out$type),1,1),"l",out$id,sep="")
    }
    ## remove some columns that are unlikely to be of value here
    xcols=setdiff(names(out),unwanted_columns("layers"))
    out=out[,xcols]
    ## change variable names for consistency
    names(out)=rename_variables(names(out),type="layers")
    ## change "name" to "shortName", "displayname" to "name" so as to match ala_fields("layers")
    names(out)[names(out)=="name"]="shortName"
    names(out)[names(out)=="displayname"]="name"    
    ## reorder columns, for minor convenience
    xcols=names(out)
    firstcols=intersect(c("name","id","type","description"),xcols)
    xcols=c(firstcols,setdiff(xcols,firstcols))
    out=out[,xcols]
    attr(out,"output_format")=output_format
    class(out)=c("search_layers",class(out)) ## add the search_names class
    out
}

#' @S3method print search_layers
"print.search_layers" <- function(x, ...)
{
    cols=names(x)
    if (identical(attr(x,"output_format"),"simple")) {
        cols=intersect(c("name","id","description","type","notes","environmentalvalueunits","licence_notes","licence_link","notes"),cols)
    }
    m=as.matrix(format.data.frame(x[,cols],na.encode=FALSE))
    print(m)
    invisible(x)
}
