#' Search for environmental and contextual data layers
#' 
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/} \url{http://spatial.ala.org.au/layers}
#'
#' @param type string: either "all" (all possible layers; default), "grids" (gridded environmental layers), or "shapes" (contextual shapefile layers)
#' @param query text string: optional search term against layer metadata. Only layers that include this term in their metadata will be returned.
#' 
#' @return A data frame of results. The contents (column names) of the data frame will vary depending on the details of the search and the results.
#' 
#' @examples
#' \dontrun{
#' search_layers(type="all")
#' search_layers(type="grids",query="income")
#' l=search_layers(type="shapes",query="coral")
#' str(l)
#' }
#' @export
search_layers = function(type="all",query) {
    assert_that(is.string(type))
    if (!missing(query)) {
        assert_that(is.string(query))
    }
    type=match.arg(tolower(type),c("all","grids","shapes"))
    base_url = 'http://spatial.ala.org.au/ws/layers' ## define the base url
    if (type %in% c("grids","shapes")) {
        base_url=paste(base_url,type,sep='/')
    }
    out = cached_get(url=base_url,type="json") ## download all data
    if (!missing(query)) {
        out=out[grepl(query,out$name,ignore.case=TRUE) | grepl(query,out$description,ignore.case=TRUE),]
    }
    out
}
