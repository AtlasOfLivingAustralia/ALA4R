#' List of environmental and contextual layers
#' 
#' Used to provide a list of all environmental and contextual layers
#' provided through ALA.
#' 
#' @param type a selection of "all", "grids" or "shapes" associated with all possible layers, only environmental grids or contextual shapefiles
#' @return A data frame of results
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/}
#' @examples
#' 
#' enviro_layers(type="all")
#' enviro_layers(type="grids")
#' enviro_layers(type="shapes")
#' 
#' @export enviro_layers
enviro_layers = function(type="all") {
    type=tolower(type)
    match.arg(type,c("all","grids","shapes"))
    base_url = 'http://spatial.ala.org.au/ws/layers' #define the base url
	if (type == 'all') { 
            out = cached_get(url=base_url,type="json") #download all data
	} else if (type == 'grids') {
            out = cached_get(url=paste(base_url,type,sep='/'),type="json") #download only grids
	} else if (type == 'shapes') {
            out = cached_get(url=paste(base_url,type,sep='/'),type="json") #download only shapefile info
	} else {
            stop('type must be either all, grids or shape') #incorrect type so stop
	}
	do.call('rbind.fill',lapply(out,as.data.frame)) #bind the data as a dataframe
}
