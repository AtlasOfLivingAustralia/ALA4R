#' List of environmental and contextual layers
#' 
#' Used to provide a list of all environmental and contextual layers
#' provided through ALA.
#' 
#' @param type a selection of "all", "grids" or "shapes" associated with all possible layers, only environmental grids or contextual shapefiles
#' @return A data frame of results
#' @author Jeremy VanDerWal \email{jjvanderwal@@gmail.com}, Ben Raymond \email{ben@@theraymonds.org}
#' @references \url{http://spatial.ala.org.au/ws/}
#' @examples
#' 
#' enviro_layers(type="all")
#' enviro_layers(type="grids")
#' enviro_layers(type="shapes")
#' 
#' @export enviro_layers
enviro_layers = function(type="all") {
    base_url = 'http://spatial.ala.org.au/ws/layers' #define the base url
	if (type == 'all') { 
		out = GET(url=base_url,user_agent(ala_config()$user_agent)) #download all data
	} else if (type == 'grids') {
		out = GET(url=paste(base_url,type,sep='/'),user_agent(ala_config()$user_agent)) #download only grids
	} else if (type == 'shapes') {
		out = GET(url=paste(base_url,type,sep='/'),user_agent(ala_config()$user_agent)) #download only shapefile info
	} else {
		stop('type must be either all, grids or shape') #incorrect type so stop
	}
	out = content(out) #keep only the content
	do.call('rbind.fill',lapply(out,as.data.frame)) #bind the data as a dataframe
}
