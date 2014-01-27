#' List of environmental and contextual layers
#' 
#' Used to provide a list of all environmental and contextual layers
#' provided through ALA.
#' 
#' @param type a selection of all, grids or shape associated with all possible layers, only environmental grids or contextual shapefiles
#' @return A data frame of results
#' @author Jeremy VanDerWal \email{jjvanderwal@@gmail.com}, Ben Raymond \email{ben@@theraymonds.org}
#' @references \url{http://spatial.ala.org.au/ws/}
#' @examples
#' 
#' environmental_layers(type="all")
#' environmental_layers(type="grids")
#' environmental_layers(type="shape")
#' 
#' @export enviro_layers
enviro_layers = function(type="all") {
    if (type %in% c('all','grids','shape')) { #confirm appropriate type selected
        base_url = 'http://spatial.ala.org.au/ws/layers' #define the base url
		if (type == 'all') out = POST(url=base_url,body=toJSON(taxa),user_agent(ala_config()$user_agent))
    } else {
		stop('type must be either all, grids or shape')
	}
    taxa = lapply(taxa,clean_string) ## clean up the taxon name
    base_url="http://bie.ala.org.au/ws/species/bulklookup.json"
    x=POST(url=base_url,body=toJSON(taxa),user_agent(ala_config()$user_agent)) ## no caching on POST operations yet
    rbind.fill(lapply(content(x)[[1]],as.data.frame)) ## convert each element of content(x)[[1]] into data frame, then combine
}

