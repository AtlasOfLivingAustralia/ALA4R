#' Provides a list of all environmental and contextual layers in ALA
#' 
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/} \url{http://spatial.ala.org.au/layers}
#'
#' @param type a selection of "all", "grids" or "shapes" associated with all possible layers, 
#' only environmental grids or contextual shapefiles
#' @return A data frame of results
#' \item{name} \item{id} \item{type} \item{path} \item{description} \item{source}
#' \item{displayname} \item{enabled} \item{uid} \item{metadatapath} \item{classification1}
#' \item{classification2} \item{notes} \item{source_link} \item{licence_link} \item{licence_notes} 
#' \item{maxlatitude} \item{minlatitude} \item{minlongitude} \item{maxlongitude}
#' \item{pid} \item{shape} \item{path_orig} \item{environmentalvalueunits} \item{scale} 
#' \item{environmentalvaluemax} \item{environmentalvaluemin} \item{lookuptablepath}
#' \item{citation_date} \item{datalang} \item{licence_level} \item{mddataset}
#' \item{mdhrlv} \item{respparty_role} \item{keywords} \item{domain}
#' 
#' @examples
#' 
#' enviro_layers(type="all")
#' enviro_layers(type="grids")
#' enviro_layers(type="shapes")
#' 
#' @export enviro_layers'
#' 
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
