#' Fetch a taxon profile given a scientific name or LSID
#' 
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/}
#' 
#' @param scientificname string: scientific name of the taxon of interest (species, genus, family etc) 
#' @param LSID string: The Life Science Identifier of the taxon of interest
#' @param verbose boolean value: How much progress information to display; default is set by ala_config().
#' @return species profile in the form of a named list

#' @examples
#' 
#' species_info("Grevillea humilis subsp. maritima")
#' species_info(guid="urn:lsid:biodiversity.org.au:apni.taxon:248651")
#' 
#' @export species_info

# TODO: support multiple names or guids passed as a vector? (LB:low priority)

species_info=function(scientificname=NULL,guid=NULL,verbose=ala_config()$verbose) {
    if (is.null(scientificname) && is.null(guid)) {
        stop("either the scientific name or the guid must be provided")
    }
    if ((! is.null(scientificname)) && (! is.null(guid))) {
        stop("either the scientific name or the guid must be provided, but not both")
    }
    if (! is.null(scientificname)) {
        guid=name_guid(scientificname,guids_only=TRUE)
        if (length(guid)<1) {
            return(NULL)
        }
        guid=guid[[1]]
    }
    url=paste(ala_config()$base_url_bie,"species/",guid,".json",sep="")
    cached_get(url,type="json",verbose=verbose)
}
