#' Fetch species profile
#' 
#' Retrieve species profile given either the scientific name or GUID
#' 
#' 
#' @param scientificname string: scientific name of the species of interest
#' @param guid string: GUID of species of interest
#' @param verbose boolean value defining how much progress information to display; default is set by ala_config().
#' @return species profile in the form of a named list
#' @author Ben Raymond \email{ben@@theraymonds.org}, Jeremy VanDerWal
#' \email{jjvanderwal@@gmail.com}
#' @references \url{http://bie.ala.org.au/bie-service/}
#' @examples
#' 
#' species_info("Grevillea humilis subsp. maritima")
#' species_info(guid="urn:lsid:biodiversity.org.au:apni.taxon:248651")
#' 
#' @export species_info

# TODO: support multiple names or guids passed as a vector?

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
