#' Fetch a taxon profile given a scientific name or LSID (GUID)
#' 
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/}
#' 
#' @param scientificname string: scientific name of the taxon of interest (species, genus, family etc) 
#' @param guid string: The Life Science Identifier of the taxon of interest
#' @param verbose logical: show additional progress information? [default is set by ala_config()]
#' @return species profile in the form of a named list, each element of which is generally a data frame
#' @seealso \code{\link{ala_config}}
#' @examples
#' 
#' species_info("Grevillea humilis subsp. maritima")
#' species_info(guid="urn:lsid:biodiversity.org.au:apni.taxon:248651")
#' s=species_info("Alaba vibex")
#' str(s)
#' 
#' @export species_info

# TODO: support multiple names or guids passed as a vector? (LB:low priority)

species_info=function(scientificname,guid,verbose=ala_config()$verbose) {
    if (!missing(scientificname)) {
        assert_that(is.string(scientificname))
    }
    if (!missing(guid)) {
        assert_that(is.string(guid))
    }
    assert_that(is.flag(verbose))
    if (missing(scientificname) && missing(guid)) {
        stop("either the scientific name or the guid must be provided")
    }
    if ((!missing(scientificname)) && (!missing(guid))) {
        stop("either the scientific name or the guid must be provided, but not both")
    }
    if (!missing(scientificname)) {
        guid=search_names(scientificname,vernacular=FALSE,guids_only=TRUE)
        if (length(guid)<1) {
            return(NULL)
        }
        guid=guid[[1]]
    }
    url=paste(ala_config()$base_url_bie,"species/",guid,".json",sep="")
    out=cached_get(URLencode(url),type="json",verbose=verbose)
    ## rename a couple of things
    names(out$classification)=str_replace_all(names(out$classification),"^clazz","class")
    ## restructure any list children of out to be data.frames
    for (k in 1:length(out)) {
        if (is.list(out[[k]])) {
            out[[k]]=as.data.frame(out[[k]])
        }
    }
    out
}
