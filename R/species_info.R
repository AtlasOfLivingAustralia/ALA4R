#' Fetch a taxon profile given a scientific name or LSID (GUID)
#' 
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/}
#' 
#' @param scientificname string: scientific name of the taxon of interest (species, genus, family etc) 
#' @param guid string: The Life Science Identifier of the taxon of interest
#' @param verbose logical: show additional progress information? [default is set by ala_config()]
#' @return A species profile in the form of a named list, each element of which is generally a data frame. An empty list is returned if no match is found for the supplied name or guid
#' @seealso \code{\link{ala_config}}
#' @examples
#' 
#' s1=species_info("Grevillea humilis subsp. maritima")
#' str(s1)
#' s2=species_info(guid="urn:lsid:biodiversity.org.au:apni.taxon:248651")
#' str(s2)
#' s3=species_info("Alaba",verbose=TRUE)
#' str(s3)
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
            return(list())
        }
        guid=guid[[1]]
    }
    url=paste(ala_config()$base_url_bie,"species/",guid,".json",sep="")
    out=cached_get(URLencode(url),type="json",verbose=verbose)
    if (is.null(out)) {
        ## invalid guids will give NULL here, catch them now
        return(list())
    }
    ## restructure any list children of out to be data.frames
    for (k in 1:length(out)) {
        if (is.list(out[[k]])) {
            out[[k]]=as.data.frame(out[[k]],stringsAsFactors=FALSE)
        }
    }
    ## weed out unwanted columns and rename variables in each child object
    ## some columns consistently used across these child objects but seem to be internal identifiers and of little use here
    dud_cols=c("id","parentId","infoSourceId","documentId")
    for (k in 1:length(out)) {
        if (class(out[[k]])=="data.frame") {
            tempcols=setdiff(names(out[[k]]),dud_cols)
            out[[k]]=out[[k]][,tempcols]
        }
    }
    ## taxonConcept
    tempcols=setdiff(names(out$taxonConcept),unwanted_columns(type="general"))
    #tempcols=setdiff(tempcols,c("id","parentId","infoSourceId"))
    out$taxonConcept=subset(out$taxonConcept,select=tempcols)    
    names(out$taxonConcept)=rename_variables(names(out$taxonConcept),type="general")
    ## taxonName
    names(out$taxonName)=rename_variables(names(out$taxonName),type="general")
    ## classification
    tempcols=setdiff(names(out$classification),unwanted_columns(type="general"))
    out$classification=subset(out$classification,select=tempcols)
    names(out$classification)=str_replace_all(names(out$classification),"^clazz","class")
    names(out$classification)=rename_variables(names(out$classification),type="general")
    ## identifiers is a list - is OK
    ## commonNames - just the dud_cols above
    #tempcols=setdiff(names(out$commonNames),c("infoSourceId","documentId"))
    #out$commonNames=out$commonNames[,tempcols]
    ## synonyms
    tempcols=setdiff(names(out$synonyms),unwanted_columns(type="general"))    
    #tempcols=setdiff(tempcols,c("id","infoSourceId"))
    out$synonyms=subset(out$synonyms,select=tempcols)
    names(out$synonyms)=rename_variables(names(out$synonyms),type="general")
    ## sameAsConcepts
    #tempcols=setdiff(names(out$sameAsConcepts),c("id"))
    #out$sameAsConcepts=out$sameAsConcepts[,tempcols]
    names(out$sameAsConcepts)=rename_variables(names(out$sameAsConcepts),type="general")
    ## all other child objects seem OK at the moment
    ## we could in principle run everything through the rename/remove columns functions
    ## but let's not do so for now
    out
}
