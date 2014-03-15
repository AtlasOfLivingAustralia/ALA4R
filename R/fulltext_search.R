#' Full text search
#' 
#' Performs a search across all objects, and selects the closest matches
#'
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/}
#'  
#' @param taxon string: a character string for the taxon of interest
#' @param fq string: filters to be applied to the original query. These are of the form "INDEXEDFIELD:VALUE" e.g. "kingdom:Fungi" See ala_fields("occurrence") for all the fields that are queryable
#' @return a named list, including the results component which is a dataframe of information with columns being:
#' \itemize{
#' \item{guid}  \item{name}  \item{idxtype}
#' \item{score} \item{parentGuid}
#' \item{commonName}  \item{nameComplete}
#' \item{commonNameSingle}  \item{hasChildren} \item{rank} \item{rankId} \item{rawRank} \item{conservationStatus} \item{conservationStatusAUS}
#' \item{isAustralian}
#' \item{highlight}
#' \item{image} \item{thumbnail} \item{left} \item{right} \item{kingdom}
#' \item{phylum} \item{class} \item{order} \item{class} \item{order} \item{family} \item{genus} \item{author}
#' \item{linkIdentifier} \item{occCount} \item{imageSource} \item{imageCount} \item{isEcluded}
#' \item{imageUrl} \item{largeImageUrl} \item{smallImageUrl} \item{thumbnailUrl} \item{imageMetadataUrl} 
#' \item{acceptedConceptName} \item{synonomyRelationship} \item{synonomyDescription}
#' }
#' 
#' @examples
#'  # find information ALA holds on red kangaroo
#'  fulltext_search("red kangaroo")
#'  fulltext_search("Macropus rufus")
#'  fulltext_search("urn:lsid:biodiversity.org.au:afd.taxon:31a9b8b8-4e8f-4343-a15f-2ed24e0bf1ae")
#' 
#' @export
fulltext_search <- function(taxon,fq=NULL) {
	taxon = clean_string(taxon) #clean up the taxon name
        base_url="http://bie.ala.org.au/ws/search.json"
        this_url=parse_url(base_url)
        this_query=list(q=taxon)
        if (!is.null(fq)) {
            this_query$fq=fq
        }
        this_url$query=this_query
        this_url=build_url(this_url)
        x=cached_get(url=this_url,type="json")
        #if using jsonlite {
            x=as.list(x)
        #} else {
        #    x=x[[1]]
        #    ## reformat results to data frame
        #    x$results=rbind.fill(lapply(x$results,as.data.frame)) ## convert each element of results into data frame, then combine
        #}
        x
}

## TODO: add support for start, pageSize, sort, dir params

## TODO: reformat data structure, probably similar to rgbif

## which fields can we actually query for fq? API doc says: "Filters to be applied to the original query. These are additional params of the form fq=INDEXEDFIELD:VALUE e.g. fq=kingdom:Fungi. See http://biocache.ala.org.au/ws/index/fields for all the fields that a queryable." BUT "species" appears in this list of fields, and yet
##GET(url="http://bie.ala.org.au/ws/search.json?q=Grevillea&fq=kingdom:Plantae") # works
##GET(url="http://bie.ala.org.au/ws/search.json?q=Grevillea&fq=genus:Grevillea") # works
##GET(url="http://bie.ala.org.au/ws/search.json?q=Grevillea&fq=species:banksii") # 500 error, with message "undefined field species"
## should we instead be using the "general" fields (http://bie.ala.org.au/ws/admin/indexFields), but note that this service is not currently part of the API

## fq matches are case-sensitive, but casing of names is not consistent (e.g. kingdoms "Fungi" and "Plantae" but "ANIMALIA")
##fulltext_search("Oenanthe") # returns a mix of birds and plants, because Oenanthe is a genus name in both kingdoms
##fulltext_search("Oenanthe",fq="kingdom:Plantae") ## OK
##fulltext_search("Oenanthe",fq="kingdom:plantae") ## no results
##fulltext_search("Oenanthe",fq="kingdom:PLANTAE") ## no results
##fulltext_search("Oenanthe",fq="kingdom:Animalia") ## no results
##fulltext_search("Oenanthe",fq="kingdom:ANIMALIA") ## OK
