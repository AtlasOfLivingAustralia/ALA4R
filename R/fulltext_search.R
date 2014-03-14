#' Full text search
#' 
#' Performs a search across all objects, and selects the closest matches
#'
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/}
#'  
#' @param taxon string: a character string for the taxon of interest
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
#' 
#'  #find information ALA holds on red kangaroo
#'  fulltext_search("red kangaroo")
#'  fulltext_search("Macropus rufus")
#'  fulltext_search("urn:lsid:biodiversity.org.au:afd.taxon:31a9b8b8-4e8f-4343-a15f-2ed24e0bf1ae")
#' 
#' @export
fulltext_search <- function(taxon) {
	taxon = clean_string(taxon) #clean up the taxon name
        base_url="http://bie.ala.org.au/ws/search.json"
        this_url=parse_url(base_url)
        this_url$query=list(q=taxon)
        this_url=build_url(this_url)
        x=cached_get(url=this_url,type="json")
        if (identical(find("fromJSON"),"package:jsonlite")) {
            ## reformatting not needed with jsonlite
            x=as.list(x)
        } else {
            ## using e.g. rjson for fromJSON conversion
            x=x[[1]]
            ## reformat results to data frame
            x$results=rbind.fill(lapply(x$results,as.data.frame)) ## convert each element of results into data frame, then combine
        }
        x
}

## TODO: add support for fq, start, pageSize, sort, dir params
## TODO: reformat data structure
#GET(url="http://bie.ala.org.au/ws/search.json?q=Grevillea&fq=kingdom:Plantae") # works
#GET(url="http://bie.ala.org.au/ws/search.json?q=Grevillea&fq=genus:Grevillea") # works
#GET(url="http://bie.ala.org.au/ws/search.json?q=Grevillea&fq=species%3Abanksii") # 500 error
