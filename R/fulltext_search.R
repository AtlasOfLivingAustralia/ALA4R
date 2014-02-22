#' Full text search
#' 
#' Performs a search across all objects, and selects the closest matches
#' 
#' @param taxon a character string for the taxon of interest
#' @return a dataframe of information with columns being: \item{comp1
#' }{Description of 'comp1'}
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/}
#' @examples
#' 
#' 	#find information ALA holds on red kangaroo
#' 	fulltext_search("red kangaroo")
#' 
#' @export
fulltext_search <- function(taxon) {
	taxon = clean_string(taxon) #clean up the taxon name
        base_url="http://bie.ala.org.au/ws/search.json"
        this_url=parse_url(base_url)
        this_url$query=list(q=taxon)
        this_url=build_url(this_url)
        x=cached_get(url=this_url,type="json")
        x=x[[1]]
        ## reformat results to data frame
        x$results=rbind.fill(lapply(x$results,as.data.frame)) ## convert each element of results into data frame, then combine
        x
}

## TODO: add support for fq, start, pageSize, sort, dir params
