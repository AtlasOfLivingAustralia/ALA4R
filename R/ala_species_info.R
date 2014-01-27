#' Taxon or species info
#' 
#' Returns the profile information for a given taxon. more needed!!!
#' 
#' NEED
#' 
#' @param taxon a character string for the taxon of interest
#' @return a dataframe of information with columns being: \item{comp1
#' }{Description of 'comp1'}
#' @author Jeremy VanDerWal \email{jjvanderwal@@gmail.com}, Ben Raymond
#' \email{ben@@theraymonds.org}
#' @references
#' \url{http://www.ala.org.au/about-the-atlas/downloadable-tools/web-services/}
#' @examples
#' 
#' 	#find information ALA holds on red kangaroo
#' 	ala_species_info("red kangaroo")
#' 
#' @export ala_species_info
ala_species_info <- function(taxon) {
	taxon = clean_string(taxon) #clean up the taxon name
        base_url="http://bie.ala.org.au/ws/search.json"
        this_url=parse_url(base_url)
        this_url$query=list(q=taxon)
        this_url=build_url(this_url)
        if (identical(ala_config()$caching,"off")) {
            ## if we are not caching, get this directly without saving to file at all
            x=GET(url=this_url,user_agent(ala_config()$user_agent))
            x=content(x)
        } else {
            ## use caching
            thisfile=ala_download_to_file(this_url)
            x=fromJSON(file=thisfile)
        }
        x=x[[1]]
        ## reformat results to data frame
        x$results=rbind.fill(lapply(x$results,as.data.frame)) ## convert each element of results into data frame, then combine
        x
}
