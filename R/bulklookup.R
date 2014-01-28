#' Bulk lookup of GUIDs
#' 
#' Used to provide GUIDs for a list of names
#' 
#' 
#' @param taxa A list of names
#' @return A data frame of results
#' @author Ben Raymond \email{ben@@theraymonds.org}, Jeremy VanDerWal
#' \email{jjvanderwal@@gmail.com}
#' @references \url{http://bie.ala.org.au/bie-service/}
#' @examples
#' 
#' bulklookup(list("Grevillea humilis","Grevillea humilis subsp. maritima"))
#' 
#' @export bulklookup
bulklookup=function(taxa=list()) {
    if (is.character(taxa)) {
        ## single name provided as a string, we were expecting a list of names
        taxa=list(taxa)
    }
    taxa = lapply(taxa,clean_string) ## clean up the taxon name
    base_url=paste(ala_config()$base_url_bie,"species/bulklookup.json",sep="")
    x=POST(url=base_url,body=toJSON(taxa),user_agent(ala_config()$user_agent)) ## no caching on POST operations yet
    rbind.fill(lapply(content(x)[[1]],as.data.frame)) ## convert each element of content(x)[[1]] into data frame, then combine
}
