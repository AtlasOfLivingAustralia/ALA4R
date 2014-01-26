## grab all data for a taxon of interest
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
