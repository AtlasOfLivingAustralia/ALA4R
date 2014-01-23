## grab all data for a taxon of interest
ala_species_info <- function(taxon) {
	taxon = clean_string(taxon) #clean up the taxon name
        base_url="http://bie.ala.org.au/ws/search.json"
        this_url=parse_url(base_url)
        this_url$query=list(q=taxon)
        x=GET(url=build_url(this_url),user_agent(ala_user_agent()))
        content(x)[[1]]
}
