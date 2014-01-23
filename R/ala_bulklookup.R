ala_bulklookup=function(taxa=list()) {
	taxa = lapply(taxa,clean_string) ## clean up the taxon name
        base_url="http://bie.ala.org.au/ws/species/bulklookup.json"
        x=POST(url=base_url,body=toJSON(taxa),user_agent(ala_user_agent()))
        rbind.fill(lapply(content(x)[[1]],as.data.frame)) ## convert each element of content(x)[[1]] into data frame, then combine
    }
