ala_autocomplete=function(taxon,limit=10) {
	taxon = clean_string(taxon) #clean up the taxon name
        base_url="http://bie.ala.org.au/ws/search/auto.json"
        this_url=parse_url(base_url)
        this_url$query=list(q=taxon,limit=limit)
        x=GET(url=build_url(this_url),user_agent(ala_user_agent()))
        x=content(x)[[1]]
        ## data comes back as nested list structure, which is a direct and somewhat mindless conversion of the raw JSON data
        ## some variables are returned as lists, and these may be empty (e.g. commonNameMatches, if there are no such matches) which causes problems when casting to data.frame
        ldply(x,function(y){ as.data.frame(lapply(y,function(z){ ifelse(class(z)=="list" && length(z)==0,"",z) })) })
    }
