#' Auto Complete Search
#' 
#' This is an autocomplete search for identifying names & identifiers used at
#' ALA. It is used to provide a dataframe of scientific and common names that
#' can be used for further analysis that is a match from a supplied partial
#' name.
#' 
#' 
#' @param taxon a character string that defines part of the scientific or
#' common name of the taxa of interest
#' @param limit the maximum number of matches returned
#' @return A dataframe of taxa given the partial matches where columns are
#' identified as: \item{guid}{} \item{name}{} \item{occurrenceCount}{}
#' \item{georeferencedCount}{} \item{scientificNameMatches}{}
#' \item{commonNameMatches}{} \item{commonName}{} \item{matchedNames}{}
#' \item{ankId}{} \item{rankString}{} \item{left}{} \item{right}{}
#' @author Jeremy VanDerWal \email{jjvanderwal@@gmail.com}, Ben Raymond
#' \email{ben@@theraymonds.org}
#' @references
#' \url{http://www.ala.org.au/about-the-atlas/downloadable-tools/web-services/}
#' @examples
#' 
#' 	#find information ALA holds on red kangaroo
#' 	ala_autocomplete("red kangaroo")
#' 
#' @export ala_autocomplete
ala_autocomplete=function(taxon,limit=10) {
	taxon = clean_string(taxon) #clean up the taxon name
        base_url="http://bie.ala.org.au/ws/search/auto.json"
        this_url=parse_url(base_url)
        this_url$query=list(q=taxon,limit=limit)
        this_url=build_url(this_url)

        if (identical(ala_config()$caching,"off")) {
            ## if we are not caching, get this directly without saving to file at all
            x=GET(url=this_url,user_agent(ala_config()$user_agent))
            x=content(x)[[1]]
        } else {
            ## use caching
            thisfile=ala_download_to_file(this_url)
            x=fromJSON(file=thisfile)[[1]]
        }
        
        ## data comes back as nested list structure, which is a direct and somewhat mindless conversion of the raw JSON data
        ## some variables are returned as lists, and these may be empty (e.g. commonNameMatches, if there are no such matches) which causes problems when casting to data.frame
        ldply(x,function(y){ as.data.frame(lapply(y,function(z){ ifelse(class(z)=="list" && length(z)==0,"",z) })) })
    }
