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
#' 	autocomplete("red kangaroo")
#' 
#' @export autocomplete
autocomplete=function(taxon,limit=10) {
	taxon = clean_string(taxon) #clean up the taxon name
	taxon = gsub(' ','+',taxon) #replace spaces with + to force both terms in the search
	if (class(limit) != 'numeric' | length(limit) > 1) stop('limit must be a single numeric value') #check limit is numeric and single value
	
	base_url=ala_config()$base_url_bie #define the base URL string
	url_str = paste(base_url,"search/auto.json?q=",taxon,"&limit=",limit,sep="") #define the URL string
	
	out = cached_get(url_str,type="json") #get the data
	out = out[[1]] #looking at the data
	
	##need to collapse matchednames fields if there was more than a single matched name
	for (ii in 1:length(out)) {
		for (jj in 1:length(out[[ii]])) {
			if (length(out[[ii]][[jj]]) > 1) {
                            out[[ii]][[jj]] = paste(out[[ii]][[jj]],collapse=', ')
                        }
		}
	}
	
	out = do.call('rbind.fill',lapply(out,function(x) {as.data.frame(rbind(x))})) #define the output as a data.frame
	return(out)
}
