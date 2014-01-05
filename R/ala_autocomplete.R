
ala_autocomplete <- function(taxon,limit=10) {
	taxon = clean_string(taxon) #clean up the taxon name
	
	### download the data
	suppressWarnings( {
		out = fromJSON(file=paste("http://bie.ala.org.au/ws/search/auto.json?q=",taxon,"&limit=",limit,sep="")) 
	})
	
	tfun = function(x) { return(c( 
		guid=ifelse(is.null(x$guid),NA,x$guid),
		name=ifelse(is.null(x$name),NA,x$name),
		occurrenceCount=ifelse(is.null(x$occurrenceCount),NA,x$occurrenceCount),
		georeferencedCount=ifelse(is.null(x$georeferencedCount),NA,x$georeferencedCount),
		scientificNameMatches=ifelse(is.null(x$scientificNameMatches),NA,x$scientificNameMatches),
		commonNameMatches=ifelse(is.null(x$commonNameMatches),NA,x$commonNameMatches),
		commonName=ifelse(is.null(x$commonName),NA,x$commonName), 
		matchedNames=ifelse(is.null(x$matchedNames),NA,x$matchedNames),
		ankId=ifelse(is.null(x$rankId),NA,x$rankId),
		rankString=ifelse(is.null(x$rankString),NA,x$rankString),
		left=ifelse(is.null(x$left),NA,x$left),
		right=ifelse(is.null(x$right),NA,x$right)
	)) } #function to return a vector of key data
	
	out = do.call('rbind',lapply(out$autoCompleteList,FUN='tfun')) #create a dataframe of the species found
	
	if (is.null(out)) {
		warning('no matches found'); return(NULL) #if no matches found, warn and return NULL
	} else { 
		return(as.data.frame(out)) #return the matches that were found
	}
}
