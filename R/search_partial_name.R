#' A partial-name search for identifying species names & identifiers used at the ALA is based
#' on matches from a PARTIAL NAME. search_partial_name can generate a dataframe of scientific and 
#' common names that can be used for further analysis in R.
#' 
#' If the scientific name, common name or LSID of the taxon are known, use search_names
#' 
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/}

#' @param taxon a character string of part of the scientific, common name of the taxa
#' @param geoOnly logical: if TRUE, only results that have geospatial occurrence records will be included
#' @param idxType string: the index type to limit. Values include: TAXON REGION COLLECTION INSTITUTION DATASET
#' @param limit numeric: the maximum number of matches returned (defaults to the server-side value - currently 10)
#' @return A dataframe of taxa given the partial matches where columns are identified as: 
#' \itemize{
#' \item{guid}{} 
#' \item{name}{} 
#' \item{occurrenceCount}{}
#' \item{georeferencedCount}{} 
#' \item{scientificNameMatches}{}
#' \item{commonNameMatches}{} 
#' \item{commonName}{} 
#' \item{matchedNames}{} 
#' \item{rankId}{} 
#' \item{rankString}{} 
#' \item{left}{} 
#' \item{right}{}
#' } 
#'
#' @examples
#' # find information ALA holds on red kangaroo (Macropus rufus)
#' tt = search_partial_name("red kangaroo")
#' tt
#' #show all information stored in the object
#' str(tt)
#' as.matrix(tt)
#' 
#' #retrieve only species with geolocations
#' search_partial_name("Macropus rufus",geoOnly=TRUE)
#' 
#' @export search_partial_name

# TODO: Definitions of left,right? Are rankId, left and right needed? - remove from print.* method

search_partial_name=function(taxon,geoOnly=FALSE,idxType=NULL,limit=NULL) {
    assert_that(is.string(taxon))
    taxon = clean_string(taxon) #clean up the taxon name
    taxon = gsub(' ','+',taxon) #replace spaces with + to force both terms in the search
	
    base_url=paste(ala_config()$base_url_bie,"search/auto.json",sep="") #define the base URL string
    this_query=list(q=taxon)
    if (!is.null(limit)) {
        assert_that(is.count(limit))  #check limit is integer >0 and single value
        this_query$limit=limit
    }
    if (!is.null(geoOnly)) {
        assert_that(is.flag(geoOnly),noNA(geoOnly))
        if (geoOnly) {
            this_query$geoOnly="true" #Check for taxa that have locations (some have no location)
        }
    }
    if (!is.null(idxType)) {
        assert_that(is.string(idxType))
        idxType=match.arg(toupper(idxType),c("TAXON","REGION","COLLECTION","INSTITUTION","DATASET"))
        this_query$idxType=idxType
    }
    this_url=parse_url(base_url)
    this_url$query=this_query
        	
    out = cached_get(url=build_url(this_url),type="json") #get the data
    out = out[[1]] #looking at the data
	
    if (length(out)<1) {
        ## no results
        warning('no matched taxa')
		return(NULL)
    } else {
        ## matchedNames, commonNameMatches, and scientificNameMatches are all lists of strings
        ## convert each list to single string
        for (ii in 1:nrow(out)) {
            out$matchedNames[ii]=paste(out$matchedNames[[ii]],collapse=", ")
            out$scientificNameMatches[ii]=paste(out$scientificNameMatches[[ii]],collapse=", ")
            out$commonNameMatches[ii]=paste(out$commonNameMatches[[ii]],collapse=", ")
        }
        out$matchedNames=unlist(out$matchedNames)
        out$scientificNameMatches=unlist(out$scientificNameMatches)
        out$commonNameMatches=unlist(out$commonNameMatches)
    }
	class(out) <- c('search_partial_name',class(out)) #add the search_partial_name class
	return(out)
}

#' @export
"print.search_partial_name" <- function(x, ...)
{
	if (empty(as.data.frame(x$scientificNameMatches))) {
		m <- as.matrix(format.data.frame(x[,c('matchedNames','name','rankString')], na.encode = FALSE))
	} else {
		m <- as.matrix(format.data.frame(x[,c('name','commonName','rankString')], na.encode = FALSE))
	}
	print(m)
    invisible(x)
}


