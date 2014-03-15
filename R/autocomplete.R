#' Auto Complete Search
#' 
#' An autocomplete search for identifying species names & identifiers used at the ALA based
#' on matches from a PARTIAL NAME. atocomplete can generate a dataframe of scientific and 
#' common names that can be used for further analysis in R.
#' 
#' If the scientific name, common name or LSID are known, use fulltext_search
#' 
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/}

#' @param taxon a character string of part of the scientific, common name of the taxa
#' @param geoOnly logical: if TRUE, only results that have geospatial occurrence records will be included
#' @param idxType string: The index type to limit. Values include: TAXON REGION COLLECTION INSTITUTION DATASET
#' @param limit the maximum number of matches returned (defaults to the server-side value - currently 10)
#' @return A dataframe of taxa given the partial matches where columns are identified as: 
#' \item{guid}{} \item{name}{} \item{occurrenceCount}{}
#' \item{georeferencedCount}{} \item{scientificNameMatches}{}
#' \item{commonNameMatches}{} 
#' \item{commonName}{} 
#' \item{matchedNames}{} \item{ankId}{} \item{rankString}{} \item{left}{} \item{right}{}
#'
#' @examples
#' \dontrun{
#' # find information ALA holds on red kangaroo (Macropus rufus)
#' autocomplete("red kangaroo")
#' autocomplete("Macropus rufus",geoOnly=TRUE)
#' }
#' 
#' @export autocomplete
autocomplete=function(taxon,geoOnly=FALSE,idxType=NULL,limit=NULL) {
    is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
    if (!is.null(limit)) {
        if (class(limit) != 'numeric' | length(limit) > 1 | limit<1 | !is.wholenumber(limit)) {
            stop('limit must be a single integer value greater than 0') #check limit is integer >0 and single value
        }
    }
    if (!is.null(idxType)) {
        idxType=toupper(idxType)
        match.arg(idxType,c("TAXON","REGION","COLLECTION","INSTITUTION","DATASET"))
    }
    taxon = clean_string(taxon) #clean up the taxon name
    taxon = gsub(' ','+',taxon) #replace spaces with + to force both terms in the search
	
    base_url=paste(ala_config()$base_url_bie,"search/auto.json",sep="") #define the base URL string
    this_query=list(q=taxon)
    if (!is.null(limit)) {
        this_query$limit=limit
    }
    if (geoOnly) {
        this_query$geoOnly="true" #Check for taxa that have locations (some have no location)
    }
    if (!is.null(idxType)) {
        this_query$idxType=idxType
    }
    this_url=parse_url(base_url)
    this_url$query=this_query
        	
    out = cached_get(url=build_url(this_url),type="json") #get the data
    out = out[[1]] #looking at the data
    if (length(out)<1) {
        ## no results
        data.frame()
    } else {
        ##need to collapse matchednames fields if there was more than a single matched name
        for (ii in 1:length(out)) {
            for (jj in 1:length(out[[ii]])) {
                if (length(out[[ii]][[jj]]) > 1) {
                    out[[ii]][[jj]] = paste(out[[ii]][[jj]],collapse=', ')
                }
            }
        }
        #if (!identical(find("fromJSON"),"package:jsonlite")) {
        #    out = do.call('rbind.fill',lapply(out,function(x) {as.data.frame(rbind(x))})) #define the output as a data.frame
        #}
        out
    }
}
