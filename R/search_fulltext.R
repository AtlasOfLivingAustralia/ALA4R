#' Full text search
#' 
#' Performs a search across all objects, and selects the closest matches
#'
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/}
#'  
#' @param taxon string: a character string for the taxon of interest
#' @param fq string: a character string or vector of strings, specifying filters to be applied to the 
#' original query. These are of the form "INDEXEDFIELD:VALUE" e.g. "kingdom:Fungi". 
#' See ala_fields("general") for all the fields that are queryable. 
#' NOTE that fq matches are case-sensitive, but sometimes the entries in the fields are 
#' not consistent in terms of case (e.g. kingdom names "Fungi" and "Plantae" but "ANIMALIA"). 
#' fq matches are ANDed by default (e.g. c("field1:abc","field2:def") will match records that have 
#' field1 value "abc" and field2 value "def"). To obtain OR behaviour, use the form c("field1:abc 
#' OR field2:def")
#' @param start numeric: (positive integer) start offset for the results
#' @param pageSize numeric: (positive integer) maximum number of records to return
#' @param sort_by string: field to sort on
#' @param sort_dir string: sort direction, either "asc" or "desc"
#' 
#' @return a named list, including the results component which is a dataframe of information with columns being:
#' \itemize{
#' \item{guid}  
#' \item{name}  
#' \item{idxtype}
#' \item{score} 
#' \item{parentGuid}
#' \item{commonName}  
#' \item{nameComplete}
#' \item{commonNameSingle}  
#' \item{hasChildren} 
#' \item{rank} 
#' \item{rankId} 
#' \item{rawRank} 
#' \item{conservationStatus} 
#' \item{conservationStatusAUS}
#' \item{isAustralian}
#' \item{highlight}
#' \item{image} 
#' \item{thumbnail} 
#' \item{left} 
#' \item{right} 
#' \item{kingdom}
#' \item{phylum} 
#' \item{class} 
#' \item{order} 
#' \item{class} 
#' \item{order} 
#' \item{family} 
#' \item{genus} 
#' \item{author}
#' \item{linkIdentifier} 
#' \item{occCount} 
#' \item{imageSource} 
#' \item{imageCount} 
#' \item{isEcluded}
#' \item{imageUrl} 
#' \item{largeImageUrl} 
#' \item{smallImageUrl} 
#' \item{thumbnailUrl} 
#' \item{imageMetadataUrl} 
#' \item{acceptedConceptName} 
#' \item{synonomyRelationship} 
#' \item{synonomyDescription}
#' }
#' 
#'  TODO: Need a realistic fq example (and I can't generate it)
#'  
#' @examples
#'  # find information ALA holds on red kangaroo
#'  search_fulltext("red kangaroo")
#'  search_fulltext("Macropus rufus")
#'  search_fulltext("urn:lsid:biodiversity.org.au:afd.taxon:31a9b8b8-4e8f-4343-a15f-2ed24e0bf1ae")
#' 
#' @export
search_fulltext <- function(taxon,fq=NULL,start=NULL,pageSize=NULL,sort_by=NULL,sort_dir=NULL) {
	taxon = clean_string(taxon) #clean up the taxon name
        base_url="http://bie.ala.org.au/ws/search.json"
        this_url=parse_url(base_url)
        this_query=list(q=taxon)
        if (!is.null(fq)) {
            assert_that(is.character(fq))
            ## can have multiple fq parameters, need to specify in url as fq=a:b&fq=c:d&fq=...
            fq=as.list(fq)
            names(fq)=rep("fq",length(fq))
            this_query=c(this_query,fq)
        }
        if (!is.null(start)) {
            assert_that(is.count(start))
            this_query$start=start
        }
        if (!is.null(pageSize)) {
            assert_that(is.count(pageSize))
            this_query$pageSize=pageSize
        }
        if (!is.null(sort_by)) {
            assert_that(is.string(sort_by))
            this_query$sort=sort_by
        }
        if (!is.null(sort_dir)) {
            assert_that(is.string(sort_dir))
            sort_dir=match.arg(tolower(sort_dir),c("asc","desc"))
            this_query$dir=sort_dir
        }
        this_url$query=this_query
        this_url=build_url(this_url)
        x=cached_get(url=this_url,type="json")
        #if using jsonlite {
            x=as.list(x)
        #} else {
        #    x=x[[1]]
        #    ## reformat results to data frame
        #    x$results=rbind.fill(lapply(x$results,as.data.frame)) ## convert each element of results into data frame, then combine
        #}

        ## reformat data into a more concise structure
        ## x is a named list. Each component of that list is itself a named list with a single element "searchResults".
        ## first collate the metadata, which is everything except "results" and "facetResults" elements
        out=list(meta=x[!names(x) %in% c("results","facetResults")])
        ## collapse the singleton "searchResults" structures
        out$meta=lapply(out$meta,function(z)z$searchResults)
        out$data=x$results$searchResults
        out$facets=x$facetResults$searchResults
        out
}

