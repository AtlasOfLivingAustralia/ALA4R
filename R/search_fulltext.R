#' Full text search
#' 
#' Performs a search across all objects, and selects the closest matches
#'
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/}
#'  
#' @param taxon string: (required) a character string for the taxon of interest
#' @param fq string: (optional) character string or vector of strings, specifying filters to be applied to the 
#' original query. These are of the form "INDEXEDFIELD:VALUE" e.g. "kingdom:Fungi". 
#' See ala_fields("general") for all the fields that are queryable. 
#' NOTE that fq matches are case-sensitive, but sometimes the entries in the fields are 
#' not consistent in terms of case (e.g. kingdom names "Fungi" and "Plantae" but "ANIMALIA"). 
#' fq matches are ANDed by default (e.g. c("field1:abc","field2:def") will match records that have 
#' field1 value "abc" and field2 value "def"). To obtain OR behaviour, use the form c("field1:abc 
#' OR field2:def")
#' @param start numeric: (optional) (positive integer) start offset for the results
#' @param page_size numeric: (optional) (positive integer) maximum number of records to return. Defaults to the server-side value - currently 10
#' @param sort_by string: (optional) field to sort on
#' @param sort_dir string: (optional) sort direction, either "asc" or "desc"
#' 
#' @return a named list with the components "meta" (search metadata), "facets" (search facet results), and "data" (the search results, in the form of a dataframe).
# The data component is a dataframe with columns:
# \itemize{
# \item{guid}  
# \item{name}  
# \item{idxtype}
# \item{score} 
# \item{parentGuid}
# \item{commonName}  
# \item{nameComplete}
# \item{commonNameSingle}  
# \item{hasChildren} 
# \item{rank} 
# \item{rankId} 
# \item{rawRank} 
# \item{conservationStatus} 
# \item{conservationStatusAUS}
# \item{isAustralian}
# \item{highlight}
# \item{image} 
# \item{thumbnail} 
# \item{left} 
# \item{right} 
# \item{kingdom}
# \item{phylum} 
# \item{class} 
# \item{order} 
# \item{class} 
# \item{order} 
# \item{family} 
# \item{genus} 
# \item{author}
# \item{linkIdentifier} 
# \item{occCount} 
# \item{imageSource} 
# \item{imageCount} 
# \item{isEcluded}
# \item{imageUrl} 
# \item{largeImageUrl} 
# \item{smallImageUrl} 
# \item{thumbnailUrl} 
# \item{imageMetadataUrl} 
# \item{acceptedConceptName} 
# \item{synonomyRelationship} 
# \item{synonomyDescription}
# }
#' 
#' @examples
#'  # find information ALA holds on red kangaroo
#'  search_fulltext("red kangaroo")
#'  search_fulltext("Macropus rufus")
#'  search_fulltext("urn:lsid:biodiversity.org.au:afd.taxon:31a9b8b8-4e8f-4343-a15f-2ed24e0bf1ae")
#'
#'  # find genus names like "Oenanthe"
#'  search_fulltext("oenanthe",sort_by="kingdom",fq="rank:genus")
#' 
#' @export
search_fulltext <- function(taxon,fq,start,page_size,sort_by,sort_dir) {
        ##taxon = clean_string(taxon) #clean up the taxon name ## don't think it makes sense to do this for a fulltext search
        base_url="http://bie.ala.org.au/ws/search.json"
        this_url=parse_url(base_url)
        this_query=list(q=taxon)
        if (!missing(fq)) {
            assert_that(is.character(fq))
            ## can have multiple fq parameters, need to specify in url as fq=a:b&fq=c:d&fq=...
            check_fq(fq,type="general") ## check that fq fields are valid
            fq=as.list(fq)
            names(fq)=rep("fq",length(fq))
            this_query=c(this_query,fq)
        }
        if (!missing(start)) {
            assert_that(is.count(start))
            this_query$start=start
        }
        if (!missing(page_size)) {
            assert_that(is.count(page_size))
            this_query$pageSize=page_size
        }
        if (!missing(sort_by)) {
            assert_that(is.string(sort_by))
            ## check that this is a valid field
            valid_fields=ala_fields("general")$name
            if (! sort_by %in% valid_fields) {
                stop(sort_by," is not a valid field for sort_by. See ala_fields(\"general\")")
            }
            this_query$sort=sort_by
        }
        if (!missing(sort_dir)) {
            assert_that(is.string(sort_dir))
            sort_dir=match.arg(tolower(sort_dir),c("asc","desc"))
            this_query$dir=sort_dir
        }
        this_url$query=this_query
        this_url=build_url(this_url)
        x=cached_get(url=this_url,type="json")
        x=as.list(x)

        ## reformat data into a more concise structure
        ## x is a named list. Each component of that list is itself a named list with a single element "searchResults".
        ## first collate the metadata, which is everything except "results" and "facetResults" elements
        out=list(meta=x[!names(x) %in% c("results","facetResults")])
        ## collapse the singleton "searchResults" structures
        out$meta=lapply(out$meta,function(z)z$searchResults)
        out$data=x$results$searchResults
        if (is.list(out$data) & length(out$data)<1) {
            ## no results
            out$data=data.frame()
        } else {
            ## rename some columns
            names(out$data)[names(out$data)=="classs"]="class"
            ## remove unwanted columns
            xcols=setdiff(names(out$data),unwanted_columns("general"))
            out$data=out$data[,xcols]
        }
        out$facets=x$facetResults$searchResults

        class(out)="search_fulltext"
        out
}

#"@export
print.search_fulltext=function(x,...) {
    cat(sprintf("Search metadata:\n"))
    print(format(as.data.frame(x$meta)))
    cat(sprintf("\nFacet results:\n"))
    print(format(x$facets))    
    cat(sprintf("\nSearch results:\n"))
    #cols=names(x)
    #if (attr(x,"output_format")=="simple") {
    #    cols=intersect(c("searchTerm","name","commonName","rank","guid"),cols)
    #}
    #print(as.matrix(format.data.frame(x[,cols],na.encode=FALSE)))
    print(format(x$data))
    invisible(x)
}
    
