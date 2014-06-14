#' Full text search
#' 
#' Performs a search across all objects, and selects the closest matches. Generally, the user will provide the search term via the \code{query} parameter, with optional filtering via \code{fq}.
#'
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' @references \url{http://api.ala.org.au/}
#'  
#' @param query string: the search term
#' @param fq string: (optional) character string or vector of strings, specifying filters to be applied to the 
#' original query. These are of the form "INDEXEDFIELD:VALUE" e.g. "kingdom:Fungi". 
#' See \code{ala_fields("general")} for all the fields that are queryable. 
#' NOTE that fq matches are case-sensitive, but sometimes the entries in the fields are 
#' not consistent in terms of case (e.g. kingdom names "Fungi" and "Plantae" but "ANIMALIA"). 
#' fq matches are ANDed by default (e.g. c("field1:abc","field2:def") will match records that have 
#' field1 value "abc" and field2 value "def"). To obtain OR behaviour, use the form c("field1:abc OR field2:def")
#' @param output_format string: controls the print method for the "data" component of the returned object. Either "complete" (the complete data structure is displayed), or "simple" (a simplified version is displayed). Note that the complete data structure exists in both cases: this option only controls what is displayed when the object is printed to the console. The default output format is "simple"
#' @param start numeric: (optional) (positive integer) start offset for the results
#' @param page_size numeric: (optional) (positive integer) maximum number of records to return. Defaults to the server-side value - currently 10
#' @param sort_by string: (optional) field to sort on
#' @param sort_dir string: (optional) sort direction, either "asc" or "desc"
#' @seealso \code{\link{ala_fields}}
#' @return a named list with the components "meta" (search metadata), "facets" (search facet results), and "data" (the search results, in the form of a dataframe). The contents (column names) of the data frame will vary depending on the details of the search and the results, but should contain at least the columns \code{guid}, \code{name}, \code{score}, \code{commonName}, \code{rank}, \code{author}, \code{isAustralian}, and \code{occCount}.
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
# TODO: if columns are renamed, check the documentation "minimal columns" list (expect "name" might change)

search_fulltext <- function(query,fq,output_format="simple",start,page_size,sort_by,sort_dir) {
    output_format=match.arg(tolower(output_format),c("simple","complete"))
    base_url="http://bie.ala.org.au/ws/search.json"
    this_url=parse_url(base_url)
    this_query=list()
    if (!missing(query)) {
        this_query$q=query
    }
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
    attr(out,"output_format")=output_format
    out
}

#'@S3method print search_fulltext
"print.search_fulltext"=function(x,...) {
    cat(sprintf("Search metadata:\n"))
    print(format(as.data.frame(x$meta)))
    cat(sprintf("\nFacet results:\n"))
    print(format(x$facets))    
    cat(sprintf("\nSearch results:\n"))
    cols=names(x$data)
    if (attr(x,"output_format")=="simple") {
        cols=intersect(c("name","commonName","rank","guid"),cols)
    }
    print(as.matrix(format.data.frame(x$data[,cols],na.encode=FALSE)))
    #print(format(x$data))
    invisible(x)
}
    
