#' Fetch information about an image, given its image ID
#'
#' Note that there is currently no web service that provides image information, and so we are scraping results from pages of the form http://images.ala.org.au/image/details?imageId=id. This web scraping may be fragile, and will be replaced by a web-service-based function when one becomes available.
#' 
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' 
#' @param id character: IDs of images (e.g. as returned by \code{\link{occurrences}}  in the imageUrl column). Each ID will be of a format something like "84654e14-dc35-4486-9e7c-40eb2f8d3faa"
#' @param verbose logical: show additional progress information? [default is set by ala_config()]
#' @return A data.frame with one row per \code{id}, and at least the columns imageIdentifier and imageURL
#' @seealso \code{\link{ala_config}} \code{\link{occurrences}}
#' @examples
#' image_info(c("84654e14-dc35-4486-9e7c-40eb2f8d3faa","39836d30-0761-473d-bac2-9ed9494fd37e","this-is-an-invalid-image-id"))
#'
#' @export image_info

image_info=function(id,verbose=ala_config()$verbose) {
    if (missing(id)) {
        return(data.frame())
    }
    assert_that(is.character(id))
    assert_that(is.flag(verbose))
    non_empty=nchar(id)>0 & !is.na(id)
    ## grab each image info web page
    this_url=paste0("http://images.ala.org.au/image/details?imageId=",id[non_empty])
    ## we get a 500 error if we ask for a non-existent image ID, so catch these errors with the on_server_error parm
    pages=sapply(this_url,function(z) paste0(cached_get(URLencode(z),type="text",verbose=verbose,on_server_error=function(z)NULL),collapse=" "))
    ## keep only the table from each, which has the actual image details
    if (packageVersion("stringr")<"1.0") {    
        pages=sapply(pages,function(z) { if (nrow(str_locate(z,"<table"))==1) str_extract(z,"<table.*</table>") else stop("image information page in unexpected format: please notify the ALA4R maintainers") })
    } else {
        pages=sapply(pages,function(z) { if (nrow(str_locate(z,"<table"))==1) str_extract(z,regex("<table.*</table>",dotall=TRUE)) else stop("image information page in unexpected format: please notify the ALA4R maintainers") })
    }        
        
    out=extract_image_detail(pages,".*?")
    ## this will have NA values for imageIdentifier where id was not valid (because the imageIdentifier value comes from the scraped HTML, which won't exist for invalid id). Replace with input ids
    out$imageIdentifier=id
    out
}

## helper function to extract detail from the scraped html pages
extract_image_detail=function(html,property_name_regex) {
    regex_str=paste0("<td[^>]*>(",property_name_regex,")</td>\\s*<td[^>]*>(.*?)</t[rd]")
    if (packageVersion("stringr")<"1.0") {
        out=str_match_all(html,ignore.case(regex_str))
        ## For stringr version < 1, elements will be empty character matrices if there are no matches
        out=lapply(out,function(z) if (length(dim(z))<2) matrix("",nrow=0,ncol=3) else z)
    } else {
        out=str_match_all(html,regex(regex_str,ignore_case=TRUE))
    }
    ## out is a list, where each element is a character matrix where nrows = number of matches to property_name_regex and ncols=3
    ##  if image ID was invalid, then nrows=1 and each entry is NA
    ldply(out,function(z) {
        if (nrow(z)==1 && is.na(z[,3])) {
            data.frame(imageURL=NA)
        } else {
            this=as.data.frame(t(str_trim(z[,3])),stringsAsFactors=FALSE)
            names(this)=rename_variables(z[,2],type="occurrence")
            this
        }} )
}
