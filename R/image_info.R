#' Fetch information about an image, given its image ID
#'
#' Note that there is currently no web service that provides image information, and so we are scraping results from pages of the form http://images.ala.org.au/image/details?imageId=id. This web scraping may be fragile, and will be replaced by a web-service-based function when one becomes available.
#' 
#' @author Atlas of Living Australia \email{support@@ala.org.au}
#' 
#' @param id character: IDs of images (e.g. as returned by \code{\link{occurrences}}  in the imageUrl column). Each ID will be of a format something like "84654e14-dc35-4486-9e7c-40eb2f8d3faa"
#' @param verbose logical: show additional progress information? [default is set by ala_config()]
#' @return A data.frame with one row per \code{id}
#' @seealso \code{\link{ala_config}} \code{\link{occurrences}}
#' @examples
#' image_info(c("84654e14-dc35-4486-9e7c-40eb2f8d3faa","b8344134-254d-4116-a98d-4a37e7362a4e"))
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
    pages=sapply(this_url,function(z) paste0(cached_get(URLencode(z),type="text",verbose=verbose),collapse=" "))
    ## keep only the table from each, which has the actual image details
    if (packageVersion("stringr")<"1.0") {    
        pages=sapply(pages,function(z) { if (nrow(str_locate(z,"<table"))==1) str_extract(z,"<table.*</table>") else stop("image information page in unexpected format: please notify the ALA4R maintainers") })
    } else {
        pages=sapply(pages,function(z) { if (nrow(str_locate(z,"<table"))==1) str_extract(z,regex("<table.*</table>",dotall=TRUE)) else stop("image information page in unexpected format: please notify the ALA4R maintainers") })
    }        
        
    ##out=data.frame(id=id)
    ## image properties look something like <td class="property-name">Image URL</td> <td class="property-value">http://images.ala.org.au/store/a/a/f/3/84654e14-dc35-4486-9e7c-40eb2f8d3faa/original </tr>
    ##temp=extract_image_detail(pages,"Image URL")
    ##out$imageUrl[non_empty]=temp[,1]

    ##out=dlply(pages,function(z)
    
    extract_image_detail(pages,".*?")
}

## helper function to extract detail from the scraped html pages
extract_image_detail=function(html,property_name_regex) {
    regex_str=paste0("<td[^>]*>(",property_name_regex,")</td>\\s*<td[^>]*>([^>]*)</")
    if (packageVersion("stringr")<"1.0") {
        out=str_match_all(html,ignore.case(regex_str))
        ## For stringr version < 1, elements will be empty character matrices if there are no matches
        out=lapply(out,function(z) if (length(dim(z))<2) matrix("",nrow=0,ncol=3) else z)
    } else {
        out=str_match_all(html,regex(regex_str,ignore_case=TRUE))
    }
    ## out is a list, where each element is a character matrix where nrows = number of matches to property_name_regex and ncols=3
    out=ldply(out,function(z) { this=as.data.frame(t(str_trim(z[,3])),stringsAsFactors=FALSE); names(this)=z[,2]; this } )
    names(out)=rename_variables(names(out),type="occurrence")
    out
}
