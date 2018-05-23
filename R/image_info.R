#' Fetch information about an image, given its image ID
#'
#' Note that there is currently no web service that provides image information, and so we are scraping results from pages of the form http://images.ala.org.au/image/details?imageId=id. This web scraping may be fragile, and will be replaced by a web-service-based function when one becomes available.
#' 
#' @param id character: IDs of images (e.g. as returned by \code{\link{occurrences}}  in the imageUrl column). Each ID will be of a format something like "84654e14-dc35-4486-9e7c-40eb2f8d3faa"
#' @param image_number character or numeric: ID numbers of images (e.g. as returned by ALA's image search at \url{http://images.ala.org.au/}. Each image_number will be of a format something like 122218480
#' @param verbose logical: show additional progress information? [default is set by ala_config()]
#' @return A data.frame with one row per \code{id}, and at least the columns imageIdentifier and imageURL
#' @seealso \code{\link{ala_config}}, \code{\link{occurrences}}
#' @examples
#' \dontrun{
#' image_info(c("84654e14-dc35-4486-9e7c-40eb2f8d3faa",
#'   "39836d30-0761-473d-bac2-9ed9494fd37e",
#'   "this-is-an-invalid-image-id"))
#' }
#' @export image_info

image_info <- function(id, image_number, verbose=ala_config()$verbose) {
    if (missing(id) & missing(image_number)) stop("image id or number must be provided")
    if (!missing(id)) assert_that(is.character(id))
    if (!missing(image_number)) assert_that(is.character(image_number) || is.numeric(image_number))
    assert_that(is.flag(verbose), !is.na(verbose))
    if (is.null(getOption("ALA4R_server_config")$base_url_images) || getOption("ALA4R_server_config")$base_url_images=="") {
        stop("No URL to the image database has been configured: see base_url_images in ", getOption("ALA4R_server_config")$config_function)
    }
    ## grab each image info web page
    if (!missing(id)) {
        non_empty <- nzchar(id) & !is.na(id)
        this_url <- paste0(getOption("ALA4R_server_config")$base_url_images, "image/details?imageId=", id[non_empty])
    } else if (!missing(image_number)) {
        non_empty <- if (is.character(image_number)) nzchar(image_number) & !is.na(image_number) else !is.na(image_number)
        this_url <- paste0(getOption("ALA4R_server_config")$base_url_images, "image/details/", image_number[non_empty])
    }        
    ## we get a 500 error if we ask for a non-existent image ID, so catch these errors with the on_server_error parm
    pages <- vapply(this_url, function(z) paste0(cached_get(URLencode(z), type="text", verbose=verbose, on_server_error=function(z)NULL), collapse=" "), FUN.VALUE="", USE.NAMES=FALSE)
    ## keep only the table from each, which has the actual image details
    pages <- vapply(pages, function(z) { if (nrow(str_locate(z, "<table"))==1) str_extract(z, regex("<table.*</table>", dotall=TRUE)) else stop("image information page in unexpected format. ", getOption("ALA4R_server_config")$notify) }, FUN.VALUE="", USE.NAMES=FALSE)        
    temp <- extract_image_detail(pages, ".*?")
    ## this only has rows where the imageIdentifier had a valid match
    ## reconstruct a data.frame with the same row ordering as the input ids
    if (is.null(temp) || nrow(temp)<1) {
        ## no matches, return data.frame with supplied ids but NA-columns
        ## note that this isn't the complete set of columns that one gets when an id is actually matched
        data.frame(imageIdentifier=id, imageURL=NA_character_, stringsAsFactors=FALSE)
    } else {
        out <- data.frame(imageIdentifier=id, stringsAsFactors=FALSE)
        merge(out, temp, all.x=TRUE, by="imageIdentifier", sort=FALSE)
    }
}

## worker function to extract detail from the scraped html pages
extract_image_detail <- function(html, property_name_regex) {
    regex_str <- paste0("<td[^>]*>(", property_name_regex, ")</td>\\s*<td[^>]*>(.*?)</t[rd]")
    out <- str_match_all(html, regex(regex_str, ignore_case=TRUE, dotall=TRUE))
    ## out is a list, where each element is a character matrix where nrows = number of matches to property_name_regex and ncols=3
    ##  if image ID was invalid, then nrows=1 and each entry is NA
    do.call(rbind, lapply(out, function(z) {
        if (!(nrow(z)==1 && is.na(z[, 3]))) {
            this <- as.data.frame(t(str_trim(z[, 3])), stringsAsFactors=FALSE)
            names(this) <- rename_variables(z[, 2], type="occurrence")
            this
        }
    }))
}
