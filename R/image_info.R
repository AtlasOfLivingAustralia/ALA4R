#' Fetch information about an image, given its image ID
#'
#' 
#' @param id character: IDs of images (e.g. as returned by 
#' \code{\link{occurrences}}  in the imageUrl column). Each ID will be of a 
#' format something like "84654e14-dc35-4486-9e7c-40eb2f8d3faa"
#' @param verbose logical: show additional progress information? 
#' [default is set by ala_config()]
#' @return A data.frame with one row per \code{id}, and at least the columns 
#' imageIdentifier and imageURL
#' @seealso \code{\link{ala_config}}, \code{\link{occurrences}}
#' @examples
#' \dontrun{
#' image_info(c("84654e14-dc35-4486-9e7c-40eb2f8d3faa",
#'   "39836d30-0761-473d-bac2-9ed9494fd37e",
#'   "this-is-an-invalid-image-id"))
#' }
#' @export image_info
#' 
image_info <- function(id, verbose=ala_config()$verbose) {
  
    if (as.character(match.call()[[1]]) == "image_info") {
      warning("image_info() has been renmaed to images() and now can also 
              download images. Please use images() instead of image_info()", 
              call. = FALSE)
    }
  
    if (missing(id)) stop("image id must be provided")
    assert_that(is.character(id))
    assert_that(is.flag(verbose), !is.na(verbose))
    if (is.null(getOption("ALA4R_server_config")$base_url_images) || 
        getOption("ALA4R_server_config")$base_url_images=="") {
        stop("No URL to the image database has been configured: see
             base_url_images in ", 
             getOption("ALA4R_server_config")$config_function)
    }
    
    image_data <- do.call(rbind, lapply(id, function(z) {
      this_url <- paste0(getOption("ALA4R_server_config")$base_url_images, 
                         "ws/image/", z)
      data <- cached_get(URLencode(this_url), type="json", verbose=verbose,
                         on_client_error=function(z)NULL, 
                         on_server_error=function(z)NULL)
      if (!is.null(data)) {
        data$imageIdentifier <- z
        if (is.null(data$recognisedLicence)) {
          data$recognisedLicence <- NA
        }
      }
      # add basic info if request fails
      else {
        data <- list(success = FALSE, imageUrl = this_url, 
                     imageIdentifier = z)
      }
      
      df <- as.data.frame(data, stringsAsFactors = FALSE)
      cols <- c('imageIdentifier', 'imageUrl', 'success','mimeType',
                'originalFileName','sizeInBytes','rights','rightsHolder',
                'dateUploaded','dateTaken','tileUrlPattern','mmPerPixel',
                'height','width','tileZoomLevels','description','title',
                'creator','license','recognisedLicence',
                'recognisedLicence.acronym','recognisedLicence.id',
                'recognisedLicence.imageUrl','recognisedLicence.name', 
                'recognisedLicence.url', 'dataResourceUid','occurrenceID')
      df[cols[!(cols %in% colnames(df))]] = NA
      return(df)
    }))
    
    return(image_data)
}

#' @export
#' @rdname images
images <- image_info