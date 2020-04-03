#' Retrieve image information using image ids, with an option to download
#' 
#' @references \itemize{
#' \item Associated ALA web service for images \url{https://images.ala.org.au/ws}
#' }
#' 
#' @param id character: IDs of images to be downloaded as single string or
#' vector of strings
#' @param download logical: if TRUE download all images and add location to
#' dataframe
#' @param download_path string: (optional) filepath to download images to.
#' If not given and download param is TRUE, will create an images
#' folder
#' @param verbose logical: show additional progress information?
#' [default is set by ala_config()]
#' @return Data frame of image results
#' 
#' @examples 
#' \dontrun{
#' ## Retrieve infomation about an image and download
#' images(id="da5fe120-e213-4cd6-9c5f-62346ed2e466", download=TRUE)
#' }
#' @export images

images <- function(id, download=FALSE, download_path,
                         verbose=ala_config()$verbose) {
  
  this_query <- list()
  
  assert_that(is.flag(verbose))
  
  if(missing(id)) {
    stop("Please provide a list of images ids to retrieve")
  }
  
  assert_that(is.character(id))
  id_str  <- paste(id, collapse = '","')
  
  image_data <- do.call(rbind, lapply(id, function(z) {
    this_url <- paste0(getOption("ALA4R_server_config")$base_url_images, "ws/image/", z)
    data <- cached_get(URLencode(this_url), type="json", verbose=verbose, on_server_error = function(z){NULL}, on_client_error =  function(z){NULL})
    if (!is.null(data)) {
      data$imageIdentifier <- z
      if (is.null(data$recognisedLicence)) {
        data$recognisedLicence <- NA
      }
    }
    # add basic info if request fails
    else {
      data <- list(success = FALSE, imageUrl = this_url, imageIdentifier = z)
    }
    
    df <- as.data.frame(data, stringsAsFactors = FALSE)
    cols <- c('imageIdentifier', 'imageUrl', 'success','mimeType',
              'originalFileName','sizeInBytes','rights','rightsHolder',
              'dateUploaded','dateTaken','tileUrlPattern','mmPerPixel',
              'height','width','tileZoomLevels','description','title',
              'creator','license','recognisedLicence','recognisedLicence.acronym',
              'recognisedLicence.id','recognisedLicence.imageUrl',
              'recognisedLicence.name', 'recognisedLicence.url',
              'dataResourceUid','occurrenceID')
    df[cols[!(cols %in% colnames(df))]] = NA
    return(df)
  }))

  if (download) {
    if (missing(download_path)) {
      message(sprintf("No download path specified.
                  Media will be downloaded in %s",
                      file.path(getwd(),'media')))
      download_path <- file.path(getwd(),'media')
    }
    download_images(data=image_data,media_dir=download_path,verbose=verbose)
  }
  
  return(image_data)
}


download_images <- function(data, media_dir, verbose=verbose, sounds = FALSE) {
  
  assert_that(!missing(media_dir))
  
  if(!file.exists(media_dir)) {
    message(sprintf('Media directory does not exist, creating directory %s',
                    media_dir))
    dir.create(media_dir)
  }
  
  if (!missing(data)) {
    for(r in 1:nrow(data)) {
      id <- data[r,'imageIdentifier']
      base_url <- getOption("ALA4R_server_config")$base_url_images
      url <- build_url_from_parts(base_url, c('image',id, 'original'))
      if (sounds && data[r,'fileType'] == 'sound') {
        ext <- '.mp4'
      }
      else {
        ext <- '.jpg'
      }
      out_path <- file.path(media_dir,paste0(id,ext))
      download_to_file(url, out_path, verbose = verbose)
      
      # throttle download requests
      Sys.sleep(1)
    }
  }
  
}
