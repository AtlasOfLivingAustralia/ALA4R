#' Retrieve images uses image ids
#' 
#' @references \itemize{
#' \item Associated ALA web service for image retrieval
#' \url{https://images.ala.org.au/ws#/Search/search}
#' }
#' 
#' @param id character: IDs of images to be downloaded as single string or
#' vector of strings
#' @param fq string: (optional) character string or vector of strings,
#' specifying filters to be applied to the original query. These are of the
#' form "INDEXEDFIELD:VALUE" e.g. "kingdom:Fungi". 
#' @param download logical: if TRUE download all images and add location to
#' dataframe
#' @param download_path string: (optional) filepath to download images to.
#' If not given and download param is TRUE, will create an images
#' folder
#' @param sounds logical (optional) Image search also returns sound files.
#' Ignored unless explicitly requested.
#' @param verbose logical: show additional progress information?
#' [default is set by ala_config()]
#' @return Data frame of image results
#' 
#' @examples 
#' \dontrun{
#' ## Download all images with a CC BY-NC 4.0 licence
#' images(id="da5fe120-e213-4cd6-9c5f-62346ed2e466",
#' fq="recognisedLicence:CC BY-NC-SA 4.0", download=TRUE)
#' }
#' @export images

# Download all images from occurrence id list

images <- function(id, fq, download=FALSE, download_path,
                         sounds = FALSE,
                         verbose=ala_config()$verbose) {
  this_query <- list()
  
  assert_that(is.flag(verbose))
  assert_that(is.flag(sounds))
  
  if(missing(id)) {
    stop("Please provide a list of images ids to retrieve")
  }
  
  assert_that(is.character(id))
  id_str  <- paste(id, collapse = '","')
  this_query$q <- paste0("imageIdentifier:",'"',id_str, '"')
  
  if (!missing(fq)) {
    assert_that(is.character(fq))
    check_fq(fq, type="images")
    
    ## can have multiple fq parameters, need to specify in url as
    ## fq=a:b&fq=c:d&fq=...
    ## check that fq fields are valid
    fq <- as.list(fq)
    names(fq) <- rep("fq", length(fq))
    this_query <- c(this_query, fq)
  }
  
  this_url <- build_url_from_parts(
    getOption("ALA4R_server_config")$base_url_images,
    c("ws","/","search"),
    query=this_query)
  
  
  image_data <- cached_get(url=this_url,type="json",caching="off",
                           verbose=verbose)
  
  # Warn that no images were found
  if(length(image_data$images) == 0) {
    warning("No images were found for the occurrence ids provided")
    return()
  }
  
  if(!sounds) {
    data <- image_data$images[image_data$images$fileType == 'image',]
  }
  else {
    data <- image_data$images
  }
  
  if (download) {
    if (missing(download_path)) {
      message(sprintf("No download path specified.
                  Media will be downloaded in %s",
                      file.path(getwd(),'media')))
      download_path <- file.path(getwd(),'media')
    }
    download_images(data=data,media_dir=download_path,verbose=verbose)
  }
  
  return(data)
}


download_images <- function(data, media_dir, verbose=verbose) {
  
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
      if(data[r,'fileType'] == 'image') {
        ext <- '.jpg'
      }
      else {
        ext <- '.mp4'
      }
      out_path <- file.path(media_dir,paste0(id,ext))
      download_to_file(url, out_path, verbose = verbose)
    }
  }
  
}