#' Query images based on search terms 
#' 
#' @references \itemize{
#' \item Associated ALA web service for image search counts: \url{https://images.ala.org.au/ws#/Search/search}
#' }
#' 
#' @param q
#' @param fq
#' @param keyword string: 
#' @param download logical: if TRUE download all images and add location to dataframe
#' @param download_path string: (optional) filepath to download images to. If not given and download param is TRUE, will create an images
#' folder
#' @param sounds: logical (optional) Image search also returns sound files. Ignored unless explicitly requested.
#' 
#' @return Data frame of image results
#' 
#' @examples 
#' \dontrun {
#' ## Download all kangaroo images with a CC BY-NC 4.0 licence
#' image_search(q="kangaroo",fq="recognisedLicence:CC BY-NC 4.0",download=TRUE)
#' 
#' 
#' }


image_search <- function(q, fq, download=FALSE, download_path, sounds = FALSE, verbose=ala_config()$verbose) {
  this_query <- list()
  
  assert_that(is.flag(verbose))
  
  if (!missing(q)) {
    if (is.factor(q)) {
      q <- as.character(q)
    }
    assert_that(is.notempty.string(q))
    this_query$q <- q
  }
  
  if (!missing(fq)) {
    assert_that(is.character(fq))
    check_fq(fq, type="images")
    
    ## can have multiple fq parameters, need to specify in url as fq=a:b&fq=c:d&fq=...
    ## check that fq fields are valid
    fq <- as.list(fq)
    names(fq) <- rep("fq", length(fq))
    this_query <- c(this_query, fq)
  }
  
  this_url <- build_url_from_parts(getOption("ALA4R_server_config")$base_url_images,c("ws","/","search"),query=this_query)
  
  if (length(this_query) == 0) {
    warning("No q or fq has been specified. All images will be returned")
  }
  
  image_data <- cached_get(url=this_url,type="json",caching="off",verbose=verbose)
  ## TODO: Check image data is correctly downloaded 
  
  if (download & missing(download_path)) {
    message(sprintf("No download path has been specified. Media will be downloaded in %s",file.path(getwd(),'media')))
    download_path <- file.path(getwd(),'media')
  }
  
  if(!sounds) {
    data <- image_data$images[image_data$images$fileType == 'image',]
  }
  else {
    data <- image_data$images
  }
  
  if (download) {
    download_images(data=data,media_dir=download_path,verbose=verbose)
  }
  
  return(data)
}

download_images <- function(data, media_dir, verbose=verbose) {
  
  assert_that(!missing(media_dir))
  
  if(!file.exists(media_dir)) {
    message(sprintf('Media directory does not exist, creating directory %s', media_dir))
    dir.create(media_dir)
  }
  
  if (!missing(data)) {
    for(r in 1:nrow(data)) {
      id <- data[r,'imageIdentifier']
      url <- build_url_from_parts(getOption("ALA4R_server_config")$base_url_images, c('image',id, 'original'))
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


  
