#' Find images using occurrence ids
#' 
#' @references \itemize{
#' \item Associated ALA web service for image search counts:
#' \url{https://images.ala.org.au/ws#/Search/search}
#' }

#' @param id character: IDs of occurrences as single sring or vector of strings
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
#' ## Download all images for an occurrence with a CC BY-NC 4.0 licence
#' occurrence_image_search(id="d201f3e0-3e1d-47f1-94ce-9fc226cbc5ec",
#' fq="recognisedLicence:CC BY-NC 4.0",
#' download=TRUE)
#' }
#' @export occurrence_images

occurrence_images <- function(id, fq, download=FALSE, download_path,
                                    sounds = FALSE,
                                    verbose=ala_config()$verbose) {
  this_query <- list()
  
  assert_that(is.flag(verbose))
  assert_that(is.flag(sounds))
  assert_that(is.flag(download))
  
  if (is.null(getOption("ALA4R_server_config")$base_url_images) ||
      getOption("ALA4R_server_config")$base_url_images=="") {
    stop("No URL to the image database has been configured: see base_url_images
         in ", getOption("ALA4R_server_config")$config_function)
  }
  
  if(missing(id)) {
    stop("Please provide a list of occurrence ids to retrieve images for")
  }
  
  assert_that(is.character(id))
  
  if (!missing(fq)) {
    assert_that(is.character(fq))
    check_fq(fq, type="images")
    
    ## can have multiple fq parameters, need to specify in url as
    ## fq=a:b&fq=c:d&fq=...
    ## check that fq fields are valid
    fq <- as.list(fq)
    names(fq) <- rep("fq", length(fq))
  }
  else {
    fq <- NULL
  }
  
  image_data <- do.call(rbind, lapply(id, function(z) {
    this_query <- list()
    this_query$q <- paste0("occurrenceID:",'"',z, '"')
    if(!is.null(fq)) { this_query <- c(this_query, fq)}
    
    this_url <- build_url_from_parts(
      getOption("ALA4R_server_config")$base_url_images,
      c("ws","/","search"),
      query=this_query)
    data <- cached_get(URLencode(this_url), type="json", verbose=verbose)
    
    # if no images are found for any given occurrence id, print a warning
    if (data$totalImageCount == 0) {
      warning(paste0("No images were found for occurrence id ",z))
    }
    
    df <- as.data.frame(data$images, stringsAsFactors = FALSE)
    
    # throttle API calls so ALA server is not overloaded
    Sys.sleep(1)
    return(df)
  }))
  
  if(length(image_data) == 0) {
    warning("No images were found for any of the occurrence ids provided")
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
    download_images(data=image_data,media_dir=download_path,verbose=verbose,
                    sounds=sounds)
  }
  
  return(image_data)
}
