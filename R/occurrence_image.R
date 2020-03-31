occurrence_image_search <- function(id, fq, download=FALSE, download_path,
                                    sounds = FALSE,
                                    verbose=ala_config()$verbose) {
  this_query <- list()
  
  assert_that(is.flag(verbose))
  assert_that(is.flag(sounds))
  
  if(missing(id)) {
    stop("Please provide a list of occurrence ids to retrieve images for")
  }
  
  assert_that(is.character(id))
  id_str  <- paste(id, collapse = '","')
  this_query$q <- paste0("occurrenceID:",'"',id_str, '"')
  
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
