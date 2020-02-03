context("Test image search functions")
setup({
  # remove image and sound folders if accidentally included 
  unlink("media", recursive = TRUE)
  unlink("sounds", recursive = TRUE)
  unlink("images", recursive = TRUE)
})

teardown({
  # remove image and sound folders if accidentally included 
  unlink("media", recursive = TRUE)
  unlink("sounds", recursive = TRUE)
  unlink("images", recursive = TRUE)
})

this_check <- function() {
  test_that("images are downloaded as expected", {
    skip_on_cran()
    q <- "kangaroo"
    fq <- "recognisedLicence:CC BY-NC 4.0"
    result <- image_search(q=q,fq=fq, download = TRUE, download_path = 'images')
    # check the number of files downloaded is equal to the number of images
    file_count <- length(list.files('images'))
    expect_equal(file_count, nrow(result))
                 
    # check image directory is created if download is true and no directory specified
    expect_true(file.exists('images'))
  })
}

check_caching(this_check)


this_check <- function() {
  test_that("image information is as expected", {
    skip_on_cran()
    result <- image_search(q="kangaroo",fq="recognisedLicence:CC BY-NC 4.0", download = FALSE)
    
    # check that no sound data is present if only image data is requested
    #expect_equal(unique(result$fileType), "image")
    
    # check that all expected data columns are returned 
    image_fields <- c("rightsHolder","thumbHeight","extension","imageIdentifier", 
                       "contentmd5hash","dateUploaded","title","rights",               
                       "imageSize","height","harvestable","creator",              
                       "created","dateUploadedYearMonth","format","thumbWidth",           
                       "occurrenceID","zoomLevels","recognisedLicence","license",              
                       "originalfilename","dataResourceUid","fileSize","contentsha1hash",      
                       "width","fileType")
    expect_setequal(names(result),image_fields)
  })
}

check_caching(this_check)

this_check <- function() {
  test_that("warning is returned for empty query", {
    expect_warning(image_search())
  })
}

check_caching(this_check)

this_check <- function() {
  test_that("filter query filters correctly", {
    result <- image_search(q="red kangaroo",fq="recognisedLicence:CC BY-NC 4.0", download = FALSE)
    expect_equal(unique(result$recognisedLicence),"CC BY-NC 4.0")
  })
}

check_caching(this_check)

this_check <- function() {
  test_that("sound data is downloaded correctly", {
    skip_on_cran()
    result <- image_search(q = "kangaroo", fq = "fileType:sound", download_path = 'sounds')
    file_count <- length(list.files('sounds'))
    expect_equal(file_count, nrow(result))
  })
}

check_caching(this_check)

this_check <- function() {
  test_that("error is returned is no download file is provided", {
    expect_error(download_images(image_ids = c("a2cc242b-1a5e-4855-aad1-c96a5911d729")))
  })
}

check_caching(this_check)