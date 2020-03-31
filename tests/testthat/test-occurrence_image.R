context("Test occurrence image retrieval functions")

setup({
  # remove image and sound folders if accidentally included 
  unlink("media", recursive = TRUE)
})

teardown({
  # remove image and sound folders if accidentally included 
  unlink("media", recursive = TRUE)
})

thischeck <- function() {
  skip_on_cran()
  testthat('image retrieval throws an error when no occurrence id is provided', {
    expect_error(occurrence_image_search(download=TRUE))
    
    
  })
}

check_caching(this_check)

thischeck <- function() {
  skip_on_cran()
  testthat('image search downloads images for a single occurrence id', {
    result <- occurrence_image_search("80b1d0d6-2ca5-475a-a014-302f05d51839",
                                    download_path = 'media', download = TRUE)
    file_count <- length(list.files('media'))
    expect_equal(file_count, nrow(result))
  })
}

check_caching(this_check)

thischeck <- function() {
  skip_on_cran()
  testthat('image search downloads images for multiple occurrence ids', {
    result <- occurrence_image_search(c("80b1d0d6-2ca5-475a-a014-302f05d51839",
                                    "24fe59d8-abe0-4609-a964-e6d9729903bd"),
                                    download_path = 'media', download = TRUE)
    file_count <- length(list.files('media'))
    expect_equal(file_count, nrow(result))
  })
}

check_caching(this_check)

thischeck <- function() {
  skip_on_cran()
  testthat('image search warns if no images are found', {
    expect_warning(occurrence_image_search("this-is-an-invalid-id"))
  })
}

check_caching(this_check)