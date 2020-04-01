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
  test_that("image retrieval throws an error when no occurrence id is provided", {
    skip_on_cran()
    expect_error(occurrence_images(download=TRUE))
    
  })
}

check_caching(thischeck)

thischeck <- function() {
  test_that("image search downloads images for a single occurrence id", {
    skip_on_cran()
    result <- occurrence_images("80b1d0d6-2ca5-475a-a014-302f05d51839",
                                    download_path = 'media', download = TRUE)
    file_count <- length(list.files('media'))
    expect_equal(file_count, nrow(result))
  })
}

check_caching(thischeck)

thischeck <- function() {
  test_that("image search downloads images for multiple occurrence ids", {
    skip_on_cran()
    result <- occurrence_images(c("80b1d0d6-2ca5-475a-a014-302f05d51839",
                                    "24fe59d8-abe0-4609-a964-e6d9729903bd"),
                                    download_path = 'media', download = TRUE)
    file_count <- length(list.files('media'))
    expect_equal(file_count, nrow(result))
  })
}

check_caching(thischeck)

thischeck <- function() {
  test_that("image search warns if no images are found", {
    skip_on_cran()
    expect_warning(occurrence_images("this-is-an-invalid-id"))
  })
}

check_caching(thischeck)