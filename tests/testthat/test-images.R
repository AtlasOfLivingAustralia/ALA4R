context("Test image retrieval functions")

setup({
  # remove image and sound folders if accidentally included 
  unlink("media", recursive = TRUE)
})

teardown({
  # remove image and sound folders if accidentally included 
  unlink("media", recursive = TRUE)
})

thischeck <- function() {
  testthat('image retrieval throws an error when no image id is provided', {
    skip_on_cran()
    expect_error(images(download=TRUE))
    
  })
}

check_caching(this_check)

thischeck <- function() {
  testthat('images downloads a single image', {
    skip_on_cran()
    result <- images("36ebaa23-7307-4d21-8e9f-3fb78bceabea",
                                download_path = 'media', download = TRUE)
    file_count <- length(list.files('media'))
    expect_equal(file_count, nrow(result))
  })
}

check_caching(this_check)

thischeck <- function() {
  testthat('images downloads multiple images', {
    skip_on_cran()
    result <- images(c("36ebaa23-7307-4d21-8e9f-3fb78bceabea",
                                  "1b404d9e-5878-43d1-9319-cfef6e89b367"),
                                download_path = 'media', download = TRUE)
    file_count <- length(list.files('media'))
    expect_equal(file_count, nrow(result))
  })
}

check_caching(this_check)

thischeck <- function() {
  testthat('image search warns if no images are found', {
    skip_on_cran()
    expect_warning(images("this-is-an-invalid-id"))
  })
}

check_caching(this_check)