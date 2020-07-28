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
  test_that("image retrieval throws an error when no occurrence id is
            provided", {
    skip_on_cran()
    expect_error(occurrence_images(download = TRUE))

  })
}

check_caching(thischeck)

thischeck <- function() {
  test_that("image search downloads images for a single occurrence id", {
    skip_on_cran()
    result <- occurrence_images("80b1d0d6-2ca5-475a-a014-302f05d51839",
                                    download_path = "media", download = TRUE)
    file_count <- length(list.files("media"))
    expect_equal(file_count, nrow(result))
    unlink("media/*")

    # check can handle case when no download path is specified
    result <- occurrence_images("4e0cf691-be5b-42b6-9bd9-034a6ca4e529",
                                download = TRUE)
    file_count <- length(list.files("media"))
    expect_equal(file_count, nrow(result))
    unlink("media/*")

  })
}

check_caching(thischeck)

thischeck <- function() {
  test_that("image search downloads images for multiple occurrence ids", {
    skip_on_cran()
    result <- occurrence_images(c("80b1d0d6-2ca5-475a-a014-302f05d51839",
                                    "24fe59d8-abe0-4609-a964-e6d9729903bd"),
                                    download_path = "media", download = TRUE)
    file_count <- length(list.files("media"))
    expect_equal(file_count, nrow(result))
    unlink("media/*")
  })
}

check_caching(thischeck)

thischeck <- function() {
  test_that("image search warns if no images are found", {
    skip_on_cran()
    ala_config("warn_on_empty" = TRUE)
    expect_warning(occurrence_images("invalid-id"))
    ala_config("warn_on_empty" = FALSE)
    expect_message(occurrence_images("invalid-id"),
                   "No images were found for occurrence id invalid-id")
  })
}

check_caching(thischeck)

thischeck <- function() {
  test_that("image search handles fq correctly", {
    skip_on_cran()
    expect_error(occurrence_images(id = "80b1d0d6-2ca5-475a-a014-302f05d51839",
                                  fq = as.factor("recognisedLicence:
                                                 CC BY-NC 4.0")))
    result <- occurrence_images("4e0cf691-be5b-42b6-9bd9-034a6ca4e529",
                                fq = "recognisedLicence:CC BY-NC 4.0")
    expect_equal(unique(result$recognisedLicence), "CC BY-NC 4.0")

  })
}

check_caching(thischeck)

thischeck <- function() {
  test_that("image search can return sounds", {
    skip_on_cran()
    result <- occurrence_images(id = "ce18179e-bb34-4585-8c3b-8966f21cc2fa",
                                sounds = TRUE)
    expect_equal(unique(result$extension), c("mp3", "jpg"))

  })
}
