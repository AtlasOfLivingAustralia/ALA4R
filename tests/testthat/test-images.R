context("Test image retrieval functions")

setup({
  # remove image and sound folders if accidentally included
  unlink("media", recursive = TRUE)
})

teardown({
  # remove image and sound folders if accidentally included
  unlink("media", recursive = TRUE)
})

expected_property_names <- sort(c("imageIdentifier", "imageUrl", "success",
                                  "mimeType", "originalFileName",
                                  "sizeInBytes", "rights", "rightsHolder",
                                  "dateUploaded", "dateTaken",
                                  "tileUrlPattern", "mmPerPixel", "height",
                                  "width", "tileZoomLevels", "description",
                                  "title", "creator", "license",
                                  "recognisedLicence",
                                  "recognisedLicence.acronym",
                                  "recognisedLicence.id",
                                  "recognisedLicence.version",
                                  "recognisedLicence.imageUrl",
                                  "recognisedLicence.name",
                                  "recognisedLicence.url",
                                  "dataResourceUid", "occurrenceID"))


thischeck <- function() {
  test_that("image retrieval throws an error when no image id is provided", {
    skip_on_cran()
    expect_error(images(download = TRUE))
    expect_error(images())

  })
}

check_caching(thischeck)

thischeck <- function() {
  test_that("images downloads a single image", {
    skip_on_cran()
    result <- images("36ebaa23-7307-4d21-8e9f-3fb78bceabea",
                                download_path = "media", download = TRUE)
    file_count <- length(list.files("media"))
    expect_equal(file_count, nrow(result))

    # check handles no directory being provided
    unlink("media/*")
    result <- images("36ebaa23-7307-4d21-8e9f-3fb78bceabea",
                     download = TRUE)
    unlink("media/*")
  })
}

check_caching(thischeck)

thischeck <- function() {
  test_that("images downloads multiple images and sounds", {
    skip_on_cran()
    result <- images(c("36ebaa23-7307-4d21-8e9f-3fb78bceabea",
                                  "88f61b16-a847-46f8-b15d-75abe0696844"),
                                download_path = "media", download = TRUE)
    file_count <- length(list.files("media"))
    expect_equal(file_count, nrow(result))
    unlink("media/*")
  })
}

check_caching(thischeck)

thischeck <- function() {
  test_that("images works as expected on multiple known records", {
    skip_on_cran()
    known_image_info <- images(c("84654e14-dc35-4486-9e7c-40eb2f8d3faa",
                                 "05436aca-bb91-4801-8633-3616c6a3077e"))
    expect_equal(nrow(known_image_info), 2)
    expect_equal(sort(colnames(known_image_info)), expected_property_names)
  })
}


check_caching(thischeck)

thischeck <- function() {
  test_that("images works with un-matched records", {
    skip_on_cran()
    mixed_images <- images(c("84654e14-dc35-4486-9e7c-40eb2f8d3faa",
                             "this-is-an-invalid-image-id",
                             "05436aca-bb91-4801-8633-3616c6a3077e",
                             "this-is-also-an-invalid-image-id"))
    expect_equal(nrow(mixed_images), 4)
    expect_equal(sort(names(mixed_images)), expected_property_names)
    unmatched_images <- images("this-is-an-invalid-image-id")
    expect_equal(nrow(unmatched_images), 1)
  })
}
check_caching(thischeck)
