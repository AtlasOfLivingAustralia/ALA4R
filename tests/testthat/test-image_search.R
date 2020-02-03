context("Test image search functions")

this_check <- function() {
  test_that("images are downloaded as expected", {
    q <- "kangaroo"
    fq <- "recognisedLicence:CC BY-NC 4.0"
  })
}

check_caching(this_check)


this_check <- function() {
  test_that("image information is returned without downloads", {
    
  })
}

check_caching(this_check)

this_check <- function() {
  test_that("error is returned if query is invalid", {
    invalid_query <- ""
  })
}

check_caching(this_check)

