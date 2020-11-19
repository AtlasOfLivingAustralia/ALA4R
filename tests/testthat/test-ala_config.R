context("Test ala_config")

test_that("ala config sets default options", {
  # set to null
  options(ALA4R_config = NULL)
  # check that defaults are used
  expect_equal(ala_config()$verbose, FALSE)
})

test_that("ala config checks inputs", {
  expect_error(ala_config(caching = "value"))
  expect_error(ala_config(verbose = "value"))
  expect_error(ala_config(ala_email = 4))
  expect_error(ala_config(download_reason_id = 17))
  expect_silent(ala_config(download_reason_id = "testing"))
  expect_silent(ala_config(download_reason_id = "Testing"))
  expect_error(ala_config(download_reason_id = "tsting"))
})