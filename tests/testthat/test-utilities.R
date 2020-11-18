context("Test utility functions")

test_that("ala config works as expected", {
  skip_on_cran()
  ala_config("reset")
  conf <- ala_config()
  expect_equal(c(conf$download_reason_id, conf$warn_on_empty, conf$caching,
                 conf$verbose, conf$text_encoding),
               c(4, FALSE, "on", FALSE, "UTF-8"))
  expect_error(ala_config(bad_option = 3))
  # test numeric conversion
  ala_config(download_reason_id = "testing")
  expect_equal(ala_config()$download_reason_id, 10)
  expect_error(ala_config(cache_directory = "non_existent_directory"))
  expect_error(ala_config(verbose = "false"))
  expect_error(ala_config(warn_on_empty = "false"))
  expect_error(ala_config(user_agent = 1))
})
