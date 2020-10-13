context("ALA categories")

max_categories <- 20

test_that("ala_categories returns an error for bad categories", {
  skip_on_cran()
  expect_error(ala_categories())
  expect_error(ala_categories('bad_category'))
})

test_that("ala categories handles fields with a large number of options", {
  skip_on_cran()
  expect_warning(expect_equal(length(ala_categories('data_resource_uid')),
                              max_categories))
})

test_that("ala categories returns expected value", {
  skip_on_cran()
  expect_equal(length(ala_categories('basis_of_record')), 12)
})