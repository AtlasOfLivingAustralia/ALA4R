context("Get occurrence data")

test_that("ala_occurrences check inputs", {
  # bad field
  expect_error(ala_occurrences(filters = c(invalid_field = 'value')))
})

test_that("ala_occurrences handles filters correctly", {
  # can handle multiword filters
  skip_on_cran()
  expect_equal(unique(ala_occurrences(filters = c(state = 'Australian Capital Territory'),
                                      columns = c("default", "state"))$stateProvince),
               "Australian Capital Territory")
  
})
