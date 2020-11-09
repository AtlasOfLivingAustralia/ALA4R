context("Test ALA filters")

test_that("ala_filters builds data quality filters", {
  expect_s3_class(ala_filters(data_quality_profile = "ALA"),
                  "data.frame")
  expect_error(ala_filters(data_quality_profile = "bad"))
})

test_that("ala_filters handles assertion filters", {
  expect_true("assertions" %in%
                ala_filters(list(zeroCoordinates = FALSE,
                                 habitatMismatch = FALSE))$name)
})

test_that("ala_filters handles exclusion filters", {
  expect_false(ala_filters(list(basis_of_record =
                                  exclude("HumanObservation")))$include)
})