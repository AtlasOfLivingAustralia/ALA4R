context("Test ala counts")

test_that("ala counts checks inputs", {
  # ALA counts with no arguments gives the total number of records in the ALA
  expect_gt(ala_counts(), 90000000)
  
  # invalid facet
  expect_error(ala_counts(breakdown = "bad_facet"))
  
  # invalid filter
  expect_error(ala_counts(filters = c(bad_facet = 'test')))
  
})

test_that("ala counts returns expected outputs", {
  expect_type(ala_counts(taxon_id = "https://id.biodiversity.org.au/taxon/apni/51302291"), "integer")
  expect_equal(class(ala_counts(taxon_id = "https://id.biodiversity.org.au/taxon/apni/51302291",
                                breakdown = "basis_of_record")), "data.frame")
  
  expect_warning(ala_counts(breakdown = 'phylum'))
})

test_that("ala counts works with filters", {
  skip_on_cran()
  expect_lt(ala_counts(filters = ala_filters(list(year = 2000))),
            ala_counts())
})

test_that("ala_counts handles wkt area inputs", {
  # invalid wkt
  skip_on_cran()
  wkt <- readLines('../testdata/short_act_wkt.txt')
  expect_lt(ala_counts(area = wkt), ala_counts())
})

test_that("ala counts handles queries with no records", {
  skip_on_cran()
  filters <- ala_filters(list(kingdom = 'non-existent'))
  expect_s3_class(ala_counts(filters = filters,
                             breakdown = 'basis_of_record'), "data.frame")
})
