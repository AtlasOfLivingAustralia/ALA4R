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
})

