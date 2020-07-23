context("Test facet searching")

thischeck <- function() {
  test_that("occurrence facets gives an error for invalid facets", {
    skip_on_cran()
    expect_error(occurrence_facets(facet = 'bad_facet'))
  })
}

check_caching(thischeck)

thischeck <- function() {
  test_that("occurrence facets handles page size as expected", {
    skip_on_cran()
    expect_error(occurrence_facets(facet = 'basis_of_record',page_size = "3"))
    
    result <- occurrence_facets(facet = 'basis_of_record',
                                page_size = 10)
    expect_equal(nrow(result$data),10)
  })
}

check_caching(thischeck)

thischeck <- function() {
  test_that("occurrence facets handles offset as expected", {
    skip_on_cran()
    all_bors <- occurrence_facets(facet = 'basis_of_record')
    result <- occurrence_facets(facet = 'basis_of_record',start = 5)
    expect_equal(nrow(all_bors$data),nrow(result$data) + 5)
  })
}

check_caching(thischeck)