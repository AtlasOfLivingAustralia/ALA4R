context("Test data resource information retrieval functions")

cols <- c("uid", "name", "licenseType", "dateCreated","lastUpdated","doi",
          "Animalia","Bacteria", "Plantae","Chromista","Fungi","Protista",
          "Protozoa","Virus","Unknown","totalDownloadedRecords","totalRecords",
          "resourceType", "gbifRegistryKey")

thischeck <- function() {
  test_that("data resources returns correct data columns", {
    skip_on_cran()
    expect_equal(sort(cols),sort(names(data_resources('dr375'))))
  })
}

check_caching(thischeck)


thischeck <- function() {
  test_that("data resources returns a row for every id provided", {
    skip_on_cran()
    result <- data_resources(c('dr375','dr8128','dr743'))
    expect_equal(nrow(result),3)
  })
}

check_caching(thischeck)



thischeck <- function() {
  test_that("data resources behaves correctly with a single invalid id", {
    skip_on_cran()
    expect_warning(result <- data_resources('invalid-id'))
    expect_equal(ncol(result),length(cols))
  })
}

check_caching(thischeck)

thischeck <- function() {
  test_that("data resources behaves correctly with one valid and one invalid id", {
    skip_on_cran()
    expect_warning(data_resources(c('invalid-id', 'dr345')))
  })
}

check_caching(thischeck)

