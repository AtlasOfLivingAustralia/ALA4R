context("Get occurrence data")

test_that("ala_occurrences check inputs", {
  skip_on_cran()
  # bad field
  expect_error(ala_occurrences(filters = c(invalid_field = 'value')))
  expect_error(ala_occurrences(filters = c(state = 'Australian Capital Territory')))
  expect_error(ala_occurrences())
})

test_that("ala_occurrences handles filters correctly", {
  # can handle multiword filters
  skip_on_cran()
  expect_equal(
    unique(ala_occurrences(filters = ala_filters(
      list(state = 'Australian Capital Territory',
           basis_of_record = 'FossilSpecimen')),
      columns = c("default", "state"))$stateProvince),
    "Australian Capital Territory")
  expect_error(ala_occurrences(filters = c("FossilSpecimen")))
  
  # handles year filters
  expect_equal(range(ala_occurrences(filters = ala_filters(
    list(year = seq(1971, 1981),
    basis_of_record = 'FossilSpecimen')),
    columns = c("default", "year"))$year),
    c(1971, 1981))
  
})

test_that("ala occurrences uses data quality filters", {
  skip_on_cran()
  # check there are no records before 1700
  
  
})
test_that("ala occurrences returns requested columns",{
  skip_on_cran()
  expected_cols <- c("decimalLatitude", "decimalLongitude", "eventDate",
                     "scientificName", "taxonConceptID", "recordID",
                     "data_resource")
  id <- ala_taxa("Polytelis swainsonii")$taxon_concept_id
  expect_equal(names(ala_occurrences(taxon_id = id,
                                     filters = ala_filters(
                                       list(occurrence_decade_i = 1930)),
                                     columns = "default")), expected_cols)
  expect_equal(names(ala_occurrences(taxon_id = id,
                                     filters = ala_filters(
                                       list(occurrence_decade_i = 1930)),
                                     columns = c("occurrence_status",
                                                 "latitude", "longitude"))),
               c("occurrenceStatus","decimalLatitude","decimalLongitude"))
})

test_that("ala_occurrences handles wkt area inputs", {
  # invalid wkt
  skip_on_cran()
  invalid_wkt <- "POLYGON((145.71622941565508 -32.17848852726597,))"
  valid_wkt <- "POINT(147.08005201710293 -34.48290525355578)"
  expect_error(ala_occurrences(area = as.factor(valid_wkt)))
  expect_error(ala_occurrences(area = invalid_wkt))
  
  wkt <- readLines('../testdata/long_act_wkt.txt')
  expect_error(ala_occurrences(area = wkt))
  
  wkt <- readLines('../testdata/short_act_wkt.txt')
  expect_equal(unique(ala_occurrences(area = wkt,
                                      filters = ala_filters(
                                        list(basis_of_record = 'MachineObservation')),
                                      columns = c("default",
                                                  "state"))$stateProvince),
               "Australian Capital Territory")

})

test_that("ala_occurrences handles sf polygon inputs", {
  skip_on_cran()
  # convert wkt to sfc 
  act_shp <- st_as_sfc(readLines('../testdata/short_act_wkt.txt'))
  expect_equal(unique(ala_occurrences(area = act_shp,
                                      filters = ala_filters(
                                        list(basis_of_record = "MachineObservation")),
                                      columns = c("default",
                                                  "state"))$stateProvince),
               "Australian Capital Territory")
})
