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
    unique(ala_occurrences(filters = list(state = 'Australian Capital Territory',
                                       basis_of_record = 'FossilSpecimen'),
                           columns = c("default", "state"))$stateProvince),
    "Australian Capital Territory")
  expect_error(ala_occurrences(filters = c("FossilSpecimen")))
  
  # handles year filters
  expect_equal(range(ala_occurrences(filters = list(year = c(1971, 1981),
                                           basis_of_record = 'FossilSpecimen'),
                                      columns = c("default", "year"))$year),
               c(1971, 1981))
  
})

test_that("ala occurrences returns requested columns",{
  skip_on_cran()
  expected_cols <- c("decimalLatitude", "decimalLongitude", "eventDate",
                     "scientificName", "species_guid","recordID")
  id <- ala_taxa("Polytelis swainsonii")$taxonConceptID
  expect_equal(names(ala_occurrences(taxon_id = id,
                                     filters = list(occurrence_decade_i = 1930),
                                     columns = "default")), expected_cols)
  expect_equal(names(ala_occurrences(taxon_id = id,
                                     filters = list(occurrence_decade_i = 1930),
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
  
  wkt <- readLines('../testdata/act_wkt.txt')
  expect_equal(unique(ala_occurrences(area = wkt,
                                      filters = list(basis_of_record =
                                                    "FossilSpecimen"),
                                      columns = c("default",
                                                  "state"))$stateProvince),
               "Australian Capital Territory")

})

test_that("ala_occurrences handles sf polygon inputs", {
  skip_on_cran()
  act_shp <- st_read('../testdata/act_state_polygon_shp/ACT_STATE_POLYGON_shp.shp')
  expect_equal(unique(ala_occurrences(area = act_shp,
                                      filters = list(basis_of_record = "FossilSpecimen"),
                                      columns = c("default",
                                                  "state"))$stateProvince),
               "Australian Capital Territory")
})
