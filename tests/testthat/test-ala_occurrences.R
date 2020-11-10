context("Get occurrence data")

test_that("ala_occurrences check inputs", {
  skip_on_cran()
  # bad field
  expect_error(ala_occurrences(filters = c(invalid_field = "value")))
  expect_error(ala_occurrences(filters =
                                 c(state = "Australian Capital Territory")))
  expect_error(ala_occurrences())
})

test_that("ala_occurrences handles filters correctly", {
  # can handle multiword filters
  skip_on_cran()
  expect_equal(
    unique(ala_occurrences(filters = ala_filters(
      list(state = "Australian Capital Territory",
           basis_of_record = "FossilSpecimen")),
      columns = ala_columns("basic", "state"))$state),
    "Australian Capital Territory")
  expect_error(ala_occurrences(filters = c("FossilSpecimen")))

  # handles year filters
  expect_true(unique(ala_occurrences(filters = ala_filters(
    list(year = seq(1971, 1981),
    basis_of_record = "FossilSpecimen")),
    columns = ala_columns("basic", "year"))$year %in% seq(1971, 1981)))
})

test_that("ala occurrences handles a long query", {
  skip_on_cran()
  # generate a query longer than 2000 characters
  taxa <- ala_taxa("Hymenoptera", return_children = TRUE)
  filters <- ala_filters(filters = list(year = 1990),
                         data_quality_profile = "ALA")

})
test_that("ala occurrences returns requested columns", {
  skip_on_cran()
  expected_cols <- c("decimalLatitude", "decimalLongitude", "eventDate",
                     "scientificName", "taxonConceptID", "recordID",
                     "data_resource")
  id <- ala_taxa("Polytelis swainsonii")$taxon_concept_id
  expect_equal(sort(names(ala_occurrences(taxon_id = id,
                                     filters = ala_filters(
                                       list(occurrence_decade_i = 1930)),
                                     columns = ala_columns("basic")))),
               sort(expected_cols))

  cols <- ala_columns(extra = c("occurrence_status", "latitude", "longitude"))
  expect_equal(names(ala_occurrences(taxon_id = id,
                                     filters = ala_filters(
                                       list(occurrence_decade_i = 1930)),
                                     columns = cols)), c("occurrenceStatus",
                                                         "decimalLatitude",
                                                         "decimalLongitude"))
})

test_that("ala occurrences handles assertion columns and works with data.frame
          input", {
  skip_on_cran()
  id <- ala_taxa("Paraparatrechina minutula")
  cols <- ala_columns(extra = c("zeroLatitude", "zeroLongitude", "eventDate"))
  expect_equal(names(ala_occurrences(taxon_id = id, columns = cols)),
               c("eventDate", "zeroLatitude", "zeroLongitude"))
})

test_that("ala_occurrences handles wkt area inputs", {
  # invalid wkt
  skip_on_cran()
  invalid_wkt <- "POLYGON((145.71622941565508 -32.17848852726597,))"
  valid_wkt <- "POINT(147.08005201710293 -34.48290525355578)"
  expect_error(ala_occurrences(area = as.factor(valid_wkt)))
  expect_error(ala_occurrences(area = invalid_wkt))

  wkt <- readLines("../testdata/long_act_wkt.txt")
  expect_error(ala_occurrences(area = wkt))

  wkt <- readLines("../testdata/short_act_wkt.txt")
  cols <- ala_columns("basic", extra = "state")
  filters <- ala_filters(list(basis_of_record = "MachineObservation"))
  expect_equal(unique(ala_occurrences(area = wkt,
                                      filters = filters,
                                      columns = cols)$stateProvince),
               "Australian Capital Territory")
})

test_that("ala_occurrences handles sf polygon inputs", {
  skip_on_cran()
  # convert wkt to sfc
  act_shp <- st_as_sfc(readLines("../testdata/short_act_wkt.txt"))
  filters <- ala_filters(list(basis_of_record = "MachineObservation"))
  expect_equal(unique(ala_occurrences(area = act_shp, filters = filters,
                                      columns = ala_columns("basic",
                                                  "state"))$stateProvince),
               "Australian Capital Territory")
})
