context("Get occurrence data")

test_that("ala_occurrences check inputs", {
  skip_on_cran()
  # bad field
  expect_error(ala_occurrences(filters = c(invalid_field = 'value')))
})

test_that("ala_occurrences handles filters correctly", {
  # can handle multiword filters
  skip_on_cran()
  expect_equal(unique(ala_occurrences(filters = c(state = 'Australian Capital Territory',
                                                  basis_of_record = 'FossilSpecimen'),
                                      columns = c("default", "state"))$stateProvince),
               "Australian Capital Territory")
  
})

test_that("ala occurrences returns requested columns",{
  skip_on_cran()
  expected_cols <- c("decimalLatitude", "decimalLongitude", "eventDate",
                     "scientificName", "species_guid","species","recordID")
  id <- ala_taxa("Polytelis swainsonii")$taxonConceptID
  expect_equal(names(ala_occurrences(taxon_id = id,
                                     filters = c(occurrence_decade_i = 1930),
                                     columns = "default")), expected_cols)
  expect_equal(names(ala_occurrences(taxon_id = id,
                                     filters = c(occurrence_decade_i = 1930),
                                     columns = c("occurrence_status",
                                                 "latitude", "longitude"))),
               c("occurrenceStatus","decimalLatitude","decimalLongitude"))
})

test_that("ala_occurrences handles area inputs", {
  # invalid wkt
  invalid_wkt <- "POLYGON((145.71622941565508 -32.17848852726597,))"
  expect_error(ala_occurrences(area = invalid_wkt))
  
  
  # shapefile
  # Download ACT shapefile from ALA, should store in data/ instead?
  # tests searchability in multipolygon
  # also tests very long wkt
  #dir.create('test_data')
  #download.file(url = "https://spatial.ala.org.au/ws/shape/shp/3742602",
  #                          destfile = 'test_data/act_shp.zip')
  #unzip('test_data/act_shp.zip', exdir = 'test_data')
  #shp_file <- st_read('test_data/3742602.shp')
  # all banksia records in the ACT for this decade
  #occ <- ala_occurrences(
  #  taxon_id = "https://id.biodiversity.org.au/taxon/apni/51299884",
  #  filters = c(occurrence_decade_i = 2000), area = shp_file,
  #  columns = c("default", "state"))
  # all records are in the state
  #expect_equal(unique(occ$stateProvince), "Australian Capital Territory")

})
