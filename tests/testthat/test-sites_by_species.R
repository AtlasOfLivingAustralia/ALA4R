context("Test sites-by-species functionality")

thischeck <- function() {
    test_that("sites_by_species works as expected", {
        skip("Unreliable web service")
        ss <- sites_by_species(
            taxon = "genus:Eucalyptus",
            wkt = "POLYGON((144 -43,148 -43,148 -40,144 -40,144 -43))",
            gridsize = 0.1, verbose = FALSE)
        expect_is(ss, "data.frame")
        expect_gt(ncol(ss), 50) ## at least 50 species
        expect_gt(nrow(ss), 600) ## at least 600 sites
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("sites_by_species outputs a spatial points dataframe", {
        skip("Unreliable web service")
        ss <- sites_by_species(
          taxon = "genus:Eucalyptus",
          wkt = "POLYGON((144 -43,148 -43,148 -40,144 -40,144 -43))",
          gridsize = 0.1, verbose = FALSE, SPdata.frame = TRUE)
        expect_is(ss, "SpatialPointsDataFrame")
        expect_gt(ncol(ss), 50) ## at least 50 species
        expect_gt(nrow(ss), 600) ## at least 600 sites
    })
}
check_caching(thischeck)
