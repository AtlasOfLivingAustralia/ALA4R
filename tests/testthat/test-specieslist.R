context("Test specieslist")

thischeck <- function() {
    test_that("specieslist returns empty data.frame for no results", {
        skip_on_cran()
        x <- specieslist("dfjknnaklhkjf",
                                        wkt = "POLYGON((147.62 -42.83,147.60
                                        -42.86,147.65 -42.87,147.70 -42.86,
                                        147.62 -42.83))", fq = "taxonRank:species")
        expect_is(x, "data.frame")
        expect_equal(nrow(x), 0)
    })

    test_that("specieslist checks fq fields", {
        skip_on_cran()
        ## rk_genus is the BIE field name, but specieslist uses occurrence
        ## fields, which is just "genus"
        x <- specieslist(
          wkt = "POLYGON((145 -37,150 -37,150 -30,145 -30,145 -37))",
          fq = "genus:Heleioporus")
        expect_gt(nrow(x), 0)
    })

    test_that("specieslist handles errors", {
      skip_on_cran()
      expect_warning(specieslist(wkt = "POLYGON"))
      ala_config(warn_on_empty = TRUE)
      expect_warning(specieslist(taxon = "eucaloptus"))
      expect_error(specieslist())
    })
}
check_caching(thischeck)
