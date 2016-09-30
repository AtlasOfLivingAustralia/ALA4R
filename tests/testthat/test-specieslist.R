context("Test specieslist")

thischeck=function() {
    test_that("specieslist returns empty data.frame for no results", {
        x <- specieslist("dfjknnaklhkjf",wkt="POLYGON((147.62 -42.83,147.60 -42.86,147.65 -42.87,147.70 -42.86,147.62 -42.83))",fq="rank:species")
        expect_is(x,"data.frame")
        expect_equal(nrow(x),0)
    })
}
check_caching(thischeck)
