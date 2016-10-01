context("Test specieslist")

thischeck=function() {
    test_that("specieslist returns empty data.frame for no results", {
        x <- specieslist("dfjknnaklhkjf",wkt="POLYGON((147.62 -42.83,147.60 -42.86,147.65 -42.87,147.70 -42.86,147.62 -42.83))",fq="rank:species")
        expect_is(x,"data.frame")
        expect_equal(nrow(x),0)
    })

    test_that("specieslist checks fq fields", {
        ## rk_genus is the BIE field name, but specieslist uses occurrence fields, which is just "genus"
        expect_error(expect_warning(x <- specieslist(wkt="POLYGON((145 -37,150 -37,150 -30,145 -30,145 -37))",fq="rk_genus:Macropus")))
        x <- specieslist(wkt="POLYGON((145 -37,150 -37,150 -30,145 -30,145 -37))",fq="genus:Macropus")
        expect_gt(nrow(x),0)
    })
}
check_caching(thischeck)
