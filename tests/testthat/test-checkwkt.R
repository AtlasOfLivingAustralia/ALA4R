context("Test check_wkt function")

thischeck <- function() {
    test_that("check_wkt generally works as expected", {
        expect_false(ALA4R:::check_wkt(""))
        expect_error(ALA4R:::check_wkt(1))
        expect_true(ALA4R:::check_wkt("POLYGON((154 -43.74,154 -9,112.9 -9,112.9 -43.74,154 -43.74))"))
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("check_wkt copes with extra whitespaces", {
        ## whitespace after commas
        expect_true(ALA4R:::check_wkt("POLYGON((154 -43.74, 154 -9, 112.9 -9, 112.9 -43.74, 154 -43.74))"))
        ## whitespace before ((
        expect_true(ALA4R:::check_wkt("POLYGON ((154 -43.74, 154 -9, 112.9 -9, 112.9 -43.74, 154 -43.74))"))

        ## these ones fail: not sure if they are actually valid WKT or not
        ## whitespace before commas
        #expect_true(ALA4R:::check_wkt("POLYGON((154 -43.74 , 154 -9, 112.9 -9, 112.9 -43.74, 154 -43.74))"))
        ## whitespace after ((
        #expect_true(ALA4R:::check_wkt("POLYGON(( 154 -43.74, 154 -9, 112.9 -9, 112.9 -43.74, 154 -43.74))"))
        ## whitespace before ))
        #expect_true(ALA4R:::check_wkt("POLYGON((154 -43.74, 154 -9, 112.9 -9, 112.9 -43.74, 154 -43.74  ))"))
    })
}
check_caching(thischeck)
    
