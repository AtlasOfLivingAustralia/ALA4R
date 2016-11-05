context("Test fieldguide generator")

thischeck=function() {
    test_that("fieldguide works as expected", {
        skip_on_cran()
        tmp <- fieldguide(guids=c("urn:lsid:biodiversity.org.au:afd.taxon:95773568-053d-44de-a624-5699f0ac4a59","http://id.biodiversity.org.au/node/apni/2890970"))
        expect_is(tmp,"character")
        expect_equal(length(tmp),1)
        expect_true(file.exists(tmp))
    })
}
check_caching(thischeck)

thischeck=function() {
    test_that("fieldguide offline tests", {
        expect_error(fieldguide()) ## no guids
        expect_error(fieldguide(guids=NULL)) ## NULL guids
    })
}
check_caching(thischeck)
