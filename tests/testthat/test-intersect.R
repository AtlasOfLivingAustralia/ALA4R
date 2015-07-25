context("Test intersection of points with environmental values")

## intersect_points
thischeck=function() {
    test_that("intersect_points gives errors or warning for invalid field names", {
        layers = c('clxx')
        pnts = c(-20,130)
        expect_warning(expect_error(intersect_points(pnts,layers))) ## gives both warning and error
        layers = c('clxx','clzz')
        expect_warning(expect_error(intersect_points(pnts,layers))) ## gives both warning and error
        layers = c('clxx','cl22')
        expect_warning(intersect_points(pnts,layers)) ## just a warning
    })
}
check_caching(thischeck)

thischeck=function() {
    test_that("intersect_points gives same answers for single-location and batch methods", {
        layers = c('el773','cl22')
        pnts = c(-20,130)
        out1<-intersect_points(pnts,layers)
        expect_that(out1$waterSurplusMonthMax,is_a("numeric")) ## this can incorrectly be character under some json parsing (when bulk lookup not used for intersect points)
        pnts = c(-20,130,-30,140)
        out2<-intersect_points(pnts,layers)
        expect_equal(out1,out2[1,])
    })
}
check_caching(thischeck)
