context("Test intersection of points with environmental values")

## intersect_points
thischeck <- function() {
    test_that("intersect_points gives errors or warning for invalid field
              names", {
        skip_on_cran()
        layers <- c("clxx")
        pnts <- c(-20, 130)
        ## gives both warning and error
        expect_warning(expect_error(intersect_points(pnts, layers)))
        layers <- c("clxx", "clzz")
        ## gives both warning and error
        expect_warning(expect_error(intersect_points(pnts, layers)))
        layers <- c("clxx", "cl10925")
        expect_warning(intersect_points(pnts, layers)) ## just a warning
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("intersect_points gives some correct known answers", {
        skip_on_cran()
        temp <- intersect_points(pnts = data.frame(lat = c(-23.1, -42),
                                                 lon = c(149.1, 148)),
                                 layers = "cl10925")
        expect_true(all(temp$PSMAStates2016 ==
                          c("QUEENSLAND", "TASMANIA")))
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("intersect_points gives same answers for single-location and
              batch methods", {
        skip_on_cran()
        layers <- c("el767", "cl10925")
        pnts <- c(-20, 130)
        out1 <- intersect_points(pnts, layers)
        expect_that(out1$evaporationMonthMax, is_a("numeric"))
        pnts <- c(-20, 130, -30, 140)
        out2 <- intersect_points(pnts, layers)
        expect_equal(out1, out2[1, ])
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("intersect_points works for largeish number of points", {
        skip_on_cran()
        layers <- c("el767", "cl10925")
        pnts <- cbind(lat = runif(1000, -40, -12),
                      long = runif(1000, 115, 148))
        out1 <- intersect_points(pnts, layers)
        expect_that(out1$evaporationMonthMax, is_a("numeric"))
    })
}
check_caching(thischeck)

thischeck <- function() {
  test_that("intersect_points gives errors for bad inputs", {
    skip_on_cran()
    layers <- c("cl22", "cl23", "el767")
    pnts <- c(-23.1)
    expect_error(intersect_points(pnts, layers))
    pnts <- c(-23.1, 149.1)
    expect_error(intersect_points(as.data.frame(pnts), layers))

    # should get errors when input is too long
    pnts <- cbind(lat = runif(100002, -40, -12),
                  long = runif(100002, 115, 148))
    expect_error(intersect_points(pnts, layers))
    pnts <- c(-20, 130, -30, 140)
    layers <- rep("el767", 700)
    expect_error(intersect_points(pnts, layers))
  })
}
check_caching(thischeck)

thischeck <- function() {
  test_that("intersect points returns spatial dataframe if requested", {
    layers <- c("el767", "cl10925")
    pnts <- c(-20, 130)
    out <- intersect_points(pnts, layers, SPdata.frame = TRUE)
    expect_is(out, "SpatialPointsDataFrame")
  })
}

check_caching(thischeck)
