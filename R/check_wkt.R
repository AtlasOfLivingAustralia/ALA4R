## simple function to check that a given WKT string is valid
## prior to v1.25 this was an ALA4R-specific (and not very comprehensive) function
## as of v1.25 we rely on wellknown::lint
check_wkt <- function(wkt) {
    assert_that(is.string(wkt)) ## leave this in place for consistency with older code
    wellknown::lint(wkt)
}
