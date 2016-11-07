library(testthat)
library(ALA4R)

## run each test with and without caching
check_caching=function(f) {
    ala_config(caching="off")
    f()
    ala_config(caching="on")
    f()
    f()
}

test_check("ALA4R",reporter="summary")
