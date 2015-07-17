context("Testing occurrence-related functions")

## ala_reasons
test_that("ala_reasons works as expected", {
    expect_that(ala_reasons(),has_names(c("rkey","name","id")))
    expect_that(nrow(ala_reasons()),equals(11))
    expect_equal(sort(ala_reasons()$id),0:10)
    expect_error(ala_reasons(TRUE)) ## this should throw and error because there is an unused argument
})

