context("Testing occurrence-related functions")

ala_config(caching="off")

## ala_reasons
test_that("ala_reasons works as expected", {
    expect_that(ala_reasons(),has_names(c("rkey","name","id")))
    expect_that(nrow(ala_reasons()),equals(11))
    expect_equal(sort(ala_reasons()$id),0:10)
    expect_error(ala_reasons(TRUE)) ## this should throw and error because there is an unused argument
})


test_that("occurrences summary works when no qa are present", {
    expect_output(summary(occurrences(taxon="Amblyornis newtonianus",download_reason_id=10,qa='none')),"no assertion issues")
})

test_that("occurrences summary gives something sensible", {
    expect_output(summary(occurrences(taxon="Amblyornis newtonianus",download_reason_id=10)),"^number of names")
})
