context("Test occurrence-related functions")

## ala_reasons
thischeck=function() {
    test_that("ala_reasons works as expected", {
        expect_that(ala_reasons(),has_names(c("rkey","name","id")))
        expect_that(nrow(ala_reasons()),equals(12))
        expect_equal(sort(ala_reasons()$id),c(0:8,10:12))
        expect_error(ala_reasons(TRUE)) ## this should throw and error because there is an unused argument
    })
}
check_caching(thischeck)

thischeck=function() {
    test_that("occurrences summary works when no qa are present", {
        expect_output(summary(occurrences(taxon="Amblyornis newtonianus",download_reason_id=10,qa='none')),"no assertion issues")
    })
}
check_caching(thischeck)

thischeck=function() {
    test_that("occurrences summary gives something sensible", {
        expect_output(summary(occurrences(taxon="Amblyornis newtonianus",download_reason_id=10)),"^number of names")
    })
}
check_caching(thischeck)

thischeck=function() {
    test_that("occurrences retrieves the fields specified", {
        expect_equal(sort(names(occurrences(taxon="Eucalyptus gunnii",fields=c("latitude","longitude"),qa="none",fq="basis_of_record:LivingSpecimen",download_reason_id=10)$data)),c("latitude","longitude"))
        expect_error(occurrences(taxon="Eucalyptus gunnii",fields=c("blahblahblah"),download_reason_id=10))
    })
}
check_caching(thischeck)

