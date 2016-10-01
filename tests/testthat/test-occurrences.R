context("Test occurrence-related functions")

## ala_reasons
thischeck <- function() {
    test_that("ala_reasons works as expected", {
        expect_named(ala_reasons(),c("rkey","name","id"))
        expect_equal(nrow(ala_reasons()),12)
        expect_equal(sort(ala_reasons()$id),c(0:8,10:12))
        expect_error(ala_reasons(TRUE)) ## this should throw and error because there is an unused argument
        tmp <- ala_reasons()
        expect_equal(convert_reason("testing"),tmp$id[tmp$name=="testing"])
        expect_error(convert_reason("bilbobaggins"))
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("occurrences summary works when no qa are present", {
        expect_output(summary(occurrences(taxon="Amblyornis newtonianus",download_reason_id=10,qa="none")),"no assertion issues")
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("occurrences summary gives something sensible", {
        occ <- occurrences(taxon="Amblyornis newtonianus",download_reason_id=10)
        expect_output(summary(occ),"^number of original names")
        ## check that names required for summary.occurrences method are present
        expect_true(all(c("scientificName","scientificNameOriginal") %in% names(occ$data)) || all(c("taxonName","taxonNameOriginal") %in% names(occ$data)))
        ## check that names required for unique.occurrences method are present
        expect_true(all(c("scientificName","longitude","latitude","eventDate","month","year") %in% names(occ$data)))
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("occurrences retrieves the fields specified", {
        expect_equal(sort(names(occurrences(taxon="Eucalyptus gunnii",fields=c("latitude","longitude"),qa="none",fq="basis_of_record:LivingSpecimen",download_reason_id=10)$data)),c("latitude","longitude"))
        expect_error(occurrences(taxon="Eucalyptus gunnii",fields=c("blahblahblah"),download_reason_id=10))
    })
}
check_caching(thischeck)

