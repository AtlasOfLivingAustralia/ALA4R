context("Check assertion-related functions")

thischeck <- function() {
    test_that("NULL returned when no assertions present in input", {
        temp <- data.frame()
        class(temp) <- c("occurrences",class(temp))
        expect_null(check_assertions(temp))
    })
}
check_caching(thischeck)


thischeck <- function() {
    test_that("check_assertions checks class of input correctly", {
        temp <- data.frame()
        expect_error(check_assertions(temp))    
        class(temp) <- c("occurrences",class(temp))
        expect_null(check_assertions(temp))
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("check_assertions gets all assertions in occurrences object", {
        skip_on_cran()
        x <- occurrences(taxon="Amblyornis newtonianus",
                         email="testing@test.org",
                         download_reason_id=10,
                         qa=ala_fields("assertions",as_is=TRUE)$name)  
        ## expect all assertion fields in object to be in the list of master
        ## assertion fields 
        expect_equal(length(setdiff(check_assertions(x)$name,
                                    ala_fields("assertions",
                                               as_is=TRUE)$name)),0) 
        x <- occurrences(taxon="Amblyornis newtonianus",
                         email="testing@test.org",
                         download_reason_id=10,qa="none")
        expect_null(check_assertions(x)$name)
    })
}
check_caching(thischeck)

