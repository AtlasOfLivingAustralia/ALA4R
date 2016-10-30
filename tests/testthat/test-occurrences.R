context("Test occurrence-related functions")

## ala_reasons
thischeck <- function() {
    test_that("ala_reasons works as expected", {
        expect_named(ala_reasons(),c("rkey","name","id"))
        expect_equal(nrow(ala_reasons()),12)
        expect_equal(sort(ala_reasons()$id),c(0:8,10:12))
        expect_error(ala_reasons(TRUE)) ## this should throw and error because there is an unused argument
        tmp <- ala_reasons()
        expect_equal(ALA4R:::convert_reason("testing"),tmp$id[tmp$name=="testing"])
        expect_error(ALA4R:::convert_reason("bilbobaggins"))
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


thischeck <- function() {
    test_that("occurrences unique does something sensible", {
        x <- occurrences(taxon="Amblyornis newtonianus",download_reason_id=10)
        xu <- unique(x,spatial=0.1)
        expect_is(xu,"list")
        expect_named(xu,c("data","meta"))
        expect_is(xu$data,"data.frame")
        expect_lt(nrow(xu$data),nrow(x$data))
        xu <- unique(x,spatial=0,temporal="yearmonth")
        expect_lt(nrow(xu$data),nrow(x$data))
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("occurrences subset does something sensible", {
        x <- occurrences(taxon="Amblyornis newtonianus",download_reason_id=10)
        xs <- subset(x)
        expect_is(xs,"list")
        expect_named(xs,c("data","meta"))
        expect_is(xs$data,"data.frame")
        expect_lt(nrow(xs$data),nrow(x$data))
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("occurrences checks required inputs", {
        expect_error(occurrences(taxon="data_resource_uid:dr356",method="offline",download_reason_id="testing",email=""))
        expect_error(occurrences(taxon="data_resource_uid:dr356",method="offline",download_reason_id="testing"))
        expect_error(occurrences(taxon="data_resource_uid:dr356",method="offline",download_reason_id="testing",email=NULL))
        expect_error(occurrences(taxon="Amblyornis newtonianus")) ## missing download_reason_id
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("occurrences warns on edge behaviour", {
        expect_error(expect_warning(occurrences(taxon="data_resource_uid:dr356",method="offline",download_reason_id="testing",email="testing@test.org",fields="all"))) ## url string too long, 414 error and warning
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("occurrences gives same results for offline and indexed methods", {
        x1 <- occurrences(taxon="data_resource_uid:dr356",method="offline",download_reason_id="testing",email="ala4rtesting@test.org")
        x2 <- occurrences(taxon="data_resource_uid:dr356",download_reason_id="testing")
        expect_identical(arrange(x1$data,id),arrange(x2$data,id))
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("occurrences works with records_count_only", {
        x1 <- occurrences(taxon="data_resource_uid:dr356",record_count_only=TRUE)
        expect_true(is.numeric(x1))
        expect_gt(x1,100)
    })
}
check_caching(thischeck)
