context("Test taxonomic information functions")

## taxinfo_download
thischeck=function() {
    test_that("taxinfo_download generally works as expected", {
        tx <- taxinfo_download("rk_family:SPHENISCIDAE",fields=c("guid","rk_genus","scientificName","rank"))
        expect_equal(names(tx),c("guid","genus","scientificName","rank"))
        expect_gte(nrow(tx),10) ## expect at least 10 results here
        ## matching is case-sensitive, so this should return no results
        ala_config(warn_on_empty=TRUE)
        ## expect warning here
        expect_warning(tx <- taxinfo_download("rk_family:spheniscidae",fields=c("guid","rk_genus","scientificName","rank")))
        ala_config(warn_on_empty=FALSE)
        tx <- taxinfo_download("rk_family:spheniscidae",fields=c("guid","rk_genus","scientificName","rank"))
        expect_equal(nrow(tx),0) ## expect no results here
        ## but names in data.frame should be consistent even when empty
        expect_equal(names(tx),c("guid","genus","scientificName","rank"))

        ## default fields
        expect_true(setequal(names(taxinfo_download("rk_genus:Macropus")),c("guid","rank","scientificName","establishmentMeans","genus","family","order","class","phylum","kingdom","datasetName")))
    })
}
check_caching(thischeck)

thischeck=function() {
    test_that("taxinfo_download fields thingies work", {
        f <- ala_fields("general")
        t <- taxinfo_download("rk_family:Spheniscidae",fields="all")
        expect_equal(ncol(t),nrow(f))
    })
}
check_caching(thischeck)
