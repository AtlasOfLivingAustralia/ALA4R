context("Test that variables are renamed correctly")

thischeck <- function() {
    test_that("acronyms remain uppercase", {
        skip_on_cran()
        expect_equal(ALA4R:::rename_variables("IMCRA", "assertions"), "iMCRA")
        expect_equal(ALA4R:::rename_variables("IMCRA", "occurrence"), "IMCRA")
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("underscores are renamed to camelCase", {
        skip_on_cran()
        expect_equal(ALA4R:::rename_variables("this_that", "occurrence"),
                     "thisThat")
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("particular variables are renamed for occurrence data", {
        skip_on_cran()
        temp <- c("scientificName - original", "matchedScientificName",
               "recordID", "xVersion", "MatchTaxonConceptGUID",
               "vernacularName", "taxonRank", "matchedsomething",
               "processedsomething", "parsedsomething")
        temp2 <- ALA4R:::rename_variables(temp, "occurrence")
        expect_true(!any(temp == temp2))
    })
}
check_caching(thischeck)
