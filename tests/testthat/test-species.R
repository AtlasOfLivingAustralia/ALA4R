context("Test species information functions")

## Not tested yet: species_by_site

thischeck <- function() {
    test_that("species_info generally works as expected", {
        skip_on_cran()
        expect_is(species_info("Grevillea humilis subsp. maritima"), "list")
        expect_error(species_info("Grevillea humilis subsp. maritima",
                                  verbose = "yes"))
        expect_equal(species_info("Grevillea humilis subsp. maritima"),
                     species_info(factor("Grevillea humilis subsp. maritima")))
        expect_true(
          all(names(species_info("Grevillea humilis subsp. maritima")) %in%
                c("taxonConcept", "taxonName", "classification", "identifiers",
                  "synonyms", "commonNames", "childConcepts", "parentConcepts",
                  "sameAsConcepts", "pestStatuses", "conservationStatuses",
                  "simpleProperties", "images", "imageIdentifier",
                  "distributionImages", "screenshotImages", "extantStatuses",
                  "habitats", "regionTypes", "references",
                  "publicationReference", "identificationKeys",
                  "specimenHolding", "categories", "isAustralian",
                  "linkIdentifier", "variants")))

        # empty result should still be a list
        expect_is(species_info(guid = "bilbobaggins"), "list")
        expect_is(species_info("bilbobaggins"), "list")
        expect_error(species_info())
        expect_error(species_info(
          scientificname = "Grevillea humilis subsp. maritima",
          guid = "https://id.biodiversity.org.au/node/apni/2915203"))
        ala_config(warn_on_empty = TRUE)
        expect_warning(species_info(scientificname = "bilbobaggins"))
        ala_config(warn_on_empty = FALSE)
    })
    test_that("text encoding works as expected", {
        skip_on_cran()
        expect_equal(as.character(species_info("Simoselaps fasciolatus")$
                                    taxonConcept$author),
                     paste0("(G", intToUtf8(252),
                            "nther, 1872)")) ## (Günther, 1872)
    })
}
check_caching(thischeck)


thischeck <- function() {
    test_that("species_info gives resolvable guids for known species", {
        skip_on_cran()
        rsp <- httr::GET(as.character(species_info(
          "Grevillea humilis subsp. maritima")$taxonConcept$guid))
        expect_equal(rsp$status_code, 200)

        rsp <-
          httr::GET("http://id.biodiversity.org.au/node/apni/blahblahblah")
        expect_equal(rsp$status_code, 404)
    })
}
check_caching(thischeck)
