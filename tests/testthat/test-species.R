context("Test species information functions")

ala_config(caching="off")

## Not tested yet: species_by_site

## Not tested yet: taxinfo_download

test_that("species_info generally works as expected", {
    expect_that(species_info("Grevillea humilis subsp. maritima"),is_a("list"))
    expect_that(species_info("Grevillea humilis subsp. maritima",verbose=TRUE),is_a("list"))
    expect_error(species_info("Grevillea humilis subsp. maritima",verbose="yes"))
    expect_equal(species_info("Grevillea humilis subsp. maritima"),species_info(factor("Grevillea humilis subsp. maritima")))
    expect_true(all(names(species_info("Grevillea humilis subsp. maritima")) %in% c("taxonConcept","taxonName","classification","identifiers","synonyms","commonNames","childConcepts","parentConcepts","sameAsConcepts","pestStatuses","conservationStatuses","simpleProperties","images","distributionImages","screenshotImages","extantStatuses","habitats","regionTypes","references","publicationReference","identificationKeys","specimenHolding","categories","isAustralian","linkIdentifier","extantStatusus")))
    expect_that(species_info(guid="urn:lsid:biodiversity.org.au:apni.taxon:248651"),is_a("list"))
    expect_that(species_info(guid="bilbobaggins"),is_a("list")) ## empty result should still be a list
    expect_that(species_info("bilbobaggins"),is_a("list")) ## empty result should still be a list
    expect_that(species_info(guid="ALA_Pterostylis_squamata")$classification,is_a("data.frame")) ## taxon with improper classification
    expect_equal(species_info(guid="ALA_Pterostylis_squamata")$classification[[1]],"ORCHIDACEAE") ## taxon with improper classification
})
