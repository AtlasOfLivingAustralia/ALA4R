context("Test species information functions")

## Not tested yet: species_by_site

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
    test_that("species_info generally works as expected", {
        expect_is(species_info("Grevillea humilis subsp. maritima"),"list")
        expect_error(species_info("Grevillea humilis subsp. maritima",verbose="yes"))
        expect_equal(species_info("Grevillea humilis subsp. maritima"),species_info(factor("Grevillea humilis subsp. maritima")))
        expect_true(all(names(species_info("Grevillea humilis subsp. maritima")) %in% c("taxonConcept","taxonName","classification","identifiers","synonyms","commonNames","childConcepts","parentConcepts","sameAsConcepts","pestStatuses","conservationStatuses","simpleProperties","images","imageIdentifier","distributionImages","screenshotImages","extantStatuses","habitats","regionTypes","references","publicationReference","identificationKeys","specimenHolding","categories","isAustralian","linkIdentifier","extantStatuses")))
        expect_warning(expect_is(species_info(guid="urn:lsid:biodiversity.org.au:apni.taxon:248651"),"list"))
        expect_is(species_info(guid="bilbobaggins"),"list") ## empty result should still be a list
        expect_is(species_info("bilbobaggins"),"list") ## empty result should still be a list
        ## this one no longer matches anything with new taxonomy
        ##expect_is(species_info(guid="ALA_Pterostylis_squamata")$classification,"data.frame") ## taxon with improper classification
        ##expect_equal(species_info(guid="ALA_Pterostylis_squamata")$classification[[1]],"ORCHIDACEAE") ## taxon with improper classification
    })
    test_that("text encoding works as expected",{
        ## this one now just returns "Simoselaps fasciolatus"  - need to find new example to test
        ##expect_equal(search_names("Simoselaps fasciolatus")$name,"Simoselaps fasciolatus (Günther, 1872)") ## uses POST
        expect_equal(as.character(species_info('Simoselaps fasciolatus')$taxonConcept$author),"(Günther, 1872)") ## uses GET
    })
}
check_caching(thischeck)


thischeck=function() {
    test_that("species_info gives resolvable guids for known species", {
        rsp <- httr::GET(as.character(species_info("Grevillea humilis subsp. maritima")$taxonConcept$guid))
        expect_equal(rsp$status_code,200)
        
        rsp <- httr::GET("http://id.biodiversity.org.au/node/apni/blahblahblah")
        expect_equal(rsp$status_code,404)
    })
}
check_caching(thischeck)
