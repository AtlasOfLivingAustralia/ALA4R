thischeck=function() {
    test_that("search_guids can cope with factor inputs", {
        expect_equal(search_guids(factor("urn:lsid:biodiversity.org.au:afd.taxon:95773568-053d-44de-a624-5699f0ac4a59")),search_guids("urn:lsid:biodiversity.org.au:afd.taxon:95773568-053d-44de-a624-5699f0ac4a59"))
    })
}
check_caching(thischeck)

thischeck=function() {
    test_that("search_guids can cope with mixed recognized/unrecogized guids", {
        expect_equal(is.na(search_guids(c("urn:lsid:biodiversity.org.au:afd.taxon:95773568-053d-44de-a624-5699f0ac4a59","http://id.biodiversity.org.au/node/apni/2890970","this_is_not_a_valid_guid"))$guid),c(FALSE,FALSE,TRUE))
    })
}
check_caching(thischeck)


thischeck=function() {
    test_that("search_guids can cope with all-unrecogized guids", {
        expect_equal(nrow(search_guids("fljkhdlsi")),1)
        expect_equal(nrow(search_guids(c("fljkhdlsi","sdkhfowbiu"))),2)
        expect_true(all(is.na(search_guids(c("fljkhdlsi","sdkhfowbiu"))$guid)))
    })
}
check_caching(thischeck)


thischeck=function() {
    test_that("search_guids returns occurrence counts when asked", {
        expect_false(is.na(search_guids("urn:lsid:biodiversity.org.au:afd.taxon:95773568-053d-44de-a624-5699f0ac4a59",occurrence_count=TRUE)$occurrenceCount))        
        expect_equal(is.na(search_guids(c("urn:lsid:biodiversity.org.au:afd.taxon:95773568-053d-44de-a624-5699f0ac4a59","isdulfsadh"),occurrence_count=TRUE)$occurrenceCount),c(FALSE,TRUE))
        expect_true(is.na(search_guids(c("blahblah"),occurrence_count=TRUE)$occurrenceCount))
        expect_false(is.list(search_guids(c("blahblah"),occurrence_count=TRUE)$occurrenceCount))
        expect_false(is.list(search_guids(c("blahblah","jdfhsdjk"),occurrence_count=TRUE)$occurrenceCount))
        expect_false(is.list(search_guids(c("urn:lsid:biodiversity.org.au:afd.taxon:95773568-053d-44de-a624-5699f0ac4a59","blahblah","jdfhsdjk"),occurrence_count=TRUE)$occurrenceCount))
        expect_false(is.list(search_guids(c("urn:lsid:biodiversity.org.au:afd.taxon:95773568-053d-44de-a624-5699f0ac4a59","http://id.biodiversity.org.au/node/apni/2890970"),occurrence_count=TRUE)$occurrenceCount))
        expect_false(is.list(search_guids(c("http://id.biodiversity.org.au/node/apni/2890970"),occurrence_count=TRUE)$occurrenceCount))
        expect_output(print(search_guids(c("urn:lsid:biodiversity.org.au:afd.taxon:95773568-053d-44de-a624-5699f0ac4a59"),occurrence_count=TRUE)),"occurrenceCount")        
        expect_output(print(search_guids(c("urn:lsid:biodiversity.org.au:afd.taxon:95773568","isdulfsadh"),occurrence_count=TRUE)),"occurrenceCount")
        expect_null(search_guids(c("urn:lsid:biodiversity.org.au:afd.taxon:95773568-053d-44de-a624-5699f0ac4a59","isdulfsadh"),occurrence_count=FALSE)$occurrenceCount)
        expect_equal(length(grep("occurrenceCount",capture.output(print(search_guids(c("urn:lsid:biodiversity.org.au:afd.taxon:95773568","isdulfsadh"),occurrence_count=FALSE))))),0) ## "occurrenceCount" should not appear in the print(...) output
    })
}
check_caching(thischeck)
