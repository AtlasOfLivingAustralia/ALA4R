context("Test image_info-related functions")

expected_property_names=sort(c("imageIdentifier","title","creator","dataResourceUID","filename","dimensionsWXH","fileSize","dateUploaded","uploadedBy","dateTakenCreated","mimeType","zoomLevels","linearScale","imageURL","MD5Hash","SHA1Hash","sizeOnDiskIncludingAllArtifacts","rights","rightsHolder","licence","harvestedAsOccurrenceRecord"))

thischeck=function() {
    test_that("extract_image_detail matches html as expected", {
##        expect_true(FALSE)
        expect_equal(ALA4R:::extract_image_detail(c("<td>blah</td><td>thing</td>","thiswillnotmatch"),"blah")[1,1],"thing")
        expect_equal(ALA4R:::extract_image_detail(c("<td>blah</td> <td>thing</td>","thiswillnotmatch"),"blah")[2,1],as.character(NA))
        ## no matches at all: special case because extract_image_details enforces the columns imageURL and imageIdentifier ith stringr v0.6
        if (packageVersion("stringr")<"1.0") {
            expect_equal(dim(ALA4R:::extract_image_detail(c("<td>blah</td> <td>thing</td>","thiswillnotmatch"),"nomatches")),c(2,2))
        } else {
            expect_equal(dim(ALA4R:::extract_image_detail(c("<td>blah</td> <td>thing</td>","thiswillnotmatch"),"nomatches")),c(2,0))
        }        
    })
}
check_caching(thischeck)


thischeck=function() {
    test_that("image_info works as expected on known records", {
        known_image_info=image_info("84654e14-dc35-4486-9e7c-40eb2f8d3faa")
        expect_equal(nrow(known_image_info),1)
        expect_equal(sort(names(known_image_info)),expected_property_names)
        known_image_info=image_info(c("84654e14-dc35-4486-9e7c-40eb2f8d3faa","39836d30-0761-473d-bac2-9ed9494fd37e"))
        expect_equal(nrow(known_image_info),2)
        expect_equal(sort(names(known_image_info)),expected_property_names)    
    })
}
check_caching(thischeck)

thischeck=function() {    
    test_that("image_info works with un-matched records", {
        mixed_image_info=image_info(c("84654e14-dc35-4486-9e7c-40eb2f8d3faa","this-is-an-invalid-image-id","39836d30-0761-473d-bac2-9ed9494fd37e","this-is-also-an-invalid-image-id"))
        expect_equal(nrow(mixed_image_info),4)
        expect_equal(sort(names(mixed_image_info)),expected_property_names)
        unmatched_image_info=image_info("this-is-an-invalid-image-id")
        expect_equal(nrow(unmatched_image_info),1)
        expect_equal(sort(names(unmatched_image_info)),c("imageIdentifier","imageURL"))
    })
}
check_caching(thischeck)


thischeck=function() {
    test_that("image_info handles embedded html in property value td block", {    
        expect_equal(sort(names(image_info("b8344134-254d-4116-a98d-4a37e7362a4e"))),expected_property_names)
    })
}
check_caching(thischeck)

thischeck=function() {
    test_that("image_info gives error if id missing", {
        expect_error(image_info())
    })
}
check_caching(thischeck)



    
