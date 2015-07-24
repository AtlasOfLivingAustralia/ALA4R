context("Test image_info-related functions")

ala_config(caching="off")

test_that("extract_image_detail matches html as expected", {
    expect_equal(extract_image_detail(c("<td>blah</td><td>thing</td>","thiswillnotmatch"),"blah")[1,1],"thing")
    expect_equal(extract_image_detail(c("<td>blah</td> <td>thing</td>","thiswillnotmatch"),"blah")[2,1],as.character(NA))

    ## no matches at all
    expect_equal(dim(extract_image_detail(c("<td>blah</td> <td>thing</td>","thiswillnotmatch"),"nomatches")),c(2,0))
})

test_that("image_info works as expected on known records", {
    known_image_info=image_info("84654e14-dc35-4486-9e7c-40eb2f8d3faa")
    expect_equal(nrow(known_image_info),1)
    expect_equal(sort(names(known_image_info)),sort(c("imageIdentifier","title","creator","dataResourceUID","filename","dimensionsWXH","fileSize","dateUploaded","uploadedBy","dateTakenCreated","mimeType","zoomLevels","linearScale","imageURL","MD5Hash","SHA1Hash","sizeOnDiskIncludingAllArtifacts","rights","rightsHolder","licence","harvestedAsOccurrenceRecord")))

    known_image_info=image_info(c("84654e14-dc35-4486-9e7c-40eb2f8d3faa","b8344134-254d-4116-a98d-4a37e7362a4e"))
    expect_equal(nrow(known_image_info),2)
    expect_equal(sort(names(known_image_info)),sort(c("imageIdentifier","title","creator","dataResourceUID","filename","dimensionsWXH","fileSize","dateUploaded","uploadedBy","dateTakenCreated","mimeType","zoomLevels","linearScale","imageURL","MD5Hash","SHA1Hash","sizeOnDiskIncludingAllArtifacts","rights","rightsHolder","licence","harvestedAsOccurrenceRecord")))    
})

    
