context("Test image_info-related functions")

expected_property_names <- sort(c('imageIdentifier', 'imageUrl', 'success','mimeType','originalFileName','sizeInBytes','rights','rightsHolder','dateUploaded','dateTaken','imageUrl','tileUrlPattern','mmPerPixel','height','width','tileZoomLevels','description','title','creator','license','recognisedLicence','recognisedLicence.acronym','recognisedLicence.id','recognisedLicence.imageUrl','recognisedLicence.name', 'recognisedLicence.url', 'dataResourceUid','occurrenceID'))

thischeck <- function() {
    test_that("image_info works as expected on a single known record", {
        skip_on_cran()
        known_image_info <- image_info("84654e14-dc35-4486-9e7c-40eb2f8d3faa")
        expect_equal(nrow(known_image_info), 1)
        expect_equal(sort(names(known_image_info)), expected_property_names)
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("image info works as expected on multiple known records", {
        skip_on_cran()
        known_image_info <- image_info(c("84654e14-dc35-4486-9e7c-40eb2f8d3faa", "05436aca-bb91-4801-8633-3616c6a3077e"))
        expect_equal(nrow(known_image_info), 2)
        expect_equal(sort(colnames(known_image_info)), expected_property_names)  
    })
}

thischeck <- function() {    
    test_that("image_info works with un-matched records", {
        skip_on_cran()
        mixed_image_info <- image_info(c("84654e14-dc35-4486-9e7c-40eb2f8d3faa", "this-is-an-invalid-image-id", "05436aca-bb91-4801-8633-3616c6a3077e", "this-is-also-an-invalid-image-id"))
        expect_equal(nrow(mixed_image_info), 4)
        expect_equal(sort(names(mixed_image_info)), expected_property_names)
        unmatched_image_info <- image_info("this-is-an-invalid-image-id")
        expect_equal(nrow(unmatched_image_info), 1)
        expect_equal(sort(names(unmatched_image_info)), c("imageIdentifier", "imageURL", "success"))
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("image_info gives error if id missing", {
        expect_error(image_info())
    })
}
check_caching(thischeck)
