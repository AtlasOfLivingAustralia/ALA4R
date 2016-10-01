context("Test field-related functions")

## ala_fields
thischeck=function() {
    test_that("ala_fields works as expected", {
        expect_gt(nrow(ala_fields(fields_type="occurrence")),570)
        expect_lt(nrow(ala_fields(fields_type="occurrence_indexed")),nrow(ala_fields(fields_type="occurrence")))
        expect_lt(nrow(ala_fields(fields_type="occurrence_stored")),nrow(ala_fields(fields_type="occurrence")))
        expect_gt(nrow(ala_fields(fields_type="general")),75)
        expect_gt(nrow(ala_fields(fields_type="assertions")),85)
        expect_gt(nrow(ala_fields(fields_type="layers")),410)
        expect_error(ala_fields("b"))
        expect_error(ala_fields(1))
    })
}
check_caching(thischeck)

## field_info
thischeck=function() {
    test_that("field_info does things", {
        expect_is(field_info("blah"),"data.frame") ## invalid field name
        ala_config(warn_on_empty=TRUE)
        expect_warning(field_info("blah"))
        ala_config(warn_on_empty=FALSE)
        expect_equal(nrow(field_info("blah")),0)
    })
}
check_caching(thischeck)

thischeck=function() {
    test_that("field_info on layers copes with all possible layers", {
        skip("skipping test, is slow")
        ## this test quite slow
        tt = ala_fields('layers')
        for (ii in tt$id) {
            expect_gt(nrow(field_info(ii)),0) ## this fails where JSON file too large
        }
    })
}
check_caching(thischeck)

