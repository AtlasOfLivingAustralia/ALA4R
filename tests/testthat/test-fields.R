context("Test field-related functions")

## ala_fields
thischeck <- function() {
    test_that("ala_fields works as expected", {
        skip_on_cran()
        expect_gt(nrow(ala_fields(fields_type = "occurrence")), 570)
        expect_lt(nrow(ala_fields(fields_type = "occurrence_indexed")),
                  nrow(ala_fields(fields_type = "occurrence")))
        expect_gt(nrow(ala_fields(fields_type = "general")), 75)
        expect_gt(nrow(ala_fields(fields_type = "assertions")), 85)
        expect_gt(nrow(ala_fields(fields_type = "layers")), 320)
        expect_gt(nrow(ala_fields(fields_type = "images")), 15)
        expect_gt(nrow(ala_fields(as_is = FALSE, fields_type = "occurrence")),
                  620)
        expect_error(ala_fields("b"))
        expect_error(ala_fields(1))
    })
}
check_caching(thischeck)

## field_info
thischeck <- function() {
    test_that("field_info does things", {
        skip_on_cran()
        expect_is(field_info("blah"), "data.frame") ## invalid field name
        ala_config(warn_on_empty = TRUE)
        expect_warning(field_info("blah"))
        ala_config(warn_on_empty = FALSE)
        expect_equal(nrow(field_info("blah")), 0)
        expect_is(field_info(field_id = "cl916", record_count_only = TRUE),
                  "integer")
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("field_info on layers copes with all possible layers", {
        skip("skipping really slow field_info test")
        ## this test quite slow
        tt <- ala_fields("layers")
        for (ii in tt$id) {
            ## this fails where JSON file too large
            expect_gt(nrow(field_info(ii)), 0)
        }
    })
}
check_caching(thischeck)
