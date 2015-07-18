context("Test field-related functions")

ala_config(caching="off")

## ala_fields
test_that("ala_fields works as expected", {
    expect_that(nrow(ala_fields(fields_type="occurrence")),is_more_than(570))
    expect_that(nrow(ala_fields(fields_type="occurrence_indexed")),is_less_than(nrow(ala_fields(fields_type="occurrence"))))
    expect_that(nrow(ala_fields(fields_type="occurrence_stored")),is_less_than(nrow(ala_fields(fields_type="occurrence"))))
    expect_that(nrow(ala_fields(fields_type="general")),is_more_than(75))
    expect_that(nrow(ala_fields(fields_type="assertions")),is_more_than(85))
    expect_that(nrow(ala_fields(fields_type="layers")),is_more_than(410))
    expect_error(ala_fields("b"))
    expect_error(ala_fields(1))
})

## field_info
test_that("field_info does things", {
    expect_that(field_info("blah"),is_a('data.frame')) ## invalid field name
    expect_that(nrow(field_info("blah")),equals(0))
})
test_that("field_info on layers copes with all possible layers", {
    skip("skipping test, is slow")
    ## this test quite slow
    tt = ala_fields('layers')
    for (ii in tt$id) {
        expect_that(nrow(field_info(ii)),is_more_than(0)) ## this fails where JSON file too large
    }
})

