context("Testing utility functions")

test_that("url builder works correctly", {
    expect_that(build_url_from_parts("http://a.b",""),equals("http://a.b/"))
    expect_that(build_url_from_parts("http://a.b"),equals("http://a.b/"))
    expect_that(build_url_from_parts("http://a.b","thing"),equals("http://a.b/thing"))
    expect_that(build_url_from_parts("http://a.b","thing/"),equals("http://a.b/thing/"))
    expect_that(build_url_from_parts("http://a.b",c("thing","ping")),equals("http://a.b/thing/ping"))
    expect_that(build_url_from_parts("http://a.b",list("thing","ping")),equals("http://a.b/thing/ping"))
    expect_that(build_url_from_parts("http://a.b/zing",list("thing","ping")),equals("http://a.b/zing/thing/ping"))
    expect_that(build_url_from_parts("http://a.b/zing/",list("thing","ping")),equals("http://a.b/zing/thing/ping"))
    expect_that(build_url_from_parts("http://a.b",list(1,"ping")),equals("http://a.b/1/ping"))
})

test_that("ala_cache_filename works as expected", {
    ## basic example
    temp=ala_cache_filename("http://biocache.ala.org.au/ws/index/fields")
    expect_that(sub(".*/","",temp),equals("b2fa59b0f3a1de10b06a17d8a1616059"))
    ## test weird inputs
    expect_error(ala_cache_filename(TRUE))
    expect_error(ala_cache_filename(letters[1:26]))
})

test_that("check_fq extracts field names correctly", {
    expect_equal(extract_fq_fieldnames("occurrence_year:[2000-01-01T00:00:00Z TO 2020-01-01T23:59:59Z]"),c("occurrence_year"))
    expect_null(check_fq("occurrence_year:[2000-01-01T00:00:00Z TO 2020-01-01T23:59:59Z]","occurrence"))

    expect_null(extract_fq_fieldnames("occurrence_year[2000-01-01T00:00:00Z TO 2020-01-01T23:59:59Z]"),c("occurrence_year"))    
    expect_warning(check_fq("occurrence_year[2000-01-01T00:00:00Z TO 2020-01-01T23:59:59Z]","occurrence"))
    
    expect_equal(extract_fq_fieldnames(c("month:12","family:something AND longitude:[60 TO *]")),c("month","family","longitude"))
    expect_null(check_fq(c("month:12","family:something AND longitude:[60 TO *]"),"occurrence"))
    
    expect_equal(extract_fq_fieldnames(c("month : 12","family:something AND longitude:[60 TO *]")),c("month","family","longitude"))
    expect_null(check_fq(c("month : 12","family:something AND longitude:[60 TO *]"),"occurrence"))
    
    expect_equal(extract_fq_fieldnames(" notafield:something "),"notafield")
    expect_warning(check_fq(" notafield:something ","occurrence"))
})

## not tested yet: ala_config
