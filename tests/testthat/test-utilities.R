context("Test utility functions")

test_that("url builder works correctly", {
    expect_that(ALA4R:::build_url_from_parts("http://a.b", ""),
                equals("http://a.b/"))
    expect_that(ALA4R:::build_url_from_parts("http://a.b"),
                equals("http://a.b/"))
    expect_that(ALA4R:::build_url_from_parts("http://a.b", "thing"),
                equals("http://a.b/thing"))
    expect_that(ALA4R:::build_url_from_parts("http://a.b", "thing/"),
                equals("http://a.b/thing/"))
    expect_that(ALA4R:::build_url_from_parts("http://a.b", c("thing", "ping")),
                equals("http://a.b/thing/ping"))
    expect_that(ALA4R:::build_url_from_parts(
      "http://a.b", list("thing", "ping")), equals("http://a.b/thing/ping"))
    expect_that(ALA4R:::build_url_from_parts(
      "http://a.b/zing", list("thing", "ping")),
      equals("http://a.b/zing/thing/ping"))
    expect_that(ALA4R:::build_url_from_parts(
      "http://a.b/zing/", list("thing", "ping")),
      equals("http://a.b/zing/thing/ping"))
    expect_that(ALA4R:::build_url_from_parts(
      "http://a.b", list(1, "ping")), equals("http://a.b/1/ping"))
})

test_that("ala_cache_filename works as expected", {
    ## basic example
    temp <- ala_cache_filename("http://biocache.ala.org.au/ws/index/fields")
    expect_that(sub(".*/", "", temp),
                equals("b2fa59b0f3a1de10b06a17d8a1616059"))
    ## field ordering should not matter
    expect_equal(ala_cache_filename("abcdef?b=2&a=1"),
                 ala_cache_filename("abcdef?a=1&b=2"))
    ## test weird inputs
    expect_error(ala_cache_filename(TRUE))
    expect_error(ala_cache_filename(letters[1:26]))
})

test_that("caching messages change as expected ", {
    skip_on_cran()
    ala_config(caching = "off")
    expect_message(species_info("Grevillea humilis subsp. maritima",
                                verbose = TRUE), "GETting URL")
    ala_config(caching = "refresh")
    expect_message(species_info("Grevillea humilis subsp. maritima",
                                verbose = TRUE), "Caching")
    ala_config(caching = "on")
    expect_message(species_info("Grevillea humilis subsp. maritima",
                                verbose = TRUE), "Using cached file")
})

thischeck <- function() {
    test_that("check_fq extracts field names correctly", {
        skip_on_cran()
        expect_equal(ALA4R:::extract_fq_fieldnames(
          "year:[2000-01-01T00:00:00Z TO 2020-01-01T23:59:59Z]"),
          c("year"))
        expect_null(ALA4R:::check_fq(
          "year:[2000-01-01T00:00:00Z TO 2020-01-01T23:59:59Z]",
          "occurrence"))

        expect_null(ALA4R:::extract_fq_fieldnames(
          "year[2000-01-01T00:00:00Z TO 2020-01-01T23:59:59Z]"))
        expect_warning(ALA4R:::check_fq(
          "year[2000-01-01T00:00:00Z TO 2020-01-01T23:59:59Z]",
          "occurrence"))
        expect_equal(ALA4R:::extract_fq_fieldnames(
          c("month:12", "family:something AND decimalLongitude:[60 TO *]")),
          c("month", "family", "decimalLongitude"))
        expect_null(ALA4R:::check_fq(
          c("month:12", "family:something AND decimalLongitude:[60 TO *]"),
          "occurrence"))
        expect_equal(ALA4R:::extract_fq_fieldnames(
          c("month : 12", "family:something AND decimalLongitude:[60 TO *]")),
          c("month", "family", "decimalLongitude"))
        expect_null(ALA4R:::check_fq(
          c("month : 12", "family:something AND decimalLongitude:[60 TO *]"),
          "occurrence"))
        expect_equal(ALA4R:::extract_fq_fieldnames(" notafield:something "),
                     "notafield")
        expect_warning(ALA4R:::check_fq(" notafield:something ", "occurrence"))
    })
}
check_caching(thischeck)


test_that("ala config works as expected", {
  skip_on_cran()
  ala_config("reset")
  conf <- ala_config()
  expect_equal(c(conf$download_reason_id, conf$warn_on_empty, conf$caching,
                 conf$verbose, conf$text_encoding),
               c(4, FALSE, "on", FALSE, "UTF-8"))
  expect_error(ala_config(bad_option = 3))
  # test numeric conversion
  ala_config(download_reason_id = "testing")
  expect_equal(ala_config()$download_reason_id, 10)
  expect_error(ala_config(cache_directory = "non_existent_directory"))
  expect_error(ala_config(verbose = "false"))
  expect_error(ala_config(warn_on_empty = "false"))
  expect_error(ala_config(user_agent = 1))
})
