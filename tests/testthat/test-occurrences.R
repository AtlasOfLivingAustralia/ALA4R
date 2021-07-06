context("Test occurrence-related functions")

## ala_reasons
thischeck <- function() {
    test_that("ala_reasons works as expected", {
        skip_on_cran()
        expect_named(ala_reasons(), c("rkey", "name", "id"))
        expect_equal(nrow(ala_reasons()), 13)
        expect_equal(sort(ala_reasons()$id), c(0:8, 10:13))
        ## this should throw an error because there is an unused argument
        expect_error(ala_reasons(TRUE))
        tmp <- ala_reasons()
        expect_equal(ALA4R:::convert_reason("testing"),
                     tmp$id[tmp$name == "testing"])
        expect_error(ALA4R:::convert_reason("bilbobaggins"))
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("occurrences summary works when no qa are present", {
        skip_on_cran()
        expect_output(summary(occurrences(taxon = "Amblyornis newtonianus",
                                          email = "ala4r@ala.org.au",
                                          download_reason_id = 10,
                                          qa = "none")), "no assertion issues")
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("occurrences summary gives something sensible", {
      skip("Broken by infrastructure upgrade")
        occ <- occurrences(taxon = "Amblyornis newtonianus",
                           email = "ala4r@ala.org.au",
                           download_reason_id = 10)
        expect_output(summary(occ), "^number of original names")
        ## check that names required for summary.occurrences method are present
        expect_true(all(c("scientificName", "scientificNameOriginal") %in%
                            names(occ$data)) ||
                        all(c("taxonName", "taxonNameOriginal") %in%
                                names(occ$data)))
        ## check that names required for unique.occurrences method are present
        expect_true(all(c("scientificName", "longitude", "latitude",
                          "eventDate", "month", "year") %in% names(occ$data)))
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("occurrences retrieves the fields specified", {
        skip("Broken by infrastructure upgrade")
        expect_equal(sort(names(
            occurrences(taxon = "Eucalyptus gunnii", email = "ala4r@ala.org.au",
                        fields = c("latitude", "longitude"), qa = "none",
                        fq = "basis_of_record:LivingSpecimen",
                        download_reason_id = 10)$data)),
            c("latitude", "longitude"))
        expect_error(occurrences(taxon = "Eucalyptus gunnii",
                                 email = "ala4r@ala.org.au",
                                 fields = c("blahblahblah"),
                                 download_reason_id = 10))
    })
}
check_caching(thischeck)


thischeck <- function() {
    test_that("occurrences unique does something sensible", {
        skip_on_cran()
        x <- occurrences(taxon = "Amblyornis newtonianus",
                         email = "ala4r@ala.org.au", download_reason_id = 10)
        Sys.sleep(20)
        xu <- unique(x, spatial = 0.1)
        expect_is(xu, "list")
        expect_named(xu, c("data", "meta"))
        expect_is(xu$data, "data.frame")
        expect_lt(nrow(xu$data), nrow(x$data))
        xu <- unique(x, spatial = 0, temporal = "yearmonth")
        expect_lt(nrow(xu$data), nrow(x$data))
        xu <- unique(x, temporal = "full")
        expect_true("eventDate" %in% names(xu$data))
        expect_error(unique(x, temporal = "y"))
        #drop scientific name
        x$data <- x$data[, !names(x$data) == "species"]
        expect_error(unique(x))
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("occurrences subset does something sensible", {
        skip("Broken by infrastructure upgrade")
        x <- occurrences(taxon = "Amblyornis newtonianus",
                         email = "ala4r@ala.org.au", download_reason_id = 10)
        Sys.sleep(20)
        xs <- subset(x)
        expect_is(xs, "list")
        expect_named(xs, c("data", "meta"))
        expect_is(xs$data, "data.frame")
        expect_lt(nrow(xs$data), nrow(x$data))
        expect_error(subset(x, exclude.spatial = "some"))
        xs <- subset(x, exclude.taxonomic = "warning")
        expect_lt(nrow(xs$data), nrow(x$data))
        expect_error(subset(x, max.spatial.uncertainty = ""))
        xs <- subset(x, max.spatial.uncertainty = 2)
        expect_lt(nrow(xs$data), nrow(x$data))
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("occurrences checks required inputs", {
        skip_on_cran()
        expect_error(occurrences())
        expect_error(occurrences(taxon = "data_resource_uid:dr356",
                                 reason = 10, email = "ala4r@ala.org.au"))
        expect_error(occurrences(taxon = "data_resource_uid:dr356",
                                 download_reason_id = "testing", email = ""))
        expect_error(occurrences(taxon = "data_resource_uid:dr356",
                                 download_reason_id = "testing"))
        expect_error(occurrences(taxon = "data_resource_uid:dr356",
                                 download_reason_id = "testing", email = NULL))
        ## missing download_reason_id
        expect_error(occurrences(taxon = "Amblyornis newtonianus"))
        expect_warning(occurrences(taxon = "Amblyornis newtonianus",
                                   email = "ala4r@ala.org.au",
                                   download_reason_id = 10, method = "indexed"))
        # bad download reason
        expect_error(occurrences(taxon = "Amblyornis newtonianus",
                     email = "ala4r@ala.org.au", download_reason_id = 20))

        # bad qa fields
        expect_error(occurrences(taxon = "Amblyornis newtonianus",
                                 email = "ala4r@ala.org.au",
                                 download_reason_id = 10,
                                 qa = "bad_assertion"))
    })
}
check_caching(thischeck)

thischeck <- function() {
  test_that("occurrences handles fields correctly", {
    skip_on_cran()
    expect_is(
      occurrences(taxon = as.factor("Amblyornis newtonianus"),
                  email = "ala4r@ala.org.au",
                  download_reason_id = 10,
                  wkt = "POLYGON((145 -37,150 -37,150 -30,145 -30,145 -37))")$
        data, "data.frame")
    expect_true("images" %in%
                  names(occurrences(taxon = "Amblyornis newtonianus",
                                    email = "ala4r@ala.org.au",
                                    download_reason_id = 10,
                                    extra = "images")$data))
  })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("occurrences warns for long URLs", {
        skip_on_cran()
        ## url string too long, 414 error and warning
        expect_error(expect_warning(
            occurrences(taxon = "data_resource_uid:dr356",
                        download_reason_id = "testing",
                        email = "ala4r@ala.org.au", fields = "all")))
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("occurrences works with records_count_only", {
        skip_on_cran()
        x1 <- occurrences(taxon = "data_resource_uid:dr356",
                          record_count_only = TRUE)
        expect_true(is.numeric(x1))
        expect_gt(x1, 100)
    })
}
check_caching(thischeck)

thischeck <- function() {
  test_that("occurrences generates a doi if requested", {
    skip("Skip except for manual testing to avoid clogging up doi system with
         unnecessary dois.")
    expect_true("doi" %in% occurrences(taxon = "Acacia podalyriifolia",
                                       email = "ala4r@ala.org.au",
                                       download_reason_id = "testing",
                                       generateDoi = TRUE)$meta$doi)
    })
}
