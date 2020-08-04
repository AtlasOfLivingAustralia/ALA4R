context("Test searching functions")

thischeck <- function() {
    test_that("search_fulltext generally functions as expected", {
        skip_on_cran()
        expect_that(search_fulltext("red kangaroo"),
                    has_names(c("meta", "data")))
        ## "score" and "isAustralian" also used to be present, but no longer
        expect_true(all(c("guid", "name", "commonName", "rank", "author",
                          "occurrenceCount") %in%
                          names(search_fulltext("red kangaroo")$data)))
        ## query that should not match anything
        expect_that(nrow(search_fulltext("bilbobaggins")$data), equals(0))
        expect_that(nrow(search_fulltext("red", page_size = 20)$data),
                    equals(20))
        expect_that(search_fulltext("red kangaroo",
                                    output_format = "complete"),
                    has_names(c("meta", "data")))
        expect_that(search_fulltext("kingdom:Fungi",
                                    output_format = "complete"),
                    has_names(c("meta", "data")))
        expect_output(print(search_fulltext("red kangaroo")), "Search results:")
        ## expect extra cols here
        expect_output(print(search_fulltext("red kangaroo",
                                            output_format = "complete")),
                      "nameFormatted")
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("search_fulltext start param works as expected", {
        skip_on_cran()
        x1 <- search_fulltext("red", page_size = 10)
        x2 <- search_fulltext("red", page_size = 10, start = 2)
        ## so row 1 of x2$data should equal row 2 of x1$data ...
        ## but columns may actually be different!
        temp <- intersect(names(x1$data), names(x2$data))
        x1 <- x1$data[2, c(temp)]
        x2 <- x2$data[1, c(temp)]
        rownames(x1) <- ""
        rownames(x2) <- ""
        expect_equal(x1, x2)
    })
}
check_caching(thischeck)


thischeck <- function() {
  test_that("search_fulltext sort_by param works as expected", {
        skip_on_cran()
        expect_error(search_fulltext("red", page_size = 10, sort_by = "blurg"))
        ## sort by scientific name
        ## note that ALA sorting is case-sensitive, with A-Z preceding a-z
        temp <- search_fulltext("red", page_size = 10,
                                sort_by = "scientificName")$data$scientificName
        temp <- temp[grepl("^[A-Z]", temp)]
        expect_equal(order(temp), seq_len(length(temp)))
        ## descending order
        temp <- search_fulltext("red", page_size = 10,
                                sort_by = "scientificName", sort_dir = "desc")$
          data$scientificName
        temp <- temp[grepl("^[A-Z]", temp)]
        expect_equal(order(temp), rev(seq_len(length(temp))))
    })
}
check_caching(thischeck)

## not tested yet: S3method(print,search_fulltext)

thischeck <- function() {
    test_that("search_layers generally works as expected", {
        skip_on_cran()
        expect_that(search_layers(type = "all"), is_a("data.frame"))
        expect_that(search_layers(type = "all", output_format = "complete"),
                    is_a("data.frame"))
        expect_that(nrow(search_layers(type = "all")), is_more_than(320))
        expect_that(nrow(search_layers(type = "all", query = "bilbobaggins")),
                    equals(0))
        expect_error(search_layers(type = "bilbobaggins"))
        tmp <- search_layers(type = "shapes", query = "coral sea conservation")
        expect_lt(nchar(tmp$shortName), nchar(tmp$name))
    })
}
check_caching(thischeck)

thischeck <- function() {
  test_that("print search_layers works as expected", {
    skip_on_cran()
    l <- search_layers(type = "all", query = "fire")
    expect_output(print(l), "name")
    # source link should not appear in the output
    expect_equal(length(grep("sourceLink",
                             capture.output(
                               print(l)))), 0)
  })
}

## not tested yet: S3method(print,search_layers)

thischeck <- function() {
    test_that("search_names can cope with factor inputs", {
        skip_on_cran()
        expect_equal(search_names(factor("Grevillea humilis")),
                     search_names("Grevillea humilis"))
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("search_names can cope with all-unrecogized names", {
        skip_on_cran()
        expect_equal(nrow(search_names("fljkhdlsi")), 1)
        expect_equal(nrow(search_names(c("fljkhdlsi", "sdkhfowbiu"))), 2)
        expect_true(all(is.na(search_names(c("fljkhdlsi", "sdkhfowbiu"))$guid)))
    })
}
check_caching(thischeck)

thischeck <- function() {
    test_that("unexpected case-related behaviour in search_names has not
              changed", {
        skip_on_cran()
        expect_equal(search_names("Gallirallus australis")$name,
                     "Gallirallus australis")
        expect_equal(search_names("Gallirallus Australis")$name,
                     "Gallirallus australis")
        expect_equal(search_names("Gallirallus australi")$name,
                     as.character(NA))
        expect_equal(search_names("Gallirallus Australi")$name,
                     as.character(NA))
    })
}
check_caching(thischeck)
    
thischeck <- function() {
    test_that("nonbreaking spaces not present in names", {
        skip_on_cran()
        expect_false(any(colSums(apply(
          search_fulltext("Gallirallus australis")$data, 2,
          function(z)grepl("\ua0", z))) > 0))
        expect_false(grepl("\ua0"
                           , search_names("Gallirallus australis")$name))
    })
}
check_caching(thischeck)


thischeck <- function() {
    test_that("search_names returns occurrence counts when asked", {
        skip_on_cran()
        expect_false(is.na(search_names("Grevillea", occurrence_count = TRUE)$
                             occurrenceCount))
        expect_equal(is.na(search_names(c("Pachyptila turtur", "isdulfsadh"),
                                        occurrence_count = TRUE)$
                             occurrenceCount), c(FALSE, TRUE))
        expect_output(print(search_names(c("Pachyptila turtur", "isdulfsadh"),
                                         occurrence_count = TRUE)),
                      "occurrenceCount")
        expect_null(search_names(c("Pachyptila turtur", "isdulfsadh"),
                                 occurrence_count = FALSE)$occurrenceCount)

        ## "occurrenceCount" should not appear in the print(...) output
        expect_equal(length(grep("occurrenceCount",
                                 capture.output(
                                   print(
                                     search_names(c("Pachyptila turtur",
                                                        "isdulfsadh"),
                                                  occurrence_count = FALSE))))),
                     0)
        expect_false(is.list(search_names(c("blahblah"),
                                          occurrence_count = TRUE)$
                               occurrenceCount))
        expect_false(is.list(search_names(c("blahblah", "jdfhsdjk"),
                                          occurrence_count = TRUE)$
                               occurrenceCount))
        expect_false(is.list(search_names(c("Pachyptila turtur",
                                            "blahblah", "jdfhsdjk"),
                                          occurrence_count = TRUE)$
                               occurrenceCount))
        expect_false(is.list(search_names(c("Pachyptila turtur", "Grevillea"),
                                          occurrence_count = TRUE)$
                               occurrenceCount))
        expect_false(is.list(search_names(c("Grevillea"),
                                          occurrence_count = TRUE)$
                               occurrenceCount))
    })
}
check_caching(thischeck)

## not tested yet: S3method(print,search_names)
          

## not tested yet: export(search_partial_name)
thischeck <- function() {
  test_that("search partial names returns a dataframe with matched names", {
    skip_on_cran()
    expect_setequal(is.na(search_partial_name(taxon = "Eucalypt")$name),
                    "FALSE")
    expect_equal(nrow(search_partial_name(taxon = "eupalypt")), 0)
    expect_true(is.data.frame(search_partial_name(taxon = "Eucalpyt")))
  })
}
check_caching(thischeck)

thischeck <- function() {
  test_that("search partial names handles index type", {
    skip_on_cran()
    # need to check if index type works
    expect_error(search_partial_name(taxon = "eudy", index_type = "bad_type"))
  })
}
check_caching(thischeck)

thischeck <- function() {
  test_that("search partial names handles geo type", {
    skip_on_cran()
    # need to check if index type works
    expect_error(search_partial_name(taxon = "eudy", geo_only = ""))
  })
}
check_caching(thischeck)

thischeck <- function() {
  test_that("search partial names doesn't return more names than the limit", {
    skip_on_cran()
    expect_lte(nrow(search_partial_name(taxon = "Eudypt", limit = 2)), 2)
  })
}
check_caching(thischeck)

## not tested yet: S3method(print,search_partial_name)
