context("Test creation of occurrences plot")

thischeck <- function() {
  test_that("occurrences plot creates a plot ", {
    expect_error(occurrences_plot("bad_data"))
    occ <- occurrences(taxon = "Solanum torvum", email = "ala4r@ala.org.au",
                       download_reason_id = 10)
    occurrences_plot(occ)
    expect_true(file.exists("Rplots.pdf"))
    expect_warning(occurrences_plot(occ, pch = c(19, 20)))
  })
}

check_caching(thischeck)