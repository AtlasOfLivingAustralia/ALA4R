context("Test check status code helper function")

thischeck <- function() {
  test_that("check status code handles string and numeric inputs", {
    skip_on_cran()
    expect_equal(check_status_code(200), 0)
    expect_equal(check_status_code("200"), 0)
    expect_error(check_status_code(c()))
    # expect a warning for an invalid code
    expect_warning(check_status_code(600))
  })
}
check_caching(thischeck)

thischeck <- function() {
  test_that("check status code handles redirect function", {
    skip_on_cran()
    redirect_function <- function(status) {
      return(status)
    }
    expect_equal(check_status_code(300, on_redirect = redirect_function),
                 "300")
  })
}
check_caching(thischeck)

thischeck <- function() {
  test_that("check status code handles 500 codes", {
    skip_on_cran()
    expect_error(check_status_code(500))
    resp <- GET("http://httpstat.us/500")
    expect_error(check_status_code(resp))
  })
}

check_caching(thischeck)

thischeck <- function() {
  test_that("check status code handles 400 codes", {
    skip_on_cran()
    expect_error(check_status_code(400))
    # use known 404 page
    resp <- GET("https://bie-ws.ala.org.au/ws/species/bulklookup.json")
    expect_error(check_status_code(resp))
  })
}
check_caching(thischeck)

thischeck <- function() {
  test_that("check status code handles full response", {
    skip_on_cran()
    resp <- GET("https://bie-ws.ala.org.au/ws/admin/indexFields")
    expect_equal(check_status_code(resp), 0)
    # check compatibility with older httr
    resp$headers$status <- "200"
    expect_equal(check_status_code(resp), 0)
  })
}
check_caching(thischeck)
