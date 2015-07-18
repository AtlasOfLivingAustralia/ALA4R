context("Check assertion-related functions")

test_that("NULL returned when no assertions present in input", {
    temp=data.frame()
    class(temp)=c("occurrences",class(temp))
    expect_null(check_assertions(temp))
})

test_that("check_assertions checks class of input correctly", {
    temp=data.frame()
    expect_error(check_assertions(temp))    
    class(temp)=c("occurrences",class(temp))
    expect_null(check_assertions(temp))
})

