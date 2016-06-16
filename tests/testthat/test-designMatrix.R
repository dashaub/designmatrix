# Unit tests for the designMatrix() function
if(require(testthat)){
  context("Testing the deisgnMatrix() function")
  test_that("Testing for errors", {
    sampleDates <- seq(from = as.Date("1899-01-01"),
                       to = as.Date("2001-01-01"),
                       by = "days")
    expect_error(designMatrix(sampleDates, weekdays = "all"), NA)
    expect_error(designMatrix(sampleDates, weekdays = "monday"), NA)
    expect_error(designMatrix(sampleDates, years = c(1900, 1950, 2000)), NA)
  })
}
