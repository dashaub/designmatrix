# Unit tests for the designMatrix() function
if(require(testthat)){
  context("Testing the designMatrix() function")
  test_that("Testing valid inputs", {
    sampleDates <- seq(from = as.Date("1899-01-01"),
                       to = as.Date("2001-01-01"),
                       by = "days")
    expect_error(designMatrix(sampleDates, weekdays = "all"), NA)
    #expect_error(designMatrix(sampleDates, weekdays = "monday"), NA)
    expect_error(designMatrix(sampleDates, years = c(1900, 1950, 2000)), NA)
    expect_warning(designMatrix(sampleDates, quarters = "all"))
    expect_error(designMatrix(sampleDates, months = "all"), NA)
    expect_error(designMatrix(sampleDates, weekdays = "January",
    months = "Tuesday", years = 2005, weekend = TRUE,
    weekday = TRUE, leapYear = TRUE, dayOfMonth = 15), NA)
  })
  context("Testing the designMatrix() function with invalid syntax")
  test_that("Testing invalid inputs", {
    sampleDates <- seq(from = as.Date("1899-01-01"),
                       to = as.Date("2001-01-01"),
                       by = "days")
    expect_error(designMatrix(as.numeric(0)))
    expect_error(designMatrix(sampleDates, weekdays = 1:8))
    expect_error(designMatrix(sampleDates, weekdays = c("Monday", 3)))
    expect_error(designMatrix(sampleDates, months = 1:13))
    expect_error(designMatrix(sampleDates, months = c("February", 3)))
    expect_error(designMatrix(sampleDates, quarters = 1:5))
    expect_error(designMatrix(sampleDates, quarters = c("Q1", 3)))
    expect_error(designMatrix())
    expect_error(designMatrix("January 13 2014", days = "all"))
    expect_error(designMatrix(sampleDates))
    
  })
}
