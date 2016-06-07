if(require(testthat)){
  context("Testing the deisgnMatrix() function")
  testthat("Testing for errors", {
    sampleDates <- seq(from = as.Date("1899-01-01"),
                       to = as.Date("2001-01-01"),
                       by = "days")
    expect_error(designMatrix(sampleDates, weekdays = "all"), NA)
  })
}
