# TODO list
# Add argument for writing output matrix as logical, numeric, factor, or integer
# Add argument for keeping unused levels of interactions




#' Create a design matrix withthe given days with for given design terms
#'
#' @param x A vector of dates.
#' @param weekdays A character vector of the weekdays to include.
#' @param months A numeric vector of the months to include.
#' @param holidays TODO (A character vector of the holidays to include.)
#' @param years A numeric vector of the years to include.
#' @param interactions TODO (A character vector of the weekdays to include.)
#' @param dayOfMonth tTODO A numeric vector of the day of the month to include.
#' @param daysInMonth TODO A numeric vector of the days in the month to include.
#' @param leapYear tTODO A logical that indicates whether to include a leap year indicator.
#' @param quarters A character vector of the quarters to include.
#' @param weeks A numeric vector to include the week number of the year.
#' @param weekend A logical that indicates if a weekend (Saturday or Sunday) indicator should be included
#' @param weekday A logical that indicates if a weekday (Monday through Friday) indicator should be included
#' @param removeUnusedLevels A TODO logical that indicates if colinear terms of the return matrix should be removed by dropping columns with zero variance.
#' @param returnType TODO A character vector indicating the data type for the return matrix. Acceptable values are "integer", "factor", "numeric", and "logical".
#' @return The design matrix for the input dates.
#' @examples
#' alldates <- seq(from = as.Date("2000-01-01"), to = as.Date("2005-10-01"), by = "days")
#'
#' designMatrix(x = alldates, weekdays = "all", months = "January")
#' designMatrix(x = alldates, weekdays = c("Wednesday", "Friday"), quarters = "all")
#' designMatrix(x = alldates, months = "all", years = c(2002, 2004))
#' @export
designMatrix <- function(x = NULL, weekdays = NULL, months = NULL,
                         holidays = NULL, years = NULL,
                         interactions = NULL, dayOfMonth = NULL,
                         daysInMonth = NULL, leapYear = FALSE,
                         quarters = NULL,
                         weeks = NULL, weekend = FALSE,
                         weekday = FALSE,
                         removeUnusedLevels = FALSE,
                         returnType = "numeric"){

  monthnames <- c("January", "February", "March", "April", "May",
                  "June", "July", "August", "September", "October",
                  "November", "December")
  weekdaynames <- c("Sunday", "Monday", "Tuesday", "Wednesday",
                    "Thursday", "Friday", "Saturday")
  quarternames <- c("Q1", "Q2", "Q3", "Q4")

  # Match arguments
  # Test for empty design terms
  if(is.null(weekdays) & is.null(months) & is.null(holidays)
     & is.null(years) & is.null(interactions)
     & is.null(daysInMonth) & is.null(dayOfMonth)
     & leapYear & is.null(quarters)
     & is.null(weeks) & !weekday & !weekend){
    stop("No design terms specified")
  }
  # Test for correct weekdays
  if(length(weekdays) > 7){
    stop("More than 7 weekdays were specified")
  }else if(length(weekdays)){
    if(!all(is.element(weekdays, c("all", weekdaynames)))){
      stop("Weekdays incorrectly specified")
    }
  }
  # Test for correct months
  if(length(months) > 12){
    stop("More than 12 months were specified")
  }else if(length(months)){
    if(!all(is.element(months, c("all", monthnames))))
      stop("Months incorrectly specified")
  }
  if(length(quarters) > 4){
    stop("More than 4 quarters were specified")
  }else if(length(quarters)){
    if(!all(is.element(quarters, c("all", quarternames))))
      stop("Quarters incorrectly specified")
  }

  # Test for x input
  if(is.null(x)){
    stop("Dates missing")
  }
  if(class(x) != "Date"){
    tryCatch(as.Date(x), error = stop("Input x cannot be coerced to a date object"))
    x <- as.Date(x)
  }
  if(!length(x)){
    stop("Input x must not have zero length")
  }

  # Create day of week, month, and day of month
  DOW <- lubridate::wday(x)
  #DAY <- lubridate::mday(x)
  MONTH <- lubridate::month(x)
  YEAR <- lubridate::year(x)
  QUARTER <- lubridate::quarter(x)
  WEEK <- lubridate::week(x)
  MDAY <- lubridate::mday(x)
  LY <- matrix(as.integer(lubridate::leap_year(x)), nrow = length(x))
  names(LY) <- "LeapYear"



  # Build design matrices for DOW, month, year, and quarter
  matrixout <- matrix(nrow = length(x), ncol = 0)
  dowMat <- model.matrix(~factor(DOW) - 1)
  colnames(dowMat) <- weekdaynames
  weekendMat <- as.matrix(as.numeric(dowMat[, 6] | dowMat[, 7]))
  colnames(weekendMat) <- "Weekend"
  weekdayMat <- as.matrix(as.numeric(weekendMat == 0))
  colnames(weekdayMat) <- "Weekday"
  monMat <- model.matrix(~factor(MONTH) - 1)
  colnames(monMat) <- monthnames
  yearMat <- model.matrix(~factor(YEAR) - 1)
  colnames(yearMat) <- levels(factor(YEAR))
  quarterMat <- model.matrix(~factor(QUARTER) - 1)
  colnames(quarterMat) <- quarternames
  weekMat <- model.matrix(~factor(WEEK) - 1)
  colnames(weekMat) <- levels(factor(WEEK))
  domMat <- model.matrix(~factor(MDAY - 1))
  colnames(domMat) <- levels(factor(MDAY))


  # Select the indicated variables and bind to a final output matrix
  if(is.element("all", tolower(weekdays))){
    matrixout <- cbind(matrixout, dowMat)
  }else if(!is.null(weekdays)){
    matrixout <- cbind(matrixout, dowMat[TRUE, weekdays])
  }
  if(is.element("all", tolower(months))){
    matrixout <- cbind(matrixout, monMat)
  }else if(!is.null(months)){
    matrixout <- cbind(matrixout, monMat[TRUE, months])
  }
  if(is.element("all", tolower(quarters))){
    matrixout <- cbind(matrixout, quarterMat)
  }else if(!is.null(quarters)){
    matrixout <- cbind(matrixout, quarterMat[TRUE, quarters])
  }
  if(is.element("all", tolower(years))){
    matrixout <- cbind(matrixout, yearMat)
  }else if(!is.null(years)){
    matrixout <- cbind(matrixout, yearMat[TRUE, as.character(years)])
  }
  if(is.element("all", tolower(weeks))){
    matrixout <- cbind(matrixout, weekMat)
  }else if(!is.null(weeks)){
    matrixout <- cbind(matrixout, weekMat[TRUE, as.character(weeks)])
  }
  if(weekend){
    matrixout <- cbind(matrixout, weekendMat)
  }
  if(weekday){
    matrixout <- cbind(matrixout, weekdayMat)
  }
  if(is.element("all", tolower(quarters))){
    matrixout <- cbind(matrixout, quarterMat)
  }else if(!is.null(quarters)){
    matrixout <- cbind(matrixout, quarterMat[TRUE, as.character(quarters)])
  }
  if(is.element("all", tolower(dayOfMonth))){
    matrixout <- cbind(matrixout, domMat)
  }else if(!is.null(dayOfMonth)){
    matrixout <- cbind(matrixout, domMat[TRUE, as.character(dayOfMonth)])
  }
  if(leapYear){
    matrixout <- cbind(matrixout, LY)
  }

  #Test for colinearity
  if(Matrix::rankMatrix(matrixout) < ncol(matrixout)){
    warning("Matrix has perfect colinearity")
  }
  return(matrixout)
}

#' designmatrix: A package for creating design matrices with dates.
#'
#' The designmatrix package creates deisgn/model matrices for dates.
#' The workhorse designMatrix function supports many differetn terms
#' (e.g. weekday, month, leap years, holidays, etc). Some of these
#' are in development and as yet not implemented.
#'
#' @section designmatrix functions:
#' designMatrix
#'
#' @docType package
#' @name designMatrix
NULL
#> NULL

#' #@importFrom lubridate quarter week month year wday mday
#' #@importFrom stats model.matrix
#' #@importFrom Matrix rankMatrix
#' 
