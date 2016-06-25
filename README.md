[![Travis-CI Build Status](https://travis-ci.org/dashaub/designmatrix.svg?branch=master)](https://travis-ci.org/dashaub/designmatrix)
[![Coverage Status](https://coveralls.io/repos/github/dashaub/designmatrix/badge.svg?branch=master)](https://coveralls.io/github/dashaub/designmatrix?branch=master)

# designmatrix
Tools for creating design/model matrices with dates in R. Development has restarted (June 2016) after a long hiatus.

## Usage
Many features are not yet implemented or optimized. However, basic usage is outlined below:

```r
library(designMatrix)
useDates <- seq(from = as.Date("2010-01-01"), to = as.Date("2016-12-31", by = "days)
dm <- designMatrix(useDates, months = "all", weekdays = "all", years = "all")
library(forecast)
tsData <- arima.sim(n = length(useDates), list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)), sd = sqrt(0.1796))
aa <- auto.arima(tsData, xreg = dm)
aa

```

## License
This package is free software released under the [GPL-3](http://www.gnu.org/licenses/gpl-3.0.en.html) license.
