library(GGIR)
context("is.ISO8601")
test_that("is.ISO8601 is able to detect ISO8601", {
  # west of greenwhich with negative timezone
  x = "2017-01-01 13:15:12"
  expect_equal(is.ISO8601(x),FALSE)
  tz = "America/Aruba"
  x_converted = as.character(POSIXtime2iso8601(x,tz))
  expect_equal(is.ISO8601(x_converted),TRUE)
  
  # east of greenwhich with positive timezone
  x = "2017-01-01 13:15:12"
  tz = "Europe/Amsterdam"
  x_converted = as.character(POSIXtime2iso8601(x,tz))
  expect_equal(is.ISO8601(x_converted),TRUE)
})