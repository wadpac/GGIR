library(GGIR)
context("POSIX")
test_that("g.impute returns a non-empty dataframe", {
  # POSIXtime2iso8601
  x ="2017-05-07 13:15:17 CEST"
  tz = "Europe/Amsterdam"
  x_converted = POSIXtime2iso8601(x,tz)
  expect_equal(as.character(x_converted),"2017-05-07T13:15:17+0200")
  
  # iso8601chartime2POSIX
  x ="2017-05-07T13:00:00+0200"
  tz = "Europe/Amsterdam"
  x_converted = iso8601chartime2POSIX(x,tz)
  expect_equal(as.character(x_converted),"2017-05-07 13:00:00")
})