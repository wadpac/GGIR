library(GGIR)
context("correctOlderMilestoneData")
test_that("Function changes the class of metashort and metalong columns when needed", {
  skip_on_cran()
  
  # data prep
  time0 = as.POSIXct("2023-01-24 10:00:00 CET")
  time1 = as.POSIXct("2023-01-24 10:59:55 CET")
  timestamp = POSIXtime2iso8601(seq(time0, time1, by = 5), tz = "")
  metashort = data.frame(timestamp = as.factor(timestamp),
                         ENMO = factor(1:(60*60/5)),
                         anglez = factor(1:(60*60/5)),
                         extFun_dummy = factor(1:2))
  
  # function test
  metashort2 = correctOlderMilestoneData(metashort)
  
  # tests
  expect_equal(class(metashort$timestamp), "factor")
  expect_equal(class(metashort$ENMO), "factor")
  expect_equal(class(metashort$anglez), "factor")
  expect_equal(class(metashort$extFun_dummy), "factor")
  
  expect_equal(class(metashort2$timestamp), "character")
  expect_equal(class(metashort2$ENMO), "numeric")
  expect_equal(class(metashort2$anglez), "numeric")
  expect_equal(class(metashort2$extFun_dummy), "factor")
})
