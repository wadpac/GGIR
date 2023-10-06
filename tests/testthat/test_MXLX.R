library(GGIR)
context("MXLX")

test_that("MXLX is able to identify correct mean and timing of MX and LX", {
  # When classification is possible
  Y = rep(1, 1440)
  Y[61:360] = 2
  Y[721:1020] = 0
  is.na(Y[20:30]) = TRUE # should be able to handle NA values
  
  out = MXLX(Y = Y, X = 5, epochSize = 60, tseg = c(0, 24), resolutionMin = 10)
  
  expect_false(all(is.na(out)))
  expect_equal(out$L5hr, 12)
  expect_equal(out$L5, 0)
  expect_equal(out$L5_start, 721)
  expect_equal(out$L5_end, 1020)
  expect_equal(out$M5hr, 1)
  expect_equal(out$M5, 2000)
  expect_equal(out$M5_start, 61)
  expect_equal(out$M5_end, 360)
  
  # When time segment window is shorter we still expect to find L5 at 12 hour
  out = MXLX(Y = Y, X = 5, epochSize = 60, tseg = c(12, 24), resolutionMin = 10) 
  expect_equal(nrow(out), 1)
  expect_equal(out$L5hr, 12)
  expect_equal(out$L5, 0)
  expect_equal(out$L5_start, 721)
  expect_equal(out$L5_end, 1020)
  
  # When time segment window is shorter we still expect to find M5 at 1 hour
  out = MXLX(Y = Y, X = 5, epochSize = 60, tseg = c(0, 12), resolutionMin = 10) 
  expect_equal(out$M5hr, 1)
  expect_equal(out$M5, 2000)
  expect_equal(out$M5_start, 61)
  expect_equal(out$M5_end, 360)
  
  # When recording is short
  Y = 1
  out = MXLX(Y = Y, X = 5, epochSize = 60, tseg = c(0, 24), resolutionMin = 10) 
  expect_equal(nrow(out), 1)
  expect_true(all(is.na(out)))
})


