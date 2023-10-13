library(GGIR)
context("MXLX")

test_that("MXLX is able to identify correct mean and timing of MX and LX", {
  Y = rep(1, 1440)
  # An M5 at 1:00 with M5 == 2
  Y[61:360] = 2 
  # An L5 at 12:00 with L5 == 0
  Y[721:1020] = 0 
  # A second 'M5' at 18:00 with M5 == 1.5 which we expect to find when only
  # the second half of the data is passed on
  Y[1081:1380] = 1.5
  # Add some missing values as code should be able to handle this
  is.na(Y[20:30]) = TRUE 

  # Full 24 hours
  out1 = MXLX(Y = Y, X = 5, epochSize = 60, tseg = c(0, 24), resolutionMin = 10, shift = 0)
  expect_false(all(is.na(out1)))
  expect_equal(out1$L5hr, 12)
  expect_equal(out1$L5, 0)
  expect_equal(out1$start_L5, 721)
  expect_equal(out1$end_L5, 1020)
  expect_equal(out1$M5hr, 1)
  expect_equal(out1$M5, 2)
  expect_equal(out1$start_M5, 61)
  expect_equal(out1$end_M5, 360)
  
  # When selecting only the last 12 hours we still expect to find L5 at 12:00
  out2 = MXLX(Y = Y, X = 5, epochSize = 60, tseg = c(12, 24), resolutionMin = 10, shift = 0) 
  expect_equal(nrow(out2), 1)
  expect_equal(out2$L5hr, 12)
  expect_equal(out2$L5, 0)
  expect_equal(out2$start_L5, 721)
  expect_equal(out2$end_L5, 1020)
  expect_equal(out2$M5hr, 18)
  expect_equal(out2$M5, 1.5)
  expect_equal(out2$start_M5, 1081)
  expect_equal(out2$end_M5, 1380)
  
  # When selecting only the first 12 hours we still expect to find M5 at 1:00
  out3 = MXLX(Y = Y, X = 5, epochSize = 60, tseg = c(0, 12), resolutionMin = 10, shift = 0) 
  expect_equal(nrow(out3), 1)
  expect_equal(out3$M5hr, 1)
  expect_equal(out3$M5, 2)
  expect_equal(out3$start_M5, 61)
  expect_equal(out3$end_M5, 360)
  # and L5 is now the first 5 hour block after M5
  expect_equal(out3$L5hr, 6.3333, tolerance = 0.001)
  expect_equal(out3$L5, 1)
  expect_equal(out3$start_L5, 381)
  expect_equal(out3$end_L5, 680)
  
  # When only passing on the second 12 hours (note: Y is trimmed now, so we need to specify shift)
  # we expect results to be identical to selecting only second half but indices shifted
  # as the indices need to refer to the new Y time series
  out4 = MXLX(Y = Y[721:1440], X = 5, epochSize = 60, tseg = c(12, 24), resolutionMin = 10, shift = 12) 
  expect_equal(nrow(out4), 1)
  expect_equal(out4$L5hr, 12)
  expect_equal(out4$L5, 0)
  expect_equal(out4$start_L5, 1)
  expect_equal(out4$end_L5, 300)
  expect_equal(out4$M5hr, 18)
  expect_equal(out4$M5, 1.5)
  expect_equal(out4$start_M5, 361)
  expect_equal(out4$end_M5, 660)

  # When recording is short
  Y = 1
  out = MXLX(Y = Y, X = 5, epochSize = 60, tseg = c(0, 24), resolutionMin = 10) 
  expect_equal(nrow(out), 1)
  expect_true(all(is.na(out)))
})


