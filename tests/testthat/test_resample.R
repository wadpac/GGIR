library(GGIR)
context("resample")
test_that("resample resamples data correctly", {
  N = 20
  set.seed(100)
  raw = matrix(rnorm(n = N*3, mean=0, sd = 1), ncol=3)
  now = Sys.time()
  time = as.numeric(now + seq(0.01, N * 0.01, by = 0.01))
  set.seed(100)
  rawTime = time + rnorm(n = N, mean=0, sd= 0.001)
  
  out_linear = resample(raw, rawTime, time, nrow(raw),1) # linear interpolation
  out_nn = resample(raw, rawTime, time, nrow(raw), 2) # nearest neighbour interpolation
  out_default = resample(raw, rawTime, time, nrow(raw))
  
  expect_equal(out_default[2,2], out_linear[2,2])
  expect_equal(round(out_linear[1,1], digits = 4), -0.4723)
  expect_equal(sum(abs(round(out_linear[,1], digits = 4))),8.6643)
  expect_equal(ncol(out_linear), 3)
  expect_equal(nrow(out_linear), 20)
  expect_equal(round(out_nn[1,1], digits = 4), -0.5022)
  expect_equal(sum(abs(round(out_nn[,1], digits = 4))), 9.9207)
  expect_equal(ncol(out_nn), 3)
  expect_equal(nrow(out_nn), 20)
})