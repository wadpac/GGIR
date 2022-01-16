library(GGIR)
context("g.imputeTimegaps")
test_that("timegaps are correctly imputed", {
library(testthat)
  N = 10000
  sf = 20
  x = data.frame(time = as.POSIXct(x = (1:N)/sf, tz = "", origin = "1970/1/1"),
                 X = 1:N, Y = 1:N, Z = 1:N)
  xyzCol = c("X", "Y", "Z")
  
  
  x_without_time = data.frame(X = 1:N, Y = 1:N, Z = 1:N)
  xyzCol = c("X", "Y", "Z")
  
  x1 = x
  zeros = c(1:200, 6000:6500, 7000:7500, 8000:8500)
  x1[zeros, xyzCol] = 0
  x1_imputed = g.imputeTimegaps(x1, xyzCol, timeCol = "time", sf = sf, k = 2/sf)
  
  expect_equal(nrow(x1_imputed), N)
  expect_equal(sum(x1_imputed$X), 39068022)
  
  x2 = x
  zeros = c(7000:7500, 8000:8500)
  x2[zeros, xyzCol] = 0
  x2_imputed = g.imputeTimegaps(x2, xyzCol, timeCol = "time", sf = sf, k = 2/sf)
  
  expect_equal(nrow(x2_imputed), N)
  expect_equal(sum(x2_imputed$X), 42225082)
  
  x3 = x_without_time
  zeros = c(7000:7500, 8000:8500)
  x3[zeros, xyzCol] = 0
  x3_imputed = g.imputeTimegaps(x3, xyzCol, sf = sf, k = 2/sf)
  
  expect_equal(nrow(x3_imputed), N - 2)
  expect_equal(sum(x3_imputed$X), 42225081)
  
})
 