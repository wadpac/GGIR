library(GGIR)
context("DFA, SSP and ABI")
test_that("DFA produces expected abi and ssp from sunspot.year dataset", {
  skip_on_cran()
  ssp = SSP(sunspot.year, epochSize = 60)
  abi = ABI(ssp$alpha_overall)
  expect_equal(abi, 0.145756, tolerance = 0.0001)
  expect_equal(ssp$alpha_overall, 0.7393684, tolerance = 0.0001)
  expect_equal(length(ssp), 3)
  expect_all_true(c("alpha_overall", "alpha_1", "alpha_2") %in% names(ssp))
})

test_that("DFA skips calculation if there are NA values", {
  skip_on_cran()
  sy_tmp = sunspot.year
  sy_tmp[5] = NA
  ssp = SSP(sy_tmp, epochSize = 60)
  abi = ABI(ssp$alpha_overall)
  expect_equal(abi, NA)
  expect_equal(ssp$alpha_overall, NA)
})

test_that("DFA can handle too short time series", {
  skip_on_cran()
  ssp = SSP(sunspot.year[1:2], epochSize = 60)
  abi = ABI(ssp$alpha_overall)
  expect_equal(abi, NA)
  expect_equal(ssp$alpha_overall, NA)
})