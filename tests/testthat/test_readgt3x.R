library(GGIR)
context("read.gt3x_ggir")
test_that("read.gt3x_ggir handles tz correctly", {
  skip_on_cran()
  filename  = system.file("testfiles/actigraph_testfile.gt3x", package = "GGIR")[1]
  options(digits.secs = 3)
  tzLA = "America/Los_Angeles"
  D = read.gt3x_ggir(path = filename, verbose = FALSE, asDataFrame = TRUE,
                     desiredtz = tzLA, configtz = tzLA,
                     batch_begin = 1, batch_end = 1)

  expect_equal(nrow(D), 30)
  expect_equal(as.character(D$time[2]), "2020-08-26 10:09:00.03332")
  expect_equal(D$X[1], 0.004)
  expect_equal(D$Y[2], -0.004)
  expect_equal(D$Z[1], -0.957)
  
  
  tzAms = "Europe/Amsterdam"
  tzLon = "Europe/London"
  # desiredtz == configtz
  D = read.gt3x_ggir(path = filename, verbose = FALSE, asDataFrame = TRUE,
                     desiredtz = tzAms, configtz = tzAms,
                     batch_begin = 1, batch_end = 1)
  expect_equal(as.character(D$time[2]), "2020-08-26 10:09:00.03332")
  
  # desiredtz > configtz
  D = read.gt3x_ggir(path = filename, verbose = FALSE, asDataFrame = TRUE,
                     desiredtz = tzAms, configtz = tzLon,
                     batch_begin = 1, batch_end = 1)
  expect_equal(as.character(D$time[2]), "2020-08-26 11:09:00.03332")
  
  # desiredtz < configtz
  D = read.gt3x_ggir(path = filename, verbose = FALSE, asDataFrame = TRUE,
                     desiredtz = tzLon, configtz = tzAms,
                     batch_begin = 1, batch_end = 1)
  expect_equal(as.character(D$time[2]), "2020-08-26 09:09:00.03332")
  
  
})