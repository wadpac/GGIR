library(GGIR)
context("loading and checking params")
test_that("load_params can load parameters", {
  params = load_params()
  expect_equal(params$params_sleep[[1]], 5)
  expect_equal(params$params_sleep[[3]], TRUE)
  expect_equal(params$params_sleep[[7]], "Y")
  expect_equal(params$params_cleaning[[6]], 0)

  # Test length of objects
  expect_equal(length(params), 8)
  expect_equal(length(params$params_sleep), 20)
  expect_equal(length(params$params_metrics), 41)
  expect_equal(length(params$params_rawdata), 36)
  expect_equal(length(params$params_247), 20)
  expect_equal(length(params$params_cleaning), 18)
  expect_equal(length(params$params_phyact), 13)
  expect_equal(length(params$params_output), 16)
  expect_equal(length(params$params_general), 14)

  params_sleep = params$params_sleep
  # Test that parameter check does not generate warnings:
  expect_warning(check_params(params_sleep), regexp = NA)
  # Test that parameter check does not generate errors:
  expect_error(check_params(params_sleep), regexp = NA)
  # Test that parameter check produces error when numeric is given a character value
  params_sleep$timethreshold = "ABC"
  expect_error(check_params(params_sleep), regexp = "Sleep argument timethreshold is not numeric")
  # Test that parameter check produces error when character is given a numeric value
  params = load_params(group="sleep")
  params_sleep = params$params_sleep
  params_sleep$Sadeh_axis = 123
  expect_error(check_params(params_sleep), regexp = "Sleep argument Sadeh_axis is not character")
  # Test that parameter check produces warning when rmc.desiredtz is provided
  params = load_params(group=c("rawdata", "general"))
  params_rawdata = params$params_rawdata
  params_general = params$params_general
  params_rawdata$rmc.desiredtz = "Europe/Madrid"
  expect_warning(check_params(params_rawdata = params_rawdata,
                              params_general = params_general), 
                 regexp = "Argument rmc.desiredtz is scheduled to be deprecated in GGIR because its functionality overlaps with desiredtz. Please, use desiredtz instead of rmc.desiredtz in the future.")
  # Test that parameter check produces warning when rmc.configtz is provided
  params = load_params(group=c("rawdata", "general"))
  params_rawdata = params$params_rawdata
  params_general = params$params_general
  params_rawdata$rmc.configtz = "Europe/Madrid"
  expect_warning(check_params(params_rawdata = params_rawdata,
                              params_general = params_general), 
                 regexp = "Argument rmc.configtz is scheduled to be deprecated in GGIR because its functionality overlaps with configtz. Please, use configtz instead of rmc.configtz in the future.")
})
