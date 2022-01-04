library(GGIR)
context("loading and checking params")
test_that("load_params can load parameters", {
  params = load_params(group="sleep")
  expect_equal(params$params_sleep[[1]], 5)
  expect_equal(params$params_sleep[[3]], TRUE)
  expect_equal(params$params_sleep[[8]], "Y")
  expect_equal(length(params$params_sleep), 10)
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
})