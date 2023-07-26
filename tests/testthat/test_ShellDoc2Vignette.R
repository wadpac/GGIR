library(GGIR)
context("ShellDoc2Vignette and parametersVignette")
test_that("ShellDoc2Vignette retrieves the parameters definition correctly", {
  
  # ShellDoc2Vignette
  x = ShellDoc2Vignette(mode)
  expect_true(grepl("^Numeric", x))
  
  x = ShellDoc2Vignette("desiredtz")
  expect_true(grepl("^Character", x))
  
  x = ShellDoc2Vignette("visualreport_without_invalid")
  expect_true(grepl("^Boolean", x))
  
  x = ShellDoc2Vignette("possible_nap_dur")
  expect_true(grepl("^Numeric", x))
  
  # ParametersVignette
  x = parametersVignette("sleep")
  expect_equal(length(x), 1)
  expect_equal(class(x), "character")
  
  x = parametersVignette("metrics")
  expect_equal(length(x), 1)
  expect_equal(class(x), "character")
  
  x = parametersVignette("247")
  expect_equal(length(x), 1)
  expect_equal(class(x), "character")
  
  x = parametersVignette("rawdata")
  expect_equal(length(x), 1)
  expect_equal(class(x), "character")
  
  x = parametersVignette("phyact")
  expect_equal(length(x), 1)
  expect_equal(class(x), "character")
  
  x = parametersVignette("cleaning")
  expect_equal(length(x), 1)
  expect_equal(class(x), "character")
  
  x = parametersVignette("output")
  expect_equal(length(x), 1)
  expect_equal(class(x), "character")
  
  x = parametersVignette("general")
  expect_equal(length(x), 1)
  expect_equal(class(x), "character")
  
})
