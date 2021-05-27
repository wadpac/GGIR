library(GGIR)
context("g.part5.handle_lux_extremes")
test_that("is able to handle lux extremes", {
  lux = c(130000, 1, 1, 1, 130000, 1, 1, 1, 130000, 130000, 1, 1, 1, 130000, 130000, 130000, 1, 1, 130000)
  out = g.part5.handle_lux_extremes(lux)
  expect_equal(length(which(out$correction_log == 2)), 3)
  expect_equal(length(which(out$correction_log == 1)), 5)
  expect_equal(mean(out$lux, na.rm=TRUE), 1)
})
