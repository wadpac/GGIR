library(GGIR)
context("g.part5.handle_lux_extremes")
test_that("is able to handle lux extremes", {
  skip_on_cran()
  lux = c(130000, 1, 1, 1, 130000, 1, 1, 1, 130000, 130000, 1, 1, 1, 130000, 130000, 130000, 1, 1, 130000)
  out = g.part5.handle_lux_extremes(lux)
  expect_equal(length(which(out$correction_log == 2)), 3)
  expect_equal(length(which(out$correction_log == 1)), 5)
  expect_equal(mean(out$lux, na.rm=TRUE), 1)
  
  # test also absense of extreme values
  lux = rep(1, 12)
  out = g.part5.handle_lux_extremes(lux)
  expect_equal(sum(out$lux), 12)
  expect_equal(sum(out$correction_log), 0)
})
