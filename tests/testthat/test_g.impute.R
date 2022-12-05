library(GGIR)
context("g.impute")
test_that("g.impute returns a non-empty dataframe", {
  data(data.getmeta)
  data(data.inspectfile)
  data(data.calibrate)
  IMP = g.impute(M = data.getmeta, I = data.inspectfile)
  expect_equal(length(IMP),12)  # 12 because now it also stores r5long to ease ignoring the expanded time (if not expanded, then it is not used)
})