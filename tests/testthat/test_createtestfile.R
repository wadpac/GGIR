library(GGIR)
test_that("createtestfile produces a file", {
  createtestfile()
  fn = "testfile.csv"
  expect_that(file.exists(fn),is_true())
  if (file.exists(fn)) file.remove(fn)
})