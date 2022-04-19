library(GGIR)
context("create_test_sleeplog_csv")
test_that("create_test_sleeplog_csv produces a file", {
  create_test_sleeplog_csv(advanced=FALSE)
  fn = "testsleeplogfile.csv"
  expect_true(file.exists(fn))
  if (file.exists(fn)) file.remove(fn)
  create_test_sleeplog_csv(advanced=TRUE)
  fn = "testsleeplogfile.csv"
  expect_true(file.exists(fn))
  if (file.exists(fn)) file.remove(fn)
})