library(GGIR)
context("create_test_acc_csv")
options(encoding = "UTF-8")
test_that("create_test_acc_csv produces a file", {
  create_test_acc_csv()
  fn = "123A_testaccfile.csv"
  expect_that(file.exists(fn),is_true())
  if (file.exists(fn)) file.remove(fn)
})