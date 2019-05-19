library(GGIR)
context("create_test_acc_csv")
test_that("create_test_acc_csv produces a file", {
  create_test_acc_csv()
  fn = "123A_testaccfile.csv"
  expect_true(file.exists(fn))
  if (file.exists(fn)) file.remove(fn)
})