library(GGIR)
context("create_test_acc_csv")
test_that("create_test_acc_csv produces a file", {
  create_test_acc_csv()
  fn = "123A_testaccfile.csv"
  expect_true(file.exists(fn))
  if (file.exists(fn)) file.remove(fn)

  # generate a test file with a non-wear block straddling the end of the recording 
  create_test_acc_csv(Nmin = 3000)
  expect_true(file.exists(fn))
  if (file.exists(fn)) file.remove(fn)

  # generate a test file with a sleep block straddling the end of the recording 
  create_test_acc_csv(Nmin = 2309)
  expect_true(file.exists(fn))
  if (file.exists(fn)) file.remove(fn)

  # generate a test file with an activity block straddling the end of the recording 
  create_test_acc_csv(Nmin = 2311)
  expect_true(file.exists(fn))
  if (file.exists(fn)) file.remove(fn)
})