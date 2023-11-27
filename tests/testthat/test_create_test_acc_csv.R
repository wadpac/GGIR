library(GGIR)
context("create_test_acc_csv")
test_that("create_test_acc_csv produces a file", {
  t0 = Sys.time()
  # creates file with correct filename
  create_test_acc_csv(Nmin = 10)
  fn = "123A_testaccfile.csv"
  expect_true(file.exists(fn))
  if (file.exists(fn)) file.remove(fn)

  # generate a test file with a non-wear block straddling the end of the recording 
  create_test_acc_csv(Nmin = 1500, sf = 1)
  expect_true(file.exists(fn))
  if (file.exists(fn)) file.remove(fn)

  # generate a test file with a sleep block straddling the end of the recording 
  create_test_acc_csv(Nmin = 1155, sf = 1)
  expect_true(file.exists(fn))
  if (file.exists(fn)) file.remove(fn)

  # generate a test file with an activity block straddling the end of the recording 
  create_test_acc_csv(Nmin = 2311, sf = 1)
  expect_true(file.exists(fn))
  if (file.exists(fn)) file.remove(fn)
  print(Sys.time() - t0)
})