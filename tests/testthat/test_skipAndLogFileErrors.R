library(GGIR)
context("TryCatch errors in sequential mode")
test_that("Test that potential errors found in sequential mode are skipped and logged correctly", {
  skip_on_cran()
  create_test_acc_csv(Nmin = 2*1440)
  corrupt_files = dir(system.file("testfiles", package = "GGIR"), full.names = T)
  fn = c(corrupt_files, "123A_testaccfile.csv")
  desiredtz = "Europe/London"
  dn = "output_test"
  if (file.exists(dn))  unlink(dn, recursive = TRUE)
  minimumFileSizeMB = 0
  #--------------------------------------------
  # run GGIR (this should not trigger error, but log it and inform in console)
  console_log = capture.output(
    GGIR(datadir = fn, outputdir = getwd(), studyname = "test", 
         do.cal = FALSE, do.imp = FALSE,
         do.parallel = FALSE,
         minimumFileSizeMB = minimumFileSizeMB,
         use_trycatch_serial = TRUE)
  )
  #--------------------------------------------
  # Were errors skipped and logged?
  #   - did part 5 run?
  expect_true(any(grepl("^ Part 5", console_log)))
  #   - were errors logged in part 1?
  expect_true(any(grepl("Errors in part 1... for:", console_log)))
  
  if (dir.exists(dn))  unlink(dn, recursive = TRUE)
  if (file.exists("123A_testaccfile.csv")) unlink("123A_testaccfile.csv")
})
