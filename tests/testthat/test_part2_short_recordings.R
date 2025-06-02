library(GGIR)
context("Very short recordings")
test_that("Test number of days in very short recordings", {
  skip_on_cran()
  create_test_acc_csv(Nmin = 720, start_time = "09:00:00")
  fn = "123A_testaccfile.csv"
  desiredtz = "Europe/London"
  dn = "output_test"
  if (file.exists(dn))  unlink(dn, recursive = TRUE)
  minimumFileSizeMB = 0
  #--------------------------------------------
  # run GGIR
  GGIR(datadir = fn, outputdir = getwd(), studyname = "test", 
       mode = 1:2, do.report = 2, idloc = 2,
       verbose = FALSE, desiredtz = desiredtz,
       do.cal = FALSE, do.imp = FALSE, windowsizes = c(5, 60, 60),
       ilevels = seq(0, 800, by = 100), includedaycrit = 0)
  #--------------------------------------------
  # part 2 data contains 2 complete days
  rn = "output_test/results/part2_daysummary.csv"
  out2 = read.csv(rn)
  expect_equal(nrow(out2), 1)
  expect_equal(round(out2$N.hours), 12)
  
  if (dir.exists(dn))  unlink(dn, recursive = TRUE)
  if (file.exists(fn)) unlink(fn)
})
