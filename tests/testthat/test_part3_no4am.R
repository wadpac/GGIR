library(GGIR)
context("g.part3")
test_that("Part 3 runs fine on a short file not containing a full night", {
  skip_on_cran()
  #=======================
  fn = "123A_testaccfile.csv"
  dn = "output_test"
  if (file.exists(dn)) unlink(dn, recursive = TRUE)

  # Create a test file that doesn't contain a 4am but does contain a noon and a midnight
  # (4am, noon and midnight all carry significance in the internal logic of g.sib.det)
  create_test_acc_csv(Nmin=1000, starts_at_midnight = FALSE)
  GGIR(mode = c(1:3), datadir = fn, outputdir = getwd(), studyname = "test", f0 = 1, f1 = 1,
       do.report = c(), visualreport = FALSE, viewingwindow = 1,
       do.parallel = FALSE, minimumFileSizeMB = 0)
  
  out.file.path = paste0(dn, "/meta/ms3.out/", fn, ".RData")
  expect_true(file.exists(out.file.path))

  if (file.exists(dn)) unlink(dn, recursive = TRUE)

  # Create a test file that doesn't contain a 4am or a noon, but does have a midnight
  # (4am, noon and midnight all carry significance in the internal logic of g.sib.det)
  create_test_acc_csv(Nmin=720, start_time = "13:00:00")
  GGIR(mode = c(1:3), datadir = fn, outputdir = getwd(), studyname = "test", f0 = 1, f1 = 1,
       do.report = c(), visualreport = FALSE, viewingwindow = 1,
       do.parallel = FALSE, minimumFileSizeMB = 0)
  
  out.file.path = paste0(dn, "/meta/ms3.out/", fn, ".RData")
  expect_true(file.exists(out.file.path))

  if (file.exists(fn)) file.remove(fn)
  if (file.exists(dn)) unlink(dn, recursive = TRUE)
})
