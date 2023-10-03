library(GGIR)
context("g.part3")
test_that("Part 3 runs fine on a short file not containing a night", {
  skip_on_cran()
  #=======================
  # Create a test file that doesn't contain a night.
  # More specifically, it should not contan a 4am, since it's a magic time used in g.sib.det
  create_test_acc_csv(Nmin=1000, starts_at_midnight = FALSE)
  fn = "123A_testaccfile.csv"
  dn = "output_test"
  if (file.exists(dn)) unlink(dn, recursive = TRUE)

  GGIR(mode = c(1:3), datadir = fn, outputdir = getwd(), studyname = "test", f0 = 1, f1 = 1,
       do.report = c(), visualreport = FALSE, viewingwindow = 1,
       overwrite = TRUE, do.parallel = FALSE, minimumFileSizeMB = 0)
  
  out.file.path = paste0(dn, "/meta/ms3.out/", fn, ".RData")
  expect_true(file.exists(out.file.path))

  if (file.exists(fn)) file.remove(fn)
  if (file.exists(dn)) unlink(dn, recursive = TRUE)
})
