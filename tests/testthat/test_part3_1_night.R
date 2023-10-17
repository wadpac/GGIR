library(GGIR)
context("g.part3")
test_that("Part 3 identifies 1-night scenario", {
  skip_on_cran()
  #=======================
  # Create a test file for a bit over a day worth of data.
  # Nmin=2000 is currently the default, but hardcoding it here in case the default changes.
  create_test_acc_csv(Nmin = 2000)
  fn = "123A_testaccfile.csv"
  dn = "output_test"
  if (file.exists(dn)) unlink(dn, recursive = TRUE)

  GGIR(mode = c(1:3), datadir = fn, outputdir = getwd(), studyname = "test", f0 = 1, f1 = 1,
       do.report = c(), visualreport = FALSE, viewingwindow = 1,
       overwrite = TRUE, do.parallel = FALSE, minimumFileSizeMB = 0,
       verbose = FALSE)
  
  out.file.path = paste0(dn, "/meta/ms3.out/", fn, ".RData")
  expect_true(file.exists(out.file.path))
  load(out.file.path)

  # check that the data file had only one night. The 'night' column should contain only 1s
  for (i in 1:nrow(sib.cla.sum)) {
    expect_that(sib.cla.sum$night[i],equals(1))
  }
   
  if (file.exists(fn)) file.remove(fn)
  if (file.exists(dn)) unlink(dn, recursive = TRUE)
})
