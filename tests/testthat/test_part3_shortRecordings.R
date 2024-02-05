library(GGIR)
context("g.part3")
test_that("Part 3 identifies 1-night scenario, or with an incomplete night", {
  skip_on_cran()
  #=======================
  # 1) Single night ---------
  dn = "output_test"
  if (file.exists(dn)) unlink(dn, recursive = TRUE)
  create_part1_meta(Ndays = 1.5)
  fn = "123A_testaccfile.csv"

  GGIR(mode = c(2:3), datadir = fn, outputdir = getwd(), studyname = "test", f0 = 1, f1 = 1,
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
  
  # 2) Incomplete night ---------
  # Create a test file that doesn't contain a 4am but does contain a noon and a midnight
  # (4am, noon and midnight all carry significance in the internal logic of g.sib.det)
  if (file.exists(dn)) unlink(dn, recursive = TRUE)
  create_part1_meta(Ndays = 0.6)
  GGIR(mode = c(2:3), datadir = fn, outputdir = getwd(), studyname = "test", f0 = 1, f1 = 1,
       do.report = c(), visualreport = FALSE, viewingwindow = 1,
       do.parallel = FALSE, minimumFileSizeMB = 0, verbose = FALSE)
  
  out.file.path = paste0(dn, "/meta/ms3.out/", fn, ".RData")
  expect_true(file.exists(out.file.path))
  
  # Create a test file that doesn't contain a 4am or a noon, but does have a midnight
  # (4am, noon and midnight all carry significance in the internal logic of g.sib.det)
  create_part1_meta(Ndays = 0.6, start_time = "13:00:00")
  GGIR(mode = c(2:3), datadir = fn, outputdir = getwd(), studyname = "test", f0 = 1, f1 = 1,
       do.report = c(), visualreport = FALSE, viewingwindow = 1,
       do.parallel = FALSE, minimumFileSizeMB = 0, verbose = FALSE)
  
  out.file.path = paste0(dn, "/meta/ms3.out/", fn, ".RData")
  expect_true(file.exists(out.file.path))
  
  # remove generated files --------
  if (file.exists(fn)) file.remove(fn)
  if (file.exists(dn)) unlink(dn, recursive = TRUE)
})
