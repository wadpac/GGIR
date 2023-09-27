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
  # g.part3 modules report errors and warnings only by printing them out to the console,
  # so these are not true errors and warnings that would get caught by the testing module.
  # Instead, grab the console output and parse out any errors we should know about.
  # 
  # Note: make sure to call GGIR() with do.parallel = FALSE,
  # otherwise not all console output will be captured.
  out = capture.output(GGIR(mode = c(1:3), datadir = fn, outputdir = getwd(), studyname = "test", f0 = 1, f1 = 1,
                                    do.report = c(), visualreport = FALSE, viewingwindow = 1,
                                    overwrite = TRUE, do.parallel = FALSE, minimumFileSizeMB = 0), 
                       type = c("output"))
  
  # There shouldn't be any errors or warnings reported, so if there are any, throw an error.
  err_idx = grepl("Errors and warnings for", out)
  for (ii in 1:length(err_idx)) {
    if (err_idx[ii]) {
      stop(out[ii:length(err_idx)])
    }
  }

  out.file.path = paste0(dn, "/meta/ms3.out/", fn, ".RData")
  expect_true(file.exists(out.file.path))

  if (file.exists(fn)) file.remove(fn)
  if (file.exists(dn)) unlink(dn, recursive = TRUE)
})
