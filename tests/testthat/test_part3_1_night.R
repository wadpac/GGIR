library(GGIR)
context("g.part3")
test_that("Part 3 identifies 1-night scenario", {
  skip_on_cran()
  #=======================
  # Create a test file for a bit over a day worth of data.
  # Nmin=2000 is currently the default, but hardcoding it here in case the default changes.
  create_test_acc_csv(Nmin=2000)
  fn = "123A_testaccfile.csv"
  dn = "output_test"
  if (file.exists(dn)) unlink(dn,recursive=TRUE)
  # g.part3 modules report errors and warnings only by printing them out to the console,
  # so these are not true errors and warnings that would get caught by the testing module.
  # Instead, grab the console output and parse out any errors we should know about.
  # 
  # Note: make sure to call g.shell.GGIR() with do.parallel = FALSE,
  # otherwise not all console output will be captured.
  out = capture.output(g.shell.GGIR(mode=c(1:3), datadir=fn, outputdir=getwd(), studyname="test", f0=1, f1=1,
                                    do.report=c(), visualreport=FALSE, viewingwindow=1,
                                    overwrite=TRUE, do.parallel = FALSE, minimumFileSizeMB=0), 
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
  load(out.file.path)

  # check that the data file had only one night. The 'night' column should contain only 1s
  for (i in 1:nrow(sib.cla.sum)) {
    expect_that(sib.cla.sum$night[i],equals(1))
  }
   
  if (file.exists(fn)) file.remove(fn)
  if (file.exists(dn)) unlink(dn, recursive = TRUE)
})
