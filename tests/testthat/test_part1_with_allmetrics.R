library(GGIR)
context("g.part1")
test_that("Part 1 can run with all metrics", {
  skip_on_cran()
  #=======================
  # Part 1 with all metrics
  Ndays = 2
  create_test_acc_csv(Nmin=Ndays*1440)
  create_test_sleeplog_csv()
  fn = "123A_testaccfile.csv"
  sleeplog_fn = "testsleeplogfile.csv"
  metadatadir = paste0(getwd(),"/output_test")
  desiredtz="Europe/London"
  dn = "output_test"
  if (file.exists(dn))  unlink(dn,recursive=TRUE)
  g.part1(datadir=fn,outputdir=getwd(),f0=1,f1=1,overwrite=TRUE,desiredtz=desiredtz,
          studyname="test",do.cal = FALSE,
          # We are not doing all the metrics, because Travis-CI cannot allocate enough memory
          do.enmo = TRUE,do.lfenmo = TRUE,
          do.bfen = TRUE,
          do.hfenplus = TRUE,
          do.mad = TRUE,
   windowsizes = c(15,3600,3600), do.parallel=FALSE,
   minimumFileSizeMB=0)
  rn = dir("output_test/meta/basic/",full.names = TRUE)
  load(rn[1])
  expect_that(ncol(M$metashort),equals(6))
  expect_that(nrow(M$metashort),equals(11280))
  expect_that(round(mean(M$metashort$BFEN),digits=4),equals(0.0302))
  expect_that(round(mean(M$metashort$LFENMO),digits=4),equals(0.0442))
  expect_that(round(mean(M$metashort$HFENplus),digits=4),equals(0.0804))
  expect_that(round(mean(M$metashort$MAD),digits=4),equals(0.0072))
   
  if (file.exists(fn)) file.remove(fn)
  if (file.exists(sleeplog_fn)) file.remove(sleeplog_fn)
})
