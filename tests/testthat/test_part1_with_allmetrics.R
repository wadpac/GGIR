library(GGIR)
context("Chainof5parts")
test_that("chainof5parts", {
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
          studyname="test",do.cal = FALSE, do.enmo = TRUE,do.lfenmo = TRUE,do.en = TRUE,
          do.bfen = TRUE,do.hfen=TRUE,do.hfenplus = TRUE, do.mad = TRUE,
          do.anglex=TRUE,do.angley=TRUE,do.anglez=TRUE,
          do.enmoa = TRUE,
          do.roll_med_acc_x=TRUE,do.roll_med_acc_y=TRUE,do.roll_med_acc_z=TRUE,
          do.dev_roll_med_acc_x=TRUE,do.dev_roll_med_acc_y=TRUE,do.dev_roll_med_acc_z=TRUE,
          do.lfen = TRUE,
   windowsizes = c(15,3600,3600))
  rn = dir("output_test/meta/basic/",full.names = TRUE)
  load(rn[1])
  expect_that(ncol(M$metashort),equals(19))
  expect_that(nrow(M$metashort),equals(11280))
  expect_that(round(mean(M$metashort$BFEN),digits=4),equals(0.0302))
  expect_that(round(mean(M$metashort$LFENMO),digits=4),equals(0.0442))
  expect_that(round(mean(M$metashort$HFENplus),digits=4),equals(0.0791))
  expect_that(round(mean(M$metashort$dev_roll_med_acc_x),digits=4),equals(0.0067))
  expect_that(round(mean(M$metashort$MAD),digits=4),equals(0.0072))
   
  if (file.exists(fn)) file.remove(fn)
  if (file.exists(sleeplog_fn)) file.remove(sleeplog_fn)
})
