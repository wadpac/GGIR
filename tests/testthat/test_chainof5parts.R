library(GGIR)
context("Chainof5parts")
test_that("chainof5parts", {
  Ndays = 2
  create_test_acc_csv(Nmin=Ndays*1440)
  create_test_sleeplog_csv()
  fn = "123A_testaccfile.csv"
  sleeplog_fn = "testsleeplogfile.csv"
  metadatadir = paste0(getwd(),"/output_test")
  desiredtz="Europe/London"
  dn = "output_test"
  if (file.exists(dn))  unlink(dn,recursive=TRUE)
  #--------------------------------------------
  # part 1
  g.part1(datadir=fn,outputdir=getwd(),f0=1,f1=1,overwrite=TRUE,desiredtz=desiredtz,
                     studyname="test",do.enmo = TRUE,do.anglez=TRUE,do.cal = TRUE,
          windowsizes = c(15,3600,3600))
  expect_that(dir.exists(dn),is_true())
  rn = dir("output_test/meta/basic/",full.names = TRUE)
  load(rn[1])
  expect_that(round(C$scale,digits=5),equals(c(0.98476, 0.98399, 0.98442)))
  expect_that(nrow(M$metalong),equals(47))
  # expect_that(M$metalong[2,1],equals("2016-06-23T09:15:00+0100")) # turned off because not consistent across machines, to investigate
  expect_that(nrow(M$metashort),equals(11280))
  expect_that(round(mean(M$metashort$ENMO),digits=5),equals(0.02898))
  expect_that(I$monc,equals(3))
  expect_that(I$sf,equals(3))
  expect_that(I$dformc,equals(2))
  expect_that(C$npoints,equals(9728))
  #--------------------------------------------
  # part 2 with strategy = 3
  g.part2(datadir=fn,metadatadir=metadatadir,f0=1,f1=1, idloc = 2,desiredtz=desiredtz,
          strategy = 3,overwrite=TRUE, hrs.del.start = 0,hrs.del.end = 0,
          maxdur = Ndays, includedaycrit = 0)
  dirname = "output_test/meta/ms2.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_that(nrow(IMP$metashort),equals(11280))
  expect_that(round(mean(IMP$metashort$ENMO),digits=5),equals(0.00796))
  expect_that(round(as.numeric(SUM$summary$meas_dur_def_proto_day),digits=3),equals(0.417))
  # part 2 with strategy = 2
  g.part2(datadir=fn,metadatadir=metadatadir,f0=1,f1=1, idloc = 2,desiredtz=desiredtz,
          strategy = 2,overwrite=TRUE, hrs.del.start = 0,hrs.del.end = 0,
          maxdur = Ndays, includedaycrit = 0, do.imp = FALSE, epochvalues2csv = TRUE)
  dirname = "output_test/meta/ms2.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_that(nrow(IMP$metashort),equals(11280))
  expect_that(round(mean(IMP$metashort$ENMO),digits=3),equals(0.029))
  expect_that(round(as.numeric(SUM$summary$meas_dur_def_proto_day),digits=2),equals(1))
  # part 2 with strategy = 1
  g.part2(datadir=fn,metadatadir=metadatadir,f0=1,f1=1, idloc = 2,desiredtz=desiredtz,
          strategy = 1,overwrite=TRUE, hrs.del.start = 0,hrs.del.end = 0,
          maxdur = Ndays, includedaycrit = 0)
  g.report.part2(metadatadir=metadatadir,f0=1,f1=1,maxdur=Ndays)
  dirname = "output_test/meta/ms2.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  summarycsv = "output_test/results/part2_summary.csv"
  daysummarycsv = "output_test/results/part2_daysummary.csv"
  expect_that(dir.exists(dirname),is_true())
  expect_that(file.exists(summarycsv),is_true())
  expect_that(file.exists(daysummarycsv),is_true())
  expect_that(nrow(IMP$metashort),equals(11280))
  expect_that(round(mean(IMP$metashort$ENMO),digits=5),equals(0.02898))
  expect_that(round(as.numeric(SUM$summary$meas_dur_dys),digits=5),equals(1.95833))
  #expect_that(round(as.numeric(SUM$summary$`M5_ENMO_mg_0-24h`), digits = 4),equals(80.6532))
  #expect_that(round(as.numeric(SUM$summary$WD_mean_ENMO_mg_24hr), digits = 4),equals(30.1371))
  #--------------------------------------------
  # part 3
  g.part3(metadatadir=metadatadir,f0=1,f1=1,anglethreshold = 5,desiredtz=desiredtz,
                     timethreshold = 5,ignorenonwear=FALSE,overwrite=TRUE,do.part3.pdf=TRUE)
  dirname = "output_test/meta/ms3.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])

  expect_that(dir.exists(dirname),is_true())
  expect_that(round(sum(sib.cla.sum[,4:7]),digits=0),equals(2952))

  #--------------------------------------------
  # part 4
  g.part4(datadir=fn,metadatadir=metadatadir,f0=1,f1=1,
          idloc=2,loglocation = sleeplog_fn, do.visual=TRUE,outliers.only = FALSE,
          excludefirstlast=FALSE,criterror = 1,includenightcrit=0,nnights=7,colid=1,coln1=2,
          relyonsleeplog=FALSE,desiredtz=desiredtz,
          storefolderstructure=FALSE, overwrite=TRUE)
  dirname = "output_test/meta/ms4.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  vis_sleep_file = "output_test/results/visualisation_sleep.pdf"
  g.report.part4(datadir=fn,metadatadir=metadatadir,loglocation = sleeplog_fn,f0=1,f1=1)
  expect_that(dir.exists(dirname),is_true())
  expect_that(file.exists(vis_sleep_file),is_true())
  expect_that(round(nightsummary$acc_dur_sibd[1],digits=4),equals(4.95))
  expect_that(as.logical(nightsummary$acc_available[1]),is_true())
  expect_that(as.logical(nightsummary$sleeplog_used[1]),is_true())
  #--------------------------------------------
  #part 5
  g.part5(datadir=fn,metadatadir=metadatadir,f0=1,f1=1,desiredtz=desiredtz,
          strategy=1,maxdur=Ndays,hrs.del.start=0,hrs.del.end =0,
                     loglocation= sleeplog_fn,
                     overwrite=TRUE, excludefirstlast=FALSE)
  dirname = "output_test/meta/ms5.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_that(dir.exists(dirname),is_true())
  expect_that(file.exists(rn[1]),is_true())
  expect_that(nrow(output),equals(6)) # changed because part5 now gives also first and last day
  expect_that(ncol(output),equals(134))
  expect_that(round(as.numeric(output$acc_wake[2]),digits=4),equals(35.9958))

 #--------------------------------------------
  #g.shell.GGIR
  suppressWarnings(g.shell.GGIR(mode=c(2,3,4,5),datadir=fn,outputdir=getwd(),studyname="test",f0=1,f1=1,
                          do.report=c(2,4,5),overwrite=FALSE,visualreport=FALSE,viewingwindow=1))
  suppressWarnings(g.shell.GGIR(mode=c(),datadir=fn,outputdir=getwd(),studyname="test",f0=1,f1=1,
                                do.report=c(),overwrite=FALSE,visualreport=TRUE,viewingwindow=1))
  expect_that(file.exists("output_test/results/part2_daysummary.csv"),is_true())
  expect_that(file.exists("output_test/results/part2_summary.csv"),is_true())
  expect_that(file.exists("output_test/results/part4_nightsummary_sleep_cleaned.csv"),is_true())
  expect_that(file.exists("output_test/results/part4_summary_sleep_cleaned.csv"),is_true())
  expect_that(file.exists("output_test/results/file summary reports/Report_123A_testaccfile.csv.pdf"),is_true())
  dn = "output_test"
  if (file.exists(dn))  unlink(dn,recursive=TRUE)
  if (file.exists(fn)) file.remove(fn)
  if (file.exists(sleeplog_fn)) file.remove(sleeplog_fn)
})
