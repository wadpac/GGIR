library(GGIR)
test_that("chainof5parts", {
  create_test_acc_csv()
  create_test_sleeplog_csv()
  fn = "123A_testaccfile.csv"
  sleeplog_fn = "testsleeplogfile.csv"
  metadatadir = paste0(getwd(),"/output_test")
  dn = "output_test"
  # part 1
  g.part1(datadir=fn,outputdir=getwd(),f0=1,f1=1,overwrite=TRUE,
                     studyname="test",do.enmo = TRUE,do.anglez=TRUE,do.cal = TRUE)
  expect_that(dir.exists(dn),is_true())
  rn = dir("output_test/meta/basic/",full.names = TRUE)
  load(rn[1])
  
  expect_that(round(C$scale,digits=5),equals(c(0.9852, 0.9852, 0.9852)))
  expect_that(nrow(M$metalong),equals(133))
  expect_that(M$metalong[2,1],equals("2016-06-23T09:15:00+0100"))
  expect_that(nrow(M$metashort),equals(23940))
  expect_that(round(mean(M$metashort$ENMO),digits=5),equals(0.01907))
  expect_that(I$monc,equals(3))
  expect_that(I$sf,equals(3))
  expect_that(I$dformc,equals(2))
  expect_that(C$npoints,equals(2606))
  
  # part 2
  g.part2(datadir=fn,metadatadir=metadatadir,f0=1,f1=1, idloc = 2,
          strategy = 1,overwrite=TRUE, hrs.del.start = 0,hrs.del.end = 0,
          maxdur = 2, includedaycrit = 1)
  g.report.part2(metadatadir=metadatadir,f0=1,f1=1,maxdur=2)
  
  dirname = "output_test/meta/ms2.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  summarycsv = "output_test/results/part2_summary.csv"
  daysummarycsv = "output_test/results/part2_daysummary.csv"
  
  expect_that(dir.exists(dirname),is_true())
  expect_that(file.exists(summarycsv),is_true())
  expect_that(file.exists(daysummarycsv),is_true())
  expect_that(nrow(IMP$metashort),equals(23940))
  expect_that(round(mean(IMP$metashort$ENMO),digits=5),equals(0.01907))
  expect_that(round(as.numeric(SUM$summary$meas_dur_dys),digits=5),equals(1.38542))
  expect_that(round(as.numeric(SUM$summary$`p10_ENMO_mg_0-24h`),digits=4),equals(2.3456))
  expect_that(round(as.numeric(SUM$summary$`WD_mean_ENMO_mg_24hr`),digits=4),equals(19.4651))
  
  # part 3
  g.part3(metadatadir=metadatadir,f0=1,f1=1,anglethreshold = 5,
                     timethreshold = 5,ignorenonwear=FALSE,overwrite=TRUE) 
  dirname = "output_test/meta/ms3.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  
  expect_that(dir.exists(dirname),is_true())
  expect_that(round(sib.cla.sum$tot.sib.dur.hrs,digits=4),equals(0.2417))
  expect_that(round(sib.cla.sum$fraction.night.invalid,digits=4),equals(0.0625))
  expect_that(sib.cla.sum$sib.end.time,equals("2016-06-23 18:09:50"))
  
  
  # part 4
  g.part4(datadir=fn,metadatadir=metadatadir,f0=1,f1=1,
          idloc=2,loglocation = sleeplog_fn, do.visual=TRUE,outliers.only = FALSE,
          excludefirstlast=FALSE,criterror = 1,includenightcrit=1,nnights=7,colid=1,coln1=2,
          relyonsleeplog=FALSE,
          storefolderstructure=FALSE, overwrite=TRUE)
  dirname = "output_test/meta/ms4.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  vis_sleep_file = "output_test/results/visualisation_sleep.pdf"
  g.report.part4(datadir=fn,metadatadir=metadatadir,loglocation = sleeplog_fn,f0=1,f1=1)
  
  expect_that(dir.exists(dirname),is_true())
  expect_that(file.exists(vis_sleep_file),is_true())
  expect_that(round(nightsummary$acc_dur_sibd,digits=4),equals(0.2403))
  expect_that(as.logical(nightsummary$acc_available),is_true())
  expect_that(as.logical(nightsummary$sleeplog_used),is_true())
  
  dn = "output_test"
  # if (file.exists(dn))  unlink(dn,recursive=TRUE)
  if (file.exists(fn)) file.remove(fn)
  if (file.exists(sleeplog_fn)) file.remove(sleeplog_fn)
  
})