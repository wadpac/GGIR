library(GGIR)
context("Chainof5parts")
test_that("chainof5parts", {
  skip_on_cran()
  Ndays = 2
  create_test_acc_csv(Nmin=Ndays*1440)
  create_test_sleeplog_csv()
  fn = "123A_testaccfile.csv"
  sleeplog_fn = "testsleeplogfile.csv"
  metadatadir = paste0(getwd(),"/output_test")
  desiredtz="Europe/London"
  dn = "output_test"
  do.parallel = FALSE
  if (file.exists(dn))  unlink(dn,recursive=TRUE)
  minimumFileSizeMB = 0
  #--------------------------------------------
  # part 1
  g.part1(datadir=fn,outputdir=getwd(),f0=1,f1=1,overwrite=TRUE,desiredtz=desiredtz,
                     studyname="test",do.enmo = TRUE,do.anglez=TRUE,do.cal = TRUE,
          windowsizes = c(15,3600,3600), do.parallel = do.parallel,
          minimumFileSizeMB=minimumFileSizeMB)
  expect_true(dir.exists(dn))
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
  #-------------------------
  # part 2 with strategy = 3
  g.part2(datadir=fn,metadatadir=metadatadir,f0=1,f1=1, idloc = 2,desiredtz=desiredtz,
          strategy = 3,overwrite=TRUE, hrs.del.start = 0,hrs.del.end = 0,
          maxdur = Ndays, includedaycrit = 0, do.parallel = do.parallel,myfun=c())
  dirname = "output_test/meta/ms2.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_that(nrow(IMP$metashort),equals(11280))
  expect_that(round(mean(IMP$metashort$ENMO),digits=5),equals(0.00796))
  expect_that(round(as.numeric(SUM$summary$meas_dur_def_proto_day),digits=3),equals(0.417))
  # part 2 with strategy = 2 and iglevels = TRUE
  g.part2(datadir=fn,metadatadir=metadatadir,f0=1,f1=1, idloc = 2,desiredtz=desiredtz,
          strategy = 2,overwrite=TRUE, hrs.del.start = 0,hrs.del.end = 0,
          maxdur = Ndays, includedaycrit = 0, do.imp = FALSE, epochvalues2csv = TRUE, iglevels= TRUE,
          do.parallel = do.parallel,myfun=c())
  dirname = "output_test/meta/ms2.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_that(nrow(IMP$metashort),equals(11280))
  expect_that(round(mean(IMP$metashort$ENMO),digits=3),equals(0.029))
  expect_that(round(as.numeric(SUM$summary$meas_dur_def_proto_day),digits=2),equals(1))
  # part 2 with strategy = 1
  g.part2(datadir=fn,metadatadir=metadatadir,f0=1,f1=1, idloc = 2,desiredtz=desiredtz,
          strategy = 1,overwrite=TRUE, hrs.del.start = 0,hrs.del.end = 0,
          maxdur = Ndays, includedaycrit = 0,qM5L5=c(0.2,0.4),winhr=c(3,10),
          do.parallel = do.parallel,myfun=c())
  g.report.part2(metadatadir=metadatadir,f0=1,f1=1,maxdur=Ndays)
  dirname = "output_test/meta/ms2.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  summarycsv = "output_test/results/part2_summary.csv"
  daysummarycsv = "output_test/results/part2_daysummary.csv"
  expect_true(dir.exists(dirname))
  expect_true(file.exists(summarycsv))
  expect_true(file.exists(daysummarycsv))
  expect_that(nrow(IMP$metashort),equals(11280))
  expect_that(round(mean(IMP$metashort$ENMO),digits=5),equals(0.02898))
  expect_that(round(as.numeric(SUM$summary$meas_dur_dys),digits=5),equals(1.95833))
  expect_that(ncol(SUM$daysummary), equals(34))
  expect_that(round(mean(as.numeric(SUM$daysummary$`M3_ENMO_mg_0-24hr`)), digits = 3),equals(88.992))
  expect_that(round(mean(as.numeric(SUM$daysummary$`M3_q40_ENMO_mg_0-24hr`)), digits = 3),equals(37.134))

  #expect_that(round(as.numeric(SUM$summary$`M5_ENMO_mg_0-24h`), digits = 4),equals(80.6532))
  #expect_that(round(as.numeric(SUM$summary$WD_mean_ENMO_mg_24hr), digits = 4),equals(30.1371))

  #--------------------------------------------
  # part 3
  g.part3(metadatadir=metadatadir,f0=1,f1=1,anglethreshold = 5,desiredtz=desiredtz,
                     timethreshold = 5,ignorenonwear=FALSE,overwrite=TRUE,do.part3.pdf=TRUE,
          do.parallel = do.parallel)
  dirname = "output_test/meta/ms3.out/"
  rn3 = dir(dirname,full.names = TRUE)
  load(rn3[1])

  expect_true(dir.exists(dirname))
  expect_that(round(sum(sib.cla.sum[,4:7]),digits=0),equals(2952))

  #--------------------------------------------
  # part 4
  expect_warning(g.part4(datadir=fn,metadatadir=metadatadir,f0=1,f1=1,
          idloc=2,loglocation = sleeplog_fn, do.visual=TRUE,outliers.only = FALSE,
          excludefirstlast=FALSE,criterror = 1,includenightcrit=0,nnights=7,colid=1,coln1=2,
          relyonguider=FALSE,desiredtz=desiredtz,
          storefolderstructure=FALSE, overwrite=TRUE))
  dirname = "output_test/meta/ms4.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  vis_sleep_file = "output_test/results/visualisation_sleep.pdf"
  g.report.part4(datadir=fn,metadatadir=metadatadir,loglocation = sleeplog_fn,f0=1,f1=1)
  expect_true(dir.exists(dirname))
  expect_true(file.exists(vis_sleep_file))
  expect_that(round(nightsummary$number_sib_wakinghours[1],digits=4),equals(6))
  expect_true(as.logical(nightsummary$acc_available[1]))
  expect_true(as.logical(nightsummary$sleeplog_used[1]))

  #--------------------------------------------
  #part 5
  g.part5(datadir=fn,metadatadir=metadatadir,f0=1,f1=1,desiredtz=desiredtz,
          strategy=1,maxdur=Ndays,hrs.del.start=0,hrs.del.end =0,
                     loglocation= sleeplog_fn,
                     overwrite=TRUE, excludefirstlast=FALSE, do.parallel = do.parallel,
          frag.classes.day = c("day_IN_bts", "day_IN_unbt"),  frag.classes.spt = "spt_sleep",
          frag.metrics="all")
  dirname = "output_test/meta/ms5.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_true(dir.exists(dirname))
  expect_true(file.exists(rn[1]))
  expect_that(nrow(output),equals(3)) # changed because part5 now gives also first and last day
  expect_that(ncol(output),equals(139))#143
  expect_that(round(as.numeric(output$wakeup[2]),digits=4),equals(35.9958))

 #--------------------------------------------
  #g.shell.GGIR
  suppressWarnings(g.shell.GGIR(mode=c(2,3,4,5),datadir=fn,outputdir=getwd(),studyname="test",f0=1,f1=1,
                          do.report=c(2,4,5),overwrite=FALSE,visualreport=FALSE,viewingwindow=1,
                          do.parallel = do.parallel, minimumFileSizeMB=minimumFileSizeMB))
  suppressWarnings(g.shell.GGIR(mode=c(),datadir=fn,outputdir=getwd(),studyname="test",f0=1,f1=1,
                                do.report=c(),overwrite=FALSE,visualreport=TRUE,viewingwindow=1,
                                do.parallel = do.parallel, minimumFileSizeMB=minimumFileSizeMB))
  expect_true(file.exists("output_test/results/part2_daysummary.csv"))
  expect_true(file.exists("output_test/results/part2_summary.csv"))
  expect_true(file.exists("output_test/results/part4_nightsummary_sleep_cleaned.csv"))
  expect_true(file.exists("output_test/results/part4_summary_sleep_cleaned.csv"))
  expect_true(file.exists("output_test/results/file summary reports/Report_123A_testaccfile.csv.pdf"))
  dn = "output_test"
  #--------------------------------------------
  # create dummy selectdaysfile
  selectdays = data.frame(Monitor = "MOS2D12345678",Day1="24/06/2016",Day2="25/06/2016", stringsAsFactors = TRUE)
  selectdaysfile = paste0(getwd(),"/selectdaysfile.csv")
  write.csv(selectdays, file=selectdaysfile,row.names = FALSE,fileEncoding="UTF-8")
  # we will now use it in g.part2, not sure whether g.part2 will actually be able to handle this.
  # normally, g.part1 would use the file to cut up the measurement, but that only works for GENEActiv
  # data and we do not have a multi-data GENEActiv test file in the package.
  g.part2(datadir=fn,metadatadir=metadatadir,f0=1,f1=1, idloc = 2,desiredtz=desiredtz,
          strategy = 1,overwrite=TRUE, hrs.del.start = 0,hrs.del.end = 0,
          maxdur = Ndays, includedaycrit = 0, selectdaysfile=selectdaysfile, storefolderstructure=TRUE,
          do.parallel = do.parallel,myfun=c())
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_that(nrow(IMP$metashort),equals(11280))
  expect_that(round(mean(IMP$metashort$ENMO),digits=5),equals(0.02898))

  #=======================
  # Different variations on part 4:
  #--------------------------------------------
  # part 4 without sleeplog
  expect_warning(g.part4(datadir=fn,metadatadir=metadatadir,f0=1,f1=1,
                         idloc=2,loglocation = c(), do.visual=TRUE,outliers.only = FALSE,
                         excludefirstlast=FALSE,criterror = 1,includenightcrit=0,nnights=7,colid=1,coln1=2,
                         relyonguider=FALSE,desiredtz=desiredtz,
                         storefolderstructure=TRUE, overwrite=TRUE))
  dirname = "output_test/meta/ms4.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_true(file.exists(vis_sleep_file))
  expect_that(round(nightsummary$SptDuration[1],digits=4),equals(10.2417))
  expect_true(as.logical(nightsummary$acc_available[1]))
  expect_false(as.logical(nightsummary$sleeplog_used[1]))

  #----------------------------------------------------------------------
  # Part 4 - daysleeper scenario by modifying the part 3 output & criterror = 0, and relyonguider=TRUE
  sptwindow_HDCZA_end = c(37,37) # turn into midday
  row2copy = which(sib.cla.sum$sib.onset.time == "2016-06-24T05:10:15+0100")
  newrow = sib.cla.sum[row2copy,]
  newrow$sib.onset.time = "2016-06-24T12:30:15+0100"
  newrow$sib.end.time = "2016-06-24T12:45:15+0100"
  sib.cla.sum = rbind(sib.cla.sum,newrow)
  sib.cla.sum = sib.cla.sum[order(sib.cla.sum$sib.onset.time),]
  sib.cla.sum$sib.period[1:11] = 1:11
  save(L5list, sptwindow_HDCZA_end, sptwindow_HDCZA_start, sib.cla.sum, tib.threshold, file= rn3[1])
  expect_warning(g.part4(datadir=fn,metadatadir=metadatadir,f0=1,f1=1,
                         idloc=2,loglocation = c(), do.visual=TRUE,outliers.only = FALSE,
                         excludefirstlast=FALSE,criterror = 0,includenightcrit=0,nnights=7,colid=1,coln1=2,
                         relyonguider=TRUE,desiredtz=desiredtz,
                         storefolderstructure=TRUE, overwrite=TRUE))
  dirname = "output_test/meta/ms4.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  vis_sleep_file = "output_test/results/visualisation_sleep.pdf"

  expect_true(dir.exists(dirname))
  expect_true(file.exists(vis_sleep_file))
  expect_that(round(nightsummary$number_sib_wakinghours[1],digits=4),equals(0))
  expect_that(round(nightsummary$SptDuration[1],digits=2),equals(19))
  expect_true(as.logical(nightsummary$acc_available[1]))
  expect_false(as.logical(nightsummary$sleeplog_used[1]))
  #----------------------------------------------------------------------
  # Part 4 - DST+1
  load(rn3[1])
  changetime = function(x) {
    x = gsub("-06-", "-03-", x)
    x = gsub("-23T", "-26T", x)
    x = gsub("-24T", "-27T", x)
    x = gsub("-25T", "-28T", x)
    return(x)
  }
  sib.cla.sum$start.time.day = changetime(sib.cla.sum$start.time.day)
  sib.cla.sum$sib.onset.time  = changetime(sib.cla.sum$sib.onset.time)
  sib.cla.sum$sib.end.time = changetime(sib.cla.sum$sib.end.time)
  save(L5list, sptwindow_HDCZA_end, sptwindow_HDCZA_start, sib.cla.sum, tib.threshold, file= rn3[1])
  expect_warning(g.part4(datadir=fn,metadatadir=metadatadir,f0=1,f1=1,
                         idloc=2,loglocation = sleeplog_fn, do.visual=TRUE,outliers.only = FALSE,
                         excludefirstlast=FALSE,criterror = 0,includenightcrit=0,nnights=7,colid=1,coln1=2,
                         relyonguider=TRUE,desiredtz=desiredtz,
                         storefolderstructure=TRUE, overwrite=TRUE))
  dirname = "output_test/meta/ms4.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_true(dir.exists(dirname))
  expect_that(round(nightsummary$number_sib_wakinghours[1],digits=4),equals(10))
  expect_that(round(nightsummary$SptDuration[1],digits=4),equals(7))

  #----------------------------------------------------------------------
  # Part 4 - DST-1
  load(rn3[1])
  changetime = function(x) {
    x = gsub("-03-", "-10-", x)
    x = gsub("-26T", "-29T", x)
    x = gsub("-27T", "-30T", x)
    x = gsub("-28T", "-31T", x)
    return(x)
  }
  sib.cla.sum$start.time.day = changetime(sib.cla.sum$start.time.day)
  sib.cla.sum$sib.onset.time  = changetime(sib.cla.sum$sib.onset.time)
  sib.cla.sum$sib.end.time = changetime(sib.cla.sum$sib.end.time)
  save(L5list, sptwindow_HDCZA_end, sptwindow_HDCZA_start, sib.cla.sum, tib.threshold, file= rn3[1])
  expect_warning(g.part4(datadir=fn,metadatadir=metadatadir,f0=1,f1=1,
                         idloc=2,loglocation = sleeplog_fn, do.visual=TRUE,outliers.only = FALSE,
                         excludefirstlast=FALSE,criterror = 0,includenightcrit=0,nnights=7,colid=1,coln1=2,
                         relyonguider=FALSE,desiredtz=desiredtz,
                         storefolderstructure=TRUE, overwrite=TRUE))
  dirname = "output_test/meta/ms4.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_true(dir.exists(dirname))
  expect_that(round(nightsummary$number_sib_wakinghours[1],digits=4),equals(6))
  expect_that(round(nightsummary$SptDuration[1],digits=4),equals(13.075))

  if (file.exists(selectdaysfile)) file.remove(selectdaysfile)
  if (file.exists(dn))  unlink(dn,recursive=TRUE)
  if (file.exists(fn)) file.remove(fn)
  if (file.exists(sleeplog_fn)) file.remove(sleeplog_fn)
})
