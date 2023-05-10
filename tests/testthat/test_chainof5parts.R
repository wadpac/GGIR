library(GGIR)
context("Chainof5parts")
test_that("chainof5parts", {
  skip_on_cran()
  
  Ndays = 2
  create_test_acc_csv(Nmin = Ndays*1440)
  create_test_sleeplog_csv(advanced = FALSE)
  fn = "123A_testaccfile.csv"
  sleeplog_fn = "testsleeplogfile.csv"
  metadatadir = paste0(getwd(), "/output_test")
  desiredtz = "Europe/London"
  dn = "output_test"
  do.parallel = FALSE
  if (file.exists(dn))  unlink(dn, recursive = TRUE)
  minimumFileSizeMB = 0
  #--------------------------------------------
  # part 1
  g.part1(datadir = fn, outputdir = getwd(), f0 = 1, f1 = 1,
          overwrite = TRUE, desiredtz = desiredtz,
          studyname = "test", do.enmo = TRUE, do.anglez = TRUE ,do.cal = TRUE,
          windowsizes = c(15,3600,3600), do.parallel = do.parallel,
          minimumFileSizeMB = minimumFileSizeMB, verbose = FALSE)
  expect_true(dir.exists(dn))
  rn = dir("output_test/meta/basic/",full.names = TRUE)
  load(rn[1])
  expect_equal(round(C$scale, digits = 5), c(0.98473, 0.98425, 0.98462), tolerance = 3)
  expect_that(nrow(M$metalong), equals(47))
  # expect_that(M$metalong[2,1],equals("2016-06-23T09:15:00+0100")) # turned off because not consistent across machines, to investigate
  expect_that(nrow(M$metashort),equals(11280))
  expect_equal(round(mean(M$metashort$ENMO), digits = 5), 0.02911, tolerance = 3)
  expect_that(I$monc, equals(3))
  expect_that(I$sf, equals(3))
  expect_that(I$dformc, equals(2))
  expect_equal(C$npoints, 7494, tolerance = 5)
  #-------------------------
  # part 2 with strategy = 3
  g.part2(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
          idloc = 2, desiredtz = desiredtz, ndayswindow = 1,
          strategy = 3, overwrite = TRUE, hrs.del.start = 0, hrs.del.end = 0,
          maxdur = Ndays, includedaycrit = 0, do.parallel = do.parallel, myfun = c())
  dirname = "output_test/meta/ms2.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_equal(nrow(IMP$metashort), 11280)
  expect_equal(round(mean(IMP$metashort$ENMO), digits = 5), 0.00802, tolerance = 3)
  expect_equal(round(as.numeric(SUM$summary$meas_dur_def_proto_day), digits = 3), 1)
  expect_equal(SUM$summary$`N valid WEdays`, "1")
  expect_equal(SUM$summary$`N valid WKdays`, "2")
  
  # part 2 with strategy = 5
  g.part2(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
          idloc = 2, desiredtz = desiredtz, ndayswindow = 1,
          strategy = 5, overwrite = TRUE, hrs.del.start = 0, hrs.del.end = 0,
          maxdur = Ndays, includedaycrit = 0, do.parallel = do.parallel, myfun = c())
  dirname = "output_test/meta/ms2.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_equal(nrow(IMP$metashort), 11280)
  expect_equal(round(mean(IMP$metashort$ENMO), digits = 5), 0.03398, tolerance = 3)
  expect_equal(round(as.numeric(SUM$summary$meas_dur_def_proto_day), digits = 3), 1)
  expect_equal(SUM$summary$`N valid WEdays`, "1")
  expect_equal(SUM$summary$`N valid WKdays`, "2")
  
  # part 2 with strategy = 2 and iglevels = 1
  g.part2(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
          idloc = 2, desiredtz = desiredtz,
          strategy = 2,overwrite = TRUE, hrs.del.start = 0,hrs.del.end = 0,
          maxdur = Ndays, includedaycrit = 0, do.imp = FALSE, epochvalues2csv = TRUE, iglevels = TRUE,
          do.parallel = do.parallel, myfun = c(), winhr = 16, MX.ig.min.dur = 14, verbose = FALSE)
  dirname = "output_test/meta/ms2.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_that(nrow(IMP$metashort),equals(11280))
  expect_equal(mean(IMP$metashort$ENMO), 0.029, tolerance = 3)
  expect_equal(as.numeric(SUM$summary$meas_dur_def_proto_day), 1, tolerance = 2)
  expect_equal(as.numeric(SUM$daysummary$`L16_ig_gradient_ENMO_mg_0-24hr`[1]), -1.12, tolerance = 2)
  # part 2 with strategy = 1
  g.part2(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
          idloc = 2, desiredtz = desiredtz,
          strategy = 1,overwrite = TRUE, hrs.del.start = 0,hrs.del.end = 0,
          maxdur = Ndays, includedaycrit = 0, qM5L5 = c(0.2,0.4), winhr = c(3,10),
          do.parallel = do.parallel, myfun = c(), qlevels = c(0.5, 0.9),
          cosinor = TRUE, verbose = FALSE)
  g.report.part2(metadatadir = metadatadir, f0 = 1, f1 = 1, maxdur = Ndays)
  dirname = "output_test/meta/ms2.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  summarycsv = "output_test/results/part2_summary.csv"
  daysummarycsv = "output_test/results/part2_daysummary.csv"
  expect_true(dir.exists(dirname))
  expect_true(file.exists(summarycsv))
  expect_true(file.exists(daysummarycsv))
  expect_that(nrow(IMP$metashort), equals(11280))
  expect_equal(round(mean(IMP$metashort$ENMO), digits = 5), 0.02911, tolerance = 3)
  expect_that(round(as.numeric(SUM$summary$meas_dur_dys), digits = 5), equals(1.95833))
  expect_that(ncol(SUM$daysummary), equals(34))
  expect_equal(SUM$daysummary$`p50_ENMO_mg_0-24hr`, c("17.15", "33",  "0"))
  expect_equal(round(as.numeric(SUM$daysummary$`p90_ENMO_mg_0-24hr`)), c(44, 54, 41), tolerance = 0)
  expect_equal(mean(as.numeric(SUM$daysummary$`M3_ENMO_mg_0-24hr`)), 89.26, tolerance = 3)
  expect_equal(mean(as.numeric(SUM$daysummary$`M3_q40_ENMO_mg_0-24hr`)), 37.383, tolerance = 3)
  expect_equal(as.numeric(SUM$summary$cosinor_acrophase), 3.260855, tolerance = 3)
  expect_equal(as.numeric(SUM$summary$cosinorExt_beta), 109.618, tolerance = 2)
  #--------------------------------------------
  # part 3
  g.part3(metadatadir = metadatadir, f0 = 1, f1 = 1, anglethreshold = 5, desiredtz = desiredtz,
          timethreshold = 5,ignorenonwear = FALSE, overwrite = TRUE, do.part3.pdf = TRUE,
          do.parallel = do.parallel)
  dirname = "output_test/meta/ms3.out/"
  rn3 = dir(dirname,full.names = TRUE)
  load(rn3[1])
  
  expect_true(dir.exists(dirname))
  expect_that(round(sum(sib.cla.sum[,4:7]), digits = 0), equals(2957))
  
  #--------------------------------------------
  # part 4
  g.part4(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
          idloc = 2, loglocation = sleeplog_fn, do.visual = TRUE, outliers.only = FALSE,
          excludefirstlast = FALSE, criterror = 1, includenightcrit = 0, nnights = 7,
          colid = 1, coln1 = 2, relyonguider = FALSE, desiredtz = desiredtz,
          storefolderstructure = FALSE, overwrite = TRUE)
  dirname = "output_test/meta/ms4.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  vis_sleep_file = "output_test/results/visualisation_sleep.pdf"
  g.report.part4(datadir = fn, metadatadir = metadatadir, loglocation = sleeplog_fn,
                 f0 = 1, f1 = 1, verbose = FALSE)
  expect_true(dir.exists(dirname))
  expect_true(file.exists(vis_sleep_file))
  expect_that(round(nightsummary$number_sib_wakinghours[1], digits = 4), equals(6))
  expect_true(as.logical(nightsummary$acc_available[1]))
  expect_true(as.logical(nightsummary$sleeplog_used[1]))
  
  #--------------------------------------------
  #part 5
  g.part5(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1, desiredtz = desiredtz,
          strategy = 1, maxdur = Ndays, hrs.del.start = 0, hrs.del.end = 0,
          loglocation = sleeplog_fn,
          overwrite = TRUE, excludefirstlast = FALSE, do.parallel = do.parallel,
          frag.metrics = "all", save_ms5rawlevels = TRUE,
          part5_agg2_60seconds = TRUE, do.sibreport = TRUE, nap_model = "hip3yr")
  sibreport_dirname = "output_test/meta/ms5.outraw/sib.reports"
  expect_true(dir.exists(sibreport_dirname))
  expect_true(file.exists(paste0(sibreport_dirname, "/sib_report_123A_testaccfile_T5A5.csv")))
  
  dirname = "output_test/meta/ms5.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  
  dirname = "output_test/meta/ms5.out/"
  
  expect_true(dir.exists(dirname))
  expect_true(file.exists(rn[1]))
  expect_that(nrow(output),equals(3)) # changed because part5 now gives also first and last day
  expect_that(ncol(output),equals(153))
  expect_that(round(as.numeric(output$wakeup[2]), digits = 4), equals(36))
  dirname_raw = "output_test/meta/ms5.outraw/40_100_400"
  rn2 = dir(dirname_raw,full.names = TRUE, recursive = T)
  expect_true(file.exists(rn2[1]))
  TSFILE = read.csv(rn2[1])
  expect_that(nrow(TSFILE),equals(1150))
  expect_equal(ncol(TSFILE), 11) # 11 columns now because of nap_nonwear classification
  expect_equal(length(unique(TSFILE$class_id)), 10)
  #GGIR
  suppressWarnings(GGIR(mode = c(2,3,4,5), datadir = fn, outputdir = getwd(),
                        studyname = "test", f0 = 1, f1 = 1,
                        do.report = c(2,4,5), overwrite = FALSE, visualreport = FALSE, viewingwindow = 1,
                        do.parallel = do.parallel, minimumFileSizeMB = minimumFileSizeMB,
                        verbose = FALSE))
  suppressWarnings(GGIR(mode = c(), datadir = fn, outputdir = getwd(), studyname = "test",
                        f0 = 1, f1 = 1,
                        do.report = c(), overwrite = FALSE, visualreport = TRUE, viewingwindow = 1,
                        do.parallel = do.parallel, minimumFileSizeMB = minimumFileSizeMB, 
                        verbose = FALSE))
  expect_true(file.exists("output_test/results/part2_daysummary.csv"))
  expect_true(file.exists("output_test/results/part2_summary.csv"))
  expect_true(file.exists("output_test/results/part4_nightsummary_sleep_cleaned.csv"))
  expect_true(file.exists("output_test/results/part4_summary_sleep_cleaned.csv"))
  expect_true(file.exists("output_test/results/file summary reports/Report_123A_testaccfile.csv.pdf"))
  dn = "output_test"
  
  #=======================
  # Different variations on part 4:
  #--------------------------------------------
  # part 4 without sleeplog
  g.part4(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
          idloc = 2, loglocation = c(), do.visual = TRUE, outliers.only = FALSE,
          excludefirstlast = FALSE, criterror = 1, includenightcrit = 0,
          nnights = 7, colid = 1, coln1 = 2,
          relyonguider = FALSE, desiredtz = desiredtz,
          storefolderstructure = TRUE, overwrite = TRUE, verbose = FALSE)
  
  dirname = "output_test/meta/ms4.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_true(file.exists(vis_sleep_file))
  expect_that(round(nightsummary$SptDuration[1], digits = 4), equals(18.075))
  expect_true(as.logical(nightsummary$acc_available[1]))
  expect_false(as.logical(nightsummary$sleeplog_used[1]))
  
  #----------------------------------------------------------------------
  # Part 4 - daysleeper scenario by modifying the part 3 output & criterror = 0, and relyonguider=TRUE
  SPTE_end = c(37, 37) # turn into midday
  row2copy = which(sib.cla.sum$sib.onset.time == "2016-06-24T05:10:15+0100")
  newrow = sib.cla.sum[row2copy,]
  newrow$sib.onset.time = "2016-06-24T12:30:15+0100"
  newrow$sib.end.time = "2016-06-24T12:45:15+0100"
  sib.cla.sum = rbind(sib.cla.sum, newrow)
  sib.cla.sum = sib.cla.sum[order(sib.cla.sum$sib.onset.time),]
  sib.cla.sum$sib.period[1:11] = 1:11
  save(L5list, SPTE_end, SPTE_start, sib.cla.sum, tib.threshold, file = rn3[1])
  g.part4(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
          idloc = 2, loglocation = c(), do.visual = TRUE,
          outliers.only = FALSE, excludefirstlast = FALSE,
          criterror = 0, includenightcrit = 0, nnights = 7,
          colid = 1, coln1 = 2, relyonguider = TRUE,
          desiredtz = desiredtz, storefolderstructure = TRUE,
          overwrite = TRUE, verbose = FALSE)
  dirname = "output_test/meta/ms4.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  vis_sleep_file = "output_test/results/visualisation_sleep.pdf"
  
  expect_true(dir.exists(dirname))
  expect_true(file.exists(vis_sleep_file))
  expect_that(round(nightsummary$number_sib_wakinghours[1], digits = 4), equals(0))
  expect_that(round(nightsummary$SptDuration[1], digits = 2), equals(16.14))
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
  sib.cla.sum$sib.onset.time = changetime(sib.cla.sum$sib.onset.time)
  sib.cla.sum$sib.end.time = changetime(sib.cla.sum$sib.end.time)
  save(L5list, SPTE_end, SPTE_start, sib.cla.sum, tib.threshold, file = rn3[1])
  g.part4(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
          idloc = 2, loglocation = sleeplog_fn, do.visual = TRUE,
          outliers.only = FALSE, excludefirstlast = FALSE, criterror = 0,
          includenightcrit = 0, nnights = 7, colid = 1, coln1 = 2,
          relyonguider = TRUE, desiredtz = desiredtz,
          storefolderstructure = TRUE, overwrite = TRUE, verbose = FALSE)
  dirname = "output_test/meta/ms4.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_true(dir.exists(dirname))
  expect_that(round(nightsummary$number_sib_wakinghours[1], digits = 4), equals(10))
  expect_that(round(nightsummary$SptDuration[1], digits = 4), equals(7))
  
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
  save(L5list, SPTE_end, SPTE_start, sib.cla.sum, tib.threshold, file = rn3[1])
  g.part4(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
          idloc = 2, loglocation = sleeplog_fn, do.visual = TRUE,
          outliers.only = FALSE, excludefirstlast = FALSE,
          criterror = 0, includenightcrit = 0, nnights = 7,
          colid = 1, coln1 = 2, relyonguider = FALSE,
          desiredtz = desiredtz, storefolderstructure = TRUE,
          overwrite = TRUE, verbose = FALSE)
  dirname = "output_test/meta/ms4.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_true(dir.exists(dirname))
  expect_that(round(nightsummary$number_sib_wakinghours[1], digits = 4), equals(6))
  expect_that(round(nightsummary$SptDuration[1], digits = 4), equals(13.075))
  #---------------------
  # Part 1 with external function and selectdaysfile:
  exampleExtFunction = function(data=c(), parameters=c()) {
    data = data.frame(data, agglevel = round((1:nrow(data)) / (30 * 60 * 15)))
    output = aggregate(data, by = list(data$agglevel), FUN = mean)
    output = output[, -c(1, 2, ncol(output))]
    return(output)
  }
  myfun =  list(FUN = exampleExtFunction,
                parameters = 1.1,
                expected_sample_rate = 3, # resample data to 30 Hertz before applying function
                expected_unit = "mg",
                minlength = 15,
                outputres = 15,
                colnames = c("A", "B", "C"),
                outputtype = "numeric", #"numeric" (averaging is possible), "category" (majority vote)
                aggfunction = mean,
                timestamp = as.numeric(Sys.time())) # for unit test only
  # create selectdaysfile
  SDF = matrix("", 1, 3)
  SDF[1, 1] = "MOS2D12345678"
  SDF[1, 2:3] =  c("23-05-2016", "24-05-2016")
  colnames(SDF) = c("Monitor", "Day1", "Day2")
  selectdaysfile = "selectdaysfile.csv"
  write.csv(SDF, file = selectdaysfile)
  
  
  g.part1(datadir = fn, outputdir = getwd(), f0 = 1, f1 = 1,
          overwrite = TRUE, desiredtz = desiredtz,
          do.parallel = do.parallel, myfun = myfun,
          studyname = "test", do.enmo = TRUE, do.anglez = TRUE, do.cal = FALSE,
          windowsizes = c(15, 300, 3600),
          chunksize = 2, do.en = TRUE, do.anglex = TRUE, do.angley = TRUE,
          do.roll_med_acc_x = TRUE, do.roll_med_acc_y = TRUE, do.roll_med_acc_z = TRUE,
          do.dev_roll_med_acc_x = TRUE, do.dev_roll_med_acc_y = TRUE, do.dev_roll_med_acc_z = TRUE,
          do.bfx = TRUE, do.bfy = TRUE, do.bfz = TRUE, do.hfen = TRUE,
          do.hfx = TRUE, do.hfy = TRUE, do.hfz = TRUE, do.lfen = TRUE,
          do.enmoa = TRUE, selectdaysfile = selectdaysfile, verbose = FALSE)
  
  rn = dir("output_test/meta/basic/", full.names = TRUE)
  load(rn[1])
  expect_equal(ncol(M$metashort), 24)
  expect_equal(mean(M$metashort$B, na.rm = T), 24.673, tolerance = 3)
  expect_equal(mean(M$metashort$C, na.rm = T), -6.642, tolerance = 3)
  expect_equal(mean(M$metashort$EN, na.rm = T), 1.029, tolerance = 3)
  expect_equal(mean(M$metashort$angley, na.rm = T), 0.765, tolerance = 3)
  expect_equal(mean(M$metashort$roll_med_acc_x, na.rm = T), 0.729, tolerance = 3)
  expect_equal(mean(M$metashort$roll_med_acc_z, na.rm = T), 0.007, tolerance = 3)
  expect_equal(mean(M$metashort$dev_roll_med_acc_x, na.rm = T), 0.007, tolerance = 3)
  expect_equal(mean(M$metashort$ENMOa, na.rm = T), 0.03, tolerance = 3)
  
  
  if (file.exists(selectdaysfile)) file.remove(selectdaysfile)
  if (file.exists(dn))  unlink(dn, recursive = TRUE)
  if (file.exists(fn)) file.remove(fn)
  if (file.exists(selectdaysfile)) file.remove(selectdaysfile)
  if (file.exists(sleeplog_fn)) file.remove(sleeplog_fn)
})
