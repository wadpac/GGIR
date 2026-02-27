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
  # isfilelist
  expect_true(isfilelist("file1.bin"))
  expect_true(isfilelist("file1.csv"))
  expect_true(isfilelist("file1.wav"))
  expect_true(isfilelist("file1.cwa"))
  expect_true(isfilelist("file1.gt3x"))
  expect_true(isfilelist(c("file1.bin", "file2.bin")))
  #--------------------------------------------
  # part 1
  g.part1(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
          overwrite = TRUE, desiredtz = desiredtz,
          do.enmo = TRUE, do.anglez = TRUE ,do.cal = TRUE,
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
  # part 2 with data_masking_strategy = 3
  g.part2(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
          idloc = 2, desiredtz = desiredtz, ndayswindow = 1,
          data_masking_strategy = 3, overwrite = TRUE, hrs.del.start = 0, hrs.del.end = 0,
          maxdur = Ndays, includedaycrit = 0, do.parallel = do.parallel, myfun = c(),
          verbose = FALSE)
  dirname = "output_test/meta/ms2.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  load("output_test/meta/basic/meta_123A_testaccfile.csv.RData")
  expect_equal(nrow(IMP$metashort), 11280)
  expect_equal(rle(IMP$rout$r4)$lengths[1], 10)
  expect_equal(rle(IMP$rout$r4)$lengths[3], 13)
  expect_equal(round(mean(IMP$metashort$ENMO), digits = 5), 0.00802, tolerance = 3)
  expect_equal(round(as.numeric(SUM$summary$meas_dur_def_proto_day), digits = 3), 1)
  expect_equal(SUM$summary$`N valid weekend days (WE)`, 1)
  expect_equal(SUM$summary$`N valid weekdays (WD)`, 2)
  # check the ndayswindow included is 24 hours exactly 
  # ndayswindow = 1 with windowsizes = c(15, 3600, 3600)
  first_epoch_in_protocol = rle(IMP$rout$r4)$lengths[1] + 1
  last_epoch_in_protocol = max(which(IMP$rout$r4 == 0))
  expect_equal(last_epoch_in_protocol - first_epoch_in_protocol + 1,  24)
  
  # part 2 with data_masking_strategy = 3 and hrs.del.start = 6 and hrs.del.end = 6
  g.part2(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
          idloc = 2, desiredtz = desiredtz, ndayswindow = 1,
          strategy = 3, overwrite = TRUE, hrs.del.start = 6, hrs.del.end = 6,
          maxdur = Ndays, includedaycrit = 0, do.parallel = do.parallel, myfun = c(),
          verbose = FALSE)
  dirname = "output_test/meta/ms2.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_equal(nrow(IMP$metashort), 11280)
  expect_equal(rle(IMP$rout$r4)$lengths[1], 16)
  expect_equal(rle(IMP$rout$r4)$lengths[3], 19)
  # check the ndayswindow included is 12 hours exactly (24 minus hrs.del.start/end)
  # ndayswindow = 1 with windowsizes = c(15, 3600, 3600)
  first_epoch_in_protocol = rle(IMP$rout$r4)$lengths[1] + 1
  last_epoch_in_protocol = max(which(IMP$rout$r4 == 0))
  expect_equal(last_epoch_in_protocol - first_epoch_in_protocol + 1,  12)
  
  # part 2 with data_masking_strategy = 5
  g.part2(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
          idloc = 2, desiredtz = desiredtz, ndayswindow = 1,
          strategy = 5, overwrite = TRUE, hrs.del.start = 0, hrs.del.end = 0,
          maxdur = Ndays, includedaycrit = 0, do.parallel = do.parallel, myfun = c(),
          verbose = FALSE)
  dirname = "output_test/meta/ms2.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_equal(nrow(IMP$metashort), 11280)
  expect_equal(rle(IMP$rout$r4)$lengths[1], 15)
  expect_equal(rle(IMP$rout$r4)$lengths[3], 8)
  expect_equal(round(mean(IMP$metashort$ENMO), digits = 5), 0.03398, tolerance = 3)
  expect_equal(round(as.numeric(SUM$summary$meas_dur_def_proto_day), digits = 3), 1)
  expect_equal(SUM$summary$`N valid weekend days (WE)`, 1)
  expect_equal(SUM$summary$`N valid weekdays (WD)`, 2)
  # check the ndayswindow included is 24 hours exactly
  # ndayswindow = 1 with windowsizes = c(15, 3600, 3600)
  first_epoch_in_protocol = rle(IMP$rout$r4)$lengths[1] + 1
  last_epoch_in_protocol = max(which(IMP$rout$r4 == 0))
  expect_equal(last_epoch_in_protocol - first_epoch_in_protocol + 1,  24)
  
  # part 2 with data_masking_strategy = 5 and hrs.del.start = 6 and hrs.del.end = 6
  g.part2(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
          idloc = 2, desiredtz = desiredtz, ndayswindow = 1,
          data_masking_strategy = 5, overwrite = TRUE, hrs.del.start = 6, hrs.del.end = 6,
          maxdur = Ndays, includedaycrit = 0, do.parallel = do.parallel, myfun = c(),
          verbose = FALSE)
  dirname = "output_test/meta/ms2.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_equal(nrow(IMP$metashort), 11280)
  expect_equal(rle(IMP$rout$r4)$lengths[1], 21)
  expect_equal(rle(IMP$rout$r4)$lengths[3], 14)
  # check the ndayswindow included is 12 hours exactly (24 minus hrs.del.start/end)
  # ndayswindow = 1 with windowsizes = c(15, 3600, 3600)
  first_epoch_in_protocol = rle(IMP$rout$r4)$lengths[1] + 1
  last_epoch_in_protocol = max(which(IMP$rout$r4 == 0))
  expect_equal(last_epoch_in_protocol - first_epoch_in_protocol + 1,  12)
  
  # part 2 with data_masking_strategy = 2 and iglevels = 1
  g.part2(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
          idloc = 2, desiredtz = desiredtz,
          data_masking_strategy = 2,overwrite = TRUE, hrs.del.start = 0,hrs.del.end = 0,
          maxdur = Ndays, includedaycrit = 0, do.imp = FALSE, epochvalues2csv = TRUE, iglevels = TRUE,
          do.parallel = do.parallel, myfun = c(), winhr = 16, MX.ig.min.dur = 14, verbose = FALSE)
  dirname = "output_test/meta/ms2.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_that(nrow(IMP$metashort),equals(11280))
  expect_equal(mean(IMP$metashort$ENMO), 0.029, tolerance = 3)
  expect_equal(as.numeric(SUM$summary$meas_dur_def_proto_day), 1, tolerance = 2)
  expect_equal(as.numeric(SUM$daysummary$`L16_ig_gradient_ENMO_mg_0-24hr`[1]), -1.12, tolerance = 2)
  expect_equal(as.numeric(SUM$daysummary$`L16hr_ENMO_mg_0-24hr`), c(33.5, 19.8333, 13), tolerance = 0.001)
  # part 2
  
  # part 2 with data_masking_strategy = 1
  g.part2(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
          idloc = 2, desiredtz = desiredtz,
          data_masking_strategy = 1,overwrite = TRUE, hrs.del.start = 0,hrs.del.end = 0,
          maxdur = Ndays, includedaycrit = 0, qM5L5 = c(0.2,0.4), winhr = c(3,10),
          do.parallel = do.parallel, myfun = c(), qlevels = c(0.5, 0.9),
          cosinor = TRUE, verbose = FALSE)
  params_output = load_params()$params_output
  g.report.part2(metadatadir = metadatadir, f0 = 1, f1 = 1, params_output = params_output)
  dirname = "output_test/meta/ms2.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  summarycsv = "output_test/results/part2_summary.csv"
  daysummarycsv = "output_test/results/part2_daysummary.csv"
  expect_true(dir.exists(dirname))
  expect_true(file.exists(summarycsv))
  expect_true(file.exists(daysummarycsv))
  expect_that(nrow(IMP$metashort), equals(11280))
  expect_equal(round(mean(IMP$metashort$ENMO), digits = 5), 0.02911, tolerance = 0.01)
  expect_that(round(as.numeric(SUM$summary$meas_dur_dys), digits = 5), equals(1.95833))
  expect_that(ncol(SUM$daysummary), equals(34))
  expect_equal(SUM$daysummary$`p50_ENMO_mg_0-24hr`, c(17.15, 33,  0), tolerance = 0.5)
  expect_equal(round(as.numeric(SUM$daysummary$`p90_ENMO_mg_0-24hr`)), c(44, 54, 41), tolerance = 1)
  expect_equal(mean(as.numeric(SUM$daysummary$`M3_ENMO_mg_0-24hr`)), 89.26, tolerance = 1)
  expect_equal(mean(as.numeric(SUM$daysummary$`M3_q40_ENMO_mg_0-24hr`)), 37.383, tolerance = 1)
  expect_equal(as.numeric(SUM$summary$cosinor_acrophase), 3.260855, tolerance = 1)
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
          excludefirstlast = FALSE, criterror = 1, includenightcrit = 0, #nnights = 7,
          colid = 1, coln1 = 2, relyonguider = FALSE, desiredtz = desiredtz,
          storefolderstructure = FALSE, overwrite = TRUE)
  dirname = "output_test/meta/ms4.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  vis_sleep_file = "output_test/results/visualisation_sleep.pdf"
  params_sleep = load_params()$params_sleep
  params_sleep[["loglocation"]] = sleeplog_fn
  g.report.part4(datadir = fn, metadatadir = metadatadir, 
                 f0 = 1, f1 = 1, params_sleep = params_sleep,
                 params_output = params_output, verbose = FALSE)
  expect_true(dir.exists(dirname))
  expect_true(file.exists(vis_sleep_file))
  expect_that(round(nightsummary$number_sib_wakinghours[1], digits = 4), equals(6))
  expect_true(as.logical(nightsummary$acc_available[1]))
  expect_true(as.logical(nightsummary$sleeplog_used[1]))

  #--------------------------------------------
  #part 5
  g.part5(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1, desiredtz = desiredtz,
          data_masking_strategy = 1, maxdur = Ndays, hrs.del.start = 0, hrs.del.end = 0,
          loglocation = sleeplog_fn,
          overwrite = TRUE, excludefirstlast = FALSE, do.parallel = do.parallel,
          frag.metrics = "all", save_ms5rawlevels = TRUE, save_ms5raw_format = "csv",
          part5_agg2_60seconds = TRUE, do.sibreport = TRUE, 
          iglevels = 1, timewindow = c("MM", "WW", "OO"),
          possible_nap_window = c(0, 24),
          possible_nap_dur = c(0, 240),
          possible_nap_edge_acc = Inf,
          possible_nap_gap = 0)
  sibreport_dirname = "output_test/meta/ms5.outraw/sib.reports"
  expect_true(dir.exists(sibreport_dirname))
  expect_true(file.exists(paste0(sibreport_dirname, "/sib_report_123A_testaccfile_T5A5.csv")))
  
  dirname = "output_test/meta/ms5.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  
  dirname = "output_test/meta/ms5.out/"
  
  expect_true(dir.exists(dirname))
  expect_true(file.exists(rn[1]))
  expect_that(nrow(output),equals(5))
  expect_that(ncol(output),equals(158))
  expect_that(round(as.numeric(output$wakeup[2]), digits = 4), equals(36))
  expect_that(as.numeric(output$dur_day_spt_min[4]), equals(1150)) # WW window duration
  expect_that(as.numeric(output$dur_day_spt_min[5]), equals(1680)) # OO window duration
  dirname_raw = "output_test/meta/ms5.outraw/40_100_400"
  rn2 = dir(dirname_raw,full.names = TRUE, recursive = T)
  rn2_index = grep(pattern = "[.]csv", x = rn2, value = FALSE)
  expect_true(file.exists(rn2[rn2_index]))
  TSFILE = read.csv(rn2[rn2_index])
  expect_that(nrow(TSFILE),equals(2820))
  expect_equal(ncol(TSFILE), 13)
  expect_equal(length(unique(TSFILE$class_id)), 11)
  #GGIR
  suppressWarnings(GGIR(mode = c(2,3,4,5), datadir = fn, outputdir = getwd(),
                        studyname = "test", f0 = 1, f1 = 1,
                        do.report = c(2,4,5), overwrite = FALSE, visualreport = FALSE, viewingwindow = 1,
                        do.parallel = do.parallel, minimumFileSizeMB = minimumFileSizeMB,
                        verbose = FALSE, storefolderstructure = TRUE))
  suppressWarnings(GGIR(mode = c(), datadir = fn, outputdir = getwd(), studyname = "test",
                        f0 = 1, f1 = 1,
                        do.report = c(), overwrite = FALSE, visualreport = TRUE, viewingwindow = 1,
                        do.parallel = do.parallel, minimumFileSizeMB = minimumFileSizeMB, 
                        verbose = FALSE, old_visualreport = TRUE))
  expect_true(file.exists("output_test/results/part2_daysummary.csv"))
  expect_true(file.exists("output_test/results/part2_summary.csv"))
  expect_true(file.exists("output_test/results/part4_nightsummary_sleep_cleaned.csv"))
  expect_true(file.exists("output_test/results/part4_summary_sleep_cleaned.csv"))
  expect_true(file.exists("output_test/results/file summary reports/old_report_123A_testaccfile.csv.pdf"))
  expect_true(file.exists("output_test/results/part5_daysummary_MM_L40M100V400_T5A5.csv"))
  expect_true(file.exists("output_test/results/part5_daysummary_WW_L40M100V400_T5A5.csv"))
  expect_true(file.exists("output_test/results/part5_daysummary_OO_L40M100V400_T5A5.csv"))
  dn = "output_test"
  
  # Expect warning when unknown parameters are provided
  expect_error(GGIR(mode = NULL, datadir = fn, outputdir = getwd(),
                    studyname = "test", f0 = 1, f1 = 1,
                    do.report = NULL, overwrite = FALSE,
                    verbose = FALSE,
                    iamnewhere = 0),
               paste0("\nParameter iamnewhere is unknown to GGIR and will not be",
                      " used, please check for typos or remove."))
  
  expect_error(GGIR(mode = NULL, datadir = fn, outputdir = getwd(),
                    studyname = "test", f0 = 1, f1 = 1,
                    do.report = NULL, overwrite = FALSE,
                    verbose = FALSE,
                    iamnewhere = 0,
                    iamnewtoo = 1),
               paste0("\nParameters iamnewhere and iamnewtoo are unknown",
                      " to GGIR and will not be used, please check for ",
                      "typos or remove these."))
  

  #=======================
  # Different variations on part 4:
  #--------------------------------------------
  #--------------------------------------------
  # part 4 with sleepwindowType = TimeInBed and sleepefficiency.metric = 2
  g.part4(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
          idloc = 2, loglocation = sleeplog_fn, do.visual = TRUE, outliers.only = FALSE,
          excludefirstlast = FALSE, criterror = 1, includenightcrit = 0, #nnights = 7,
          colid = 1, coln1 = 2, relyonguider = FALSE, desiredtz = desiredtz,
          storefolderstructure = FALSE, overwrite = TRUE,
          sleepwindowType = "TimeInBed", sleepefficiency.metric = 2)
  dirname = "output_test/meta/ms4.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_true("sleeplatency" %in% colnames(nightsummary))
  expect_true("sleepefficiency" %in% colnames(nightsummary))
  expect_equal(round(nightsummary$sleeplatency[1], 3), 0.171)
  expect_equal(round(nightsummary$sleepefficiency[1], 3), 0.951)
  
  #--------------------------------------------
  # part 4 with sleepwindowType = TimeInBed and sleepefficiency.metric = 1
  g.part4(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
          idloc = 2, loglocation = sleeplog_fn, do.visual = TRUE, outliers.only = FALSE,
          excludefirstlast = FALSE, criterror = 1, includenightcrit = 0, #nnights = 7,
          colid = 1, coln1 = 2, relyonguider = FALSE, desiredtz = desiredtz,
          storefolderstructure = FALSE, overwrite = TRUE,
          sleepwindowType = "TimeInBed", sleepefficiency.metric = 1)
  dirname = "output_test/meta/ms4.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_true("sleeplatency" %in% colnames(nightsummary))
  expect_true("sleepefficiency" %in% colnames(nightsummary))
  expect_equal(round(nightsummary$sleepefficiency[1], 3), 0.851)
  
  #--------------------------------------------
  # part 4 without sleeplog
  g.part4(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
          idloc = 2, loglocation = c(), do.visual = TRUE, outliers.only = FALSE,
          excludefirstlast = FALSE, criterror = 1, includenightcrit = 0,
          nnights = 7, colid = 1, coln1 = 2,
          relyonguider = FALSE, desiredtz = desiredtz,
          storefolderstructure = TRUE, overwrite = TRUE, verbose = FALSE,
          sleepwindowType = "SPT")
  
  dirname = "output_test/meta/ms4.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_true(file.exists(vis_sleep_file))
  expect_equal(round(nightsummary$SptDuration[1], digits = 4), 18.0792, tolerance = 0.0005)
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
  part3_guider = "HDCZA"
  save(L5list, SPTE_end, SPTE_start, sib.cla.sum, tib.threshold, part3_guider,
       file = rn3[1])
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
  expect_equal(nightsummary$SptDuration[1], 16.15, tolerance = 0.01)
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
  expect_equal(round(nightsummary$SptDuration[1], digits = 4), 13.0792)
  #---------------------
  # Part 1 with external function:
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
  
  g.part1(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
          overwrite = TRUE, desiredtz = desiredtz,
          do.parallel = do.parallel, myfun = myfun,
          do.enmo = TRUE, do.anglez = TRUE, do.cal = FALSE,
          windowsizes = c(15, 300, 3600),
          chunksize = 2, do.en = TRUE, do.anglex = TRUE, do.angley = TRUE,
          do.roll_med_acc_x = TRUE, do.roll_med_acc_y = TRUE, do.roll_med_acc_z = TRUE,
          do.dev_roll_med_acc_x = TRUE, do.dev_roll_med_acc_y = TRUE, do.dev_roll_med_acc_z = TRUE,
          do.bfx = TRUE, do.bfy = TRUE, do.bfz = TRUE, do.hfen = TRUE,
          do.hfx = TRUE, do.hfy = TRUE, do.hfz = TRUE, do.lfen = TRUE,
          do.enmoa = TRUE, verbose = FALSE)
  
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
  
  if (dir.exists(dn))  unlink(dn, recursive = TRUE)
  if (file.exists(fn)) unlink(fn)
  if (file.exists(sleeplog_fn)) file.remove(sleeplog_fn)
})
