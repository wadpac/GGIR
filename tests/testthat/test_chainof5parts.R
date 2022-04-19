library(GGIR)
context("Chainof5parts")
test_that("chainof5parts", {
  skip_on_cran()

  
  # dirR = "~/GGIR/R"
  # ffnames = dir(dirR) # creating list of filenames of scriptfiles to load
  # ffnames = ffnames[which(ffnames %in% c("g.cwaread.R", "read.gt3x_ggir.R") == FALSE)]
  # for (i in 1:length(ffnames)) {
  #   source(paste(dirR,"/",ffnames[i],sep="")) #loading scripts for reading geneactiv data
  # }
  # library(testthat)
  
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
          minimumFileSizeMB = minimumFileSizeMB)
  expect_true(dir.exists(dn))
  rn = dir("output_test/meta/basic/",full.names = TRUE)
  load(rn[1])
  expect_that(round(C$scale, digits = 5), equals(c(0.98474, 0.98393, 0.98439)))
  expect_that(nrow(M$metalong),equals(47))
  # expect_that(M$metalong[2,1],equals("2016-06-23T09:15:00+0100")) # turned off because not consistent across machines, to investigate
  expect_that(nrow(M$metashort),equals(11280))
  expect_that(round(mean(M$metashort$ENMO), digits = 5), equals(0.02896))
  expect_that(I$monc, equals(3))
  expect_that(I$sf, equals(3))
  expect_that(I$dformc, equals(2))
  expect_that(C$npoints, equals(9768))
  #-------------------------
  # part 2 with strategy = 3
  g.part2(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
          idloc = 2, desiredtz = desiredtz,
          strategy = 3, overwrite = TRUE, hrs.del.start = 0, hrs.del.end = 0,
          maxdur = Ndays, includedaycrit = 0, do.parallel = do.parallel, myfun = c())
  dirname = "output_test/meta/ms2.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_that(nrow(IMP$metashort),equals(11280))
  expect_that(round(mean(IMP$metashort$ENMO), digits = 5), equals(0.00796))
  expect_that(round(as.numeric(SUM$summary$meas_dur_def_proto_day), digits = 3), equals(0.417))
  # part 2 with strategy = 2 and iglevels = 1
  g.part2(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
          idloc = 2, desiredtz = desiredtz,
          strategy = 2,overwrite = TRUE, hrs.del.start = 0,hrs.del.end = 0,
          maxdur = Ndays, includedaycrit = 0, do.imp = FALSE, epochvalues2csv = TRUE, iglevels = TRUE,
          do.parallel = do.parallel, myfun = c(), winhr = 16, MX.ig.min.dur = 14)
  dirname = "output_test/meta/ms2.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_that(nrow(IMP$metashort),equals(11280))
  expect_that(round(mean(IMP$metashort$ENMO),digits = 3), equals(0.029))
  expect_that(round(as.numeric(SUM$summary$meas_dur_def_proto_day), digits = 2), equals(1))
  expect_equal(round(as.numeric(SUM$daysummary$`L16_ig_gradient_ENMO_mg_0-24hr`[1]), digits = 2), -1.11)
  # part 2 with strategy = 1
  g.part2(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
          idloc = 2, desiredtz = desiredtz,
          strategy = 1,overwrite = TRUE, hrs.del.start = 0,hrs.del.end = 0,
          maxdur = Ndays, includedaycrit = 0, qM5L5 = c(0.2,0.4), winhr = c(3,10),
          do.parallel = do.parallel, myfun = c())
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
  expect_that(round(mean(IMP$metashort$ENMO), digits = 5), equals(0.02896))
  expect_that(round(as.numeric(SUM$summary$meas_dur_dys), digits = 5), equals(1.95833))
  expect_that(ncol(SUM$daysummary), equals(32))
  expect_that(round(mean(as.numeric(SUM$daysummary$`M3_ENMO_mg_0-24hr`)), digits = 3),equals(88.946))
  expect_that(round(mean(as.numeric(SUM$daysummary$`M3_q40_ENMO_mg_0-24hr`)), digits = 3), equals(37.067))

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
  expect_warning(g.part4(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
          idloc = 2, loglocation = sleeplog_fn, do.visual = TRUE, outliers.only = FALSE,
          excludefirstlast = FALSE, criterror = 1, includenightcrit = 0, nnights = 7,
          colid = 1, coln1 = 2, relyonguider = FALSE, desiredtz = desiredtz,
          storefolderstructure = FALSE, overwrite = TRUE))
  dirname = "output_test/meta/ms4.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  vis_sleep_file = "output_test/results/visualisation_sleep.pdf"
  g.report.part4(datadir = fn, metadatadir = metadatadir, loglocation = sleeplog_fn,
                 f0 = 1, f1 = 1)
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
          # frag.classes.day = c("day_IN_bts", "day_IN_unbt"),  frag.classes.spt = "spt_sleep",
          frag.metrics = "all", save_ms5rawlevels = TRUE)
  dirname = "output_test/meta/ms5.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_true(dir.exists(dirname))
  expect_true(file.exists(rn[1]))
  expect_that(nrow(output),equals(3)) # changed because part5 now gives also first and last day
  expect_that(ncol(output),equals(149))
  expect_that(round(as.numeric(output$wakeup[2]), digits = 4), equals(35.9958))
  dirname_raw = "output_test/meta/ms5.outraw/40_100_400"
  rn2 = dir(dirname_raw,full.names = TRUE, recursive = T)
  expect_true(file.exists(rn2[1]))
  TSFILE = read.csv(rn2[1])
  expect_that(nrow(TSFILE),equals(4601))
  expect_that(ncol(TSFILE),equals(10))
  expect_that(length(unique(TSFILE$class_id)),equals(10))
 #--------------------------------------------
  #g.shell.GGIR
  suppressWarnings(g.shell.GGIR(mode = c(2,3,4,5), datadir = fn, outputdir = getwd(),
                                studyname = "test", f0 = 1, f1 = 1,
                          do.report = c(2,4,5), overwrite = FALSE, visualreport = FALSE, viewingwindow = 1,
                          do.parallel = do.parallel, minimumFileSizeMB = minimumFileSizeMB))
  suppressWarnings(g.shell.GGIR(mode = c(), datadir = fn, outputdir = getwd(), studyname = "test",
                                f0 = 1, f1 = 1,
                                do.report = c(), overwrite = FALSE, visualreport = TRUE, viewingwindow = 1,
                                do.parallel = do.parallel, minimumFileSizeMB = minimumFileSizeMB))
  expect_true(file.exists("output_test/results/part2_daysummary.csv"))
  expect_true(file.exists("output_test/results/part2_summary.csv"))
  expect_true(file.exists("output_test/results/part4_nightsummary_sleep_cleaned.csv"))
  expect_true(file.exists("output_test/results/part4_summary_sleep_cleaned.csv"))
  expect_true(file.exists("output_test/results/file summary reports/Report_123A_testaccfile.csv.pdf"))
  dn = "output_test"
  #--------------------------------------------
  # create dummy selectdaysfile
  selectdays = data.frame(Monitor = "MOS2D12345678",
                          Day1 = "24/06/2016",
                          Day2 = "25/06/2016", stringsAsFactors = TRUE)
  selectdaysfile = paste0(getwd(), "/selectdaysfile.csv")
  write.csv(selectdays, file = selectdaysfile, row.names = FALSE, fileEncoding = "UTF-8")
  # we will now use it in g.part2, not sure whether g.part2 will actually be able to handle this.
  # normally, g.part1 would use the file to cut up the measurement, but that only works for GENEActiv
  # data and we do not have a multi-data GENEActiv test file in the package.
  g.part2(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
          idloc = 2, desiredtz = desiredtz, strategy = 1,
          overwrite = TRUE, hrs.del.start = 0, hrs.del.end = 0,
          maxdur = Ndays, includedaycrit = 0, selectdaysfile = selectdaysfile,
          storefolderstructure = TRUE, do.parallel = do.parallel, myfun = c())
  rn = dir(dirname, full.names = TRUE)
  load(rn[1])
  expect_that(nrow(IMP$metashort), equals(11280))
  expect_that(round(mean(IMP$metashort$ENMO), digits = 5), equals(0.02896))

  #=======================
  # Different variations on part 4:
  #--------------------------------------------
  # part 4 without sleeplog
  g.part4(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
                         idloc = 2, loglocation = c(), do.visual = TRUE, outliers.only = FALSE,
                         excludefirstlast = FALSE, criterror = 1, includenightcrit = 0,
                         nnights = 7, colid = 1, coln1 = 2,
                         relyonguider = FALSE, desiredtz = desiredtz,
                         storefolderstructure = TRUE, overwrite = TRUE)

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
  expect_warning(g.part4(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
                         idloc = 2, loglocation = c(), do.visual = TRUE,
                         outliers.only = FALSE, excludefirstlast = FALSE,
                         criterror = 0, includenightcrit = 0, nnights = 7,
                         colid = 1, coln1 = 2, relyonguider = TRUE,
                         desiredtz = desiredtz, storefolderstructure = TRUE,
                         overwrite = TRUE))
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
  expect_warning(g.part4(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
                         idloc = 2, loglocation = sleeplog_fn, do.visual = TRUE,
                         outliers.only = FALSE, excludefirstlast = FALSE, criterror = 0,
                         includenightcrit = 0, nnights = 7, colid = 1, coln1 = 2,
                         relyonguider = TRUE, desiredtz = desiredtz,
                         storefolderstructure = TRUE, overwrite = TRUE))
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
  expect_warning(g.part4(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1,
                         idloc = 2, loglocation = sleeplog_fn, do.visual = TRUE,
                         outliers.only = FALSE, excludefirstlast = FALSE,
                         criterror = 0, includenightcrit = 0, nnights = 7,
                         colid = 1, coln1 = 2, relyonguider = FALSE,
                         desiredtz = desiredtz, storefolderstructure = TRUE,
                         overwrite = TRUE))
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
          do.enmoa = TRUE, selectdaysfile = selectdaysfile)

  rn = dir("output_test/meta/basic/", full.names = TRUE)
  load(rn[1])
  expect_equal(ncol(M$metashort), 24)
  expect_equal(round(mean(M$metashort$B, na.rm = T), digits = 3), 24.718)
  expect_equal(round(mean(M$metashort$C, na.rm = T), digits = 3), -6.528)
  expect_equal(round(mean(M$metashort$EN, na.rm = T), digits = 3), 1.029)
  expect_equal(round(mean(M$metashort$angley, na.rm = T), digits = 3), 0.768)
  expect_equal(round(mean(M$metashort$roll_med_acc_x, na.rm = T), digits = 3), 0.728)
  expect_equal(round(mean(M$metashort$roll_med_acc_z, na.rm = T), digits = 3), 0.007)
  expect_equal(round(mean(M$metashort$dev_roll_med_acc_x, na.rm = T), digits = 3), 0.007)
  expect_equal(round(mean(M$metashort$ENMOa, na.rm = T),digits = 3), 0.03)

  SDF = "output_test/meta/raw/123A_testaccfile.csv_day1.RData"
  expect_true(file.exists(SDF))
  load(SDF)
  expect_that(nrow(data),equals(390600))
  expect_that(round(mean(data),digits = 3), equals(0.266))


  if (file.exists(selectdaysfile)) file.remove(selectdaysfile)
  if (file.exists(dn))  unlink(dn, recursive = TRUE)
  if (file.exists(fn)) file.remove(fn)
  if (file.exists(selectdaysfile)) file.remove(selectdaysfile)
  if (file.exists(sleeplog_fn)) file.remove(sleeplog_fn)
})
