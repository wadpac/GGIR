library(GGIR)
context("Part 1 function")
test_that("Metrics generation works properly", {
  skip_on_cran()
  
  # 0) Generate data for tests -----------------------------------------
  # Using sf = 8 Hertz because metric zc filters at 4 Hertz 
  create_test_acc_csv(Nmin = 2*60, sf = 8)
  fn = "123A_testaccfile.csv"
  metadatadir = paste0(getwd(), "/output_test")
  desiredtz = "Europe/London"
  dn = "output_test"
  if (file.exists(dn))  unlink(dn, recursive = TRUE)
  
  # 1) various metrics + external function + select days file -------
  # external function
  exampleExtFunction = function(data=c(), parameters=c()) {
    data = data.frame(data, agglevel = round((1:nrow(data)) / (30 * 60 * 15)))
    output = aggregate(data, by = list(data$agglevel), FUN = mean)
    output = output[, -c(1, 2, ncol(output))]
    return(output)
  }
  myfun =  list(FUN = exampleExtFunction,
                parameters = 1.1,
                expected_sample_rate = 30, # resample data to 30 Hertz before applying function
                expected_unit = "mg",
                minlength = 15,
                outputres = 15,
                colnames = c("A", "B", "C"),
                outputtype = "numeric", #"numeric" (averaging is possible), "category" (majority vote)
                aggfunction = mean,
                timestamp = as.numeric(Sys.time())) # for unit test only
  
  # run part 1
  g.part1(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1, overwrite = TRUE, desiredtz = desiredtz,
          do.cal = FALSE, windowsizes = c(60,1800,1800),
          # We are not doing all the metrics, because Travis-CI cannot allocate enough memory
          myfun = myfun, do.anglex = TRUE, do.angley = TRUE,
          do.en = TRUE, do.enmo = TRUE,do.lfenmo = TRUE,
          do.bfen = TRUE, do.hfenplus = TRUE,
          do.mad = TRUE, do.zcx = TRUE, #do.brondcounts = TRUE,
          do.bfx = TRUE, do.bfy = TRUE, do.bfz = TRUE, do.hfen = TRUE,
          do.hfx = TRUE, do.hfy = TRUE, do.hfz = TRUE, do.lfen = TRUE,
          do.roll_med_acc_x = TRUE, do.roll_med_acc_y = TRUE, do.roll_med_acc_z = TRUE,
          do.dev_roll_med_acc_x = TRUE, do.dev_roll_med_acc_y = TRUE, do.dev_roll_med_acc_z = TRUE,
          do.enmoa = TRUE, 
          verbose = FALSE)
  
  # tests
  rn = dir("output_test/meta/basic/",full.names = TRUE)
  load(rn[1])
  expect_equal(ncol(M$metashort), 29)
  expect_true(nrow(M$metashort) == 90)
  expect_equal(round(mean(M$metashort$B, na.rm = T), 3), 34.964)
  expect_equal(round(mean(M$metashort$C, na.rm = T), 3), 24.856)
  expect_equal(round(mean(M$metashort$BFEN, na.rm = T), 3),  0.107)
  expect_equal(round(mean(M$metashort$LFENMO, na.rm = T), 3),  0.052)
  expect_equal(round(mean(M$metashort$HFENplus, na.rm = T), 3),  0.168)
  expect_equal(round(mean(M$metashort$MAD, na.rm = T), 3),  0.017)
  expect_equal(round(mean(M$metashort$anglex, na.rm = T), 3),  87.347)
  expect_equal(round(mean(M$metashort$anglez, na.rm = T), 3),  1.360)
  expect_equal(round(mean(M$metashort$ZCX, na.rm = T), 3),  163.178)
  expect_equal(round(mean(M$metashort$EN, na.rm = T), 3),  1.054)
  expect_equal(round(mean(M$metashort$angley, na.rm = T), 3),  1.916)
  expect_equal(round(mean(M$metashort$roll_med_acc_x, na.rm = T), 3),  1.045)
  expect_equal(round(mean(M$metashort$roll_med_acc_z, na.rm = T), 3),  0.025)
  expect_equal(round(mean(M$metashort$dev_roll_med_acc_x, na.rm = T), 3),  0.016)
  expect_equal(round(mean(M$metashort$ENMOa, na.rm = T), 3),  0.054)
})

test_that("Nonwear and clipping detection", {
  skip_on_cran()
  # new data
  create_test_acc_csv(Nmin = 2*1440, sf = 3, starts_at_midnight = TRUE)
  fn = "123A_testaccfile.csv"
  metadatadir = paste0(getwd(), "/output_test")
  desiredtz = "Europe/London"
  dn = "output_test"
  if (file.exists(dn))  unlink(dn, recursive = TRUE)
  
  data = as.matrix(read.csv(fn, skip = 10))
  colnames(data) = c("x", "y", "z")
  
  # 2013 algorithm ------
  # clipthres to 1.7 to test the clipping detection
  NWCW = detect_nonwear_clipping(data = data, nonwear_approach = "2013", 
                                 sf = 3, clipthres = 1.7)
  CW = NWCW$CWav; NW = NWCW$NWav
  NW_rle_2013 = rle(NW)
  CW = sum(NWCW$CWav > 0)
  
  # 2023 algorithm ------
  NWCW = detect_nonwear_clipping(data = data, nonwear_approach = "2023", sf = 3)
  NW = NWCW$NWav
  NW_rle_2023 = rle(NW)
  # tests ----------------
  # Does it find the 2 periods of nonwear?
  expect_equal(sum(NW_rle_2013$values == 3), 2)
  expect_equal(sum(NW_rle_2023$values == 3), 2)
  
  # Expect the 2023 algorithm finds more nonwear than the 2013
  total_nonwear_2013 = sum(NW_rle_2013$lengths[which(NW_rle_2013$values == 3)])
  total_nonwear_2023 = sum(NW_rle_2023$lengths[which(NW_rle_2023$values == 3)])
  expect_true(total_nonwear_2023 > total_nonwear_2013)
  
  # Expect six ws2 windows with some clipping (values over 1.7 in this test)
  expect_equal(CW, 6)
})

test_that("Test recordings that start at midnight and recording sleep hour work properly", {
  skip_on_cran()
  fn = "123A_testaccfile.csv"
  create_test_sleeplog_csv(advanced = FALSE)
  sleeplog_fn = "testsleeplogfile.csv"
  dn = "output_test"
  if (file.exists(dn)) unlink(dn, recursive = TRUE)
  minimumFileSizeMB = 0
  #--------------------------------------------
  # run GGIR
  GGIR(datadir = fn, outputdir = getwd(), studyname = "test",
       verbose = FALSE, desiredtz = "Europe/London",
       loglocation = sleeplog_fn, colid = 1, coln1 = 2, 
       nnights = 7, timewindow = "MM")
  #--------------------------------------------
  # part 1 milestone data starts at midnight
  expect_true(dir.exists(dn))
  rn = dir("output_test/meta/basic/",full.names = TRUE)
  load(rn[1])
  expect_true(grepl("T00:00:00", M$metashort$timestamp[1]))
  expect_true(grepl("T00:00:00", M$metalong$timestamp[1]))
  #-------------------------
  # part 2 data contains 2 complete days
  rn = "output_test/results/part2_daysummary.csv"
  out2 = read.csv(rn)
  expect_equal(nrow(out2), 2)
  expect_true(all(out2$N.hours == 24))
  
  #--------------------------------------------
  # part 5 data contains 2 complete days
  rn = "output_test/results/part5_daysummary_MM_L40M100V400_T5A5.csv"
  out5 = read.csv(rn)
  expect_equal(nrow(out5), 2)
  expect_true(all(out5$dur_day_spt_min == 1440))
  
  if (dir.exists(dn))  unlink(dn, recursive = TRUE)
  if (file.exists(sleeplog_fn)) file.remove(sleeplog_fn)
  if (file.exists(fn)) file.remove(fn)
})