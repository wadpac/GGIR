library(GGIR)
context("Recording starts at midnight")
test_that("Test recordings that start at midnight", {
  skip_on_cran()
  
  Ndays = 2
  create_test_acc_csv(Nmin = Ndays*1440, starts_at_midnight = TRUE)
  create_test_sleeplog_csv(advanced = TRUE, begin_date = "2016/06/21")
  fn = "123A_testaccfile.csv"
  sleeplog_fn = "testsleeplogfile.csv"
  desiredtz = "Europe/London"
  dn = "output_test"
  if (file.exists(dn))  unlink(dn, recursive = TRUE)
  minimumFileSizeMB = 0
  #--------------------------------------------
  # run GGIR
  GGIR(datadir = fn, outputdir = getwd(), studyname = "test", idloc = 2,
       verbose = FALSE, desiredtz = desiredtz,
       loglocation = sleeplog_fn, colid = 1, coln1 = 2, 
       nnights = 7, timewindow = "MM", visualreport = FALSE)
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
  # part 4 data contains correct night numbers
  rn = "output_test/results/QC/part4_nightsummary_sleep_full.csv"
  out4 = read.csv(rn)
  expect_equal(nrow(out4), 2)
  expect_equal(out4$guider, rep("sleeplog", 2))
  expect_equal(out4$night, 1:2)
  expect_equal(out4$guider_onset_ts, c("23:00:02", "23:00:03"))
  
  #--------------------------------------------
  # part 5 data contains 2 complete days
  rn = "output_test/results/part5_daysummary_MM_L40M100V400_T5A5.csv"
  out5 = read.csv(rn)
  expect_equal(nrow(out5), 2)
  expect_true(all(out5$dur_day_spt_min == 1440))
  
  
  #===============================================
  # Repeat part 4 with Basic sleeplog
  if (file.exists(sleeplog_fn)) file.remove(sleeplog_fn)
  create_test_sleeplog_csv(advanced = FALSE)
  #--------------------------------------------
  # run GGIR
  GGIR(datadir = fn, outputdir = getwd(), studyname = "test", idloc = 2,
       verbose = FALSE, desiredtz = desiredtz,
       loglocation = sleeplog_fn, colid = 1, coln1 = 2, 
       nnights = 7, timewindow = "MM", mode = 4, do.report = 4,
       visualreport = FALSE, overwrite = TRUE)
  
  #--------------------------------------------
  # part 4 data contains correct night numbers
  rn = "output_test/results/QC/part4_nightsummary_sleep_full.csv"
  out4 = read.csv(rn)
  expect_equal(nrow(out4), 2)
  expect_equal(out4$guider, rep("sleeplog", 2))
  expect_equal(out4$night, 1:2)
  expect_equal(out4$guider_onset_ts, c("23:00:00", "03:00:00"))
  
  if (dir.exists(dn))  unlink(dn, recursive = TRUE)
  if (file.exists(fn)) unlink(fn)
  if (file.exists(sleeplog_fn)) file.remove(sleeplog_fn)
})
