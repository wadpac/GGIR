library(GGIR)
context("Recording starts at midnight")
test_that("Test recordings that start at midnight", {
  skip_on_cran()

  Ndays = 2
  create_test_acc_csv(Nmin = Ndays*1440, starts_at_midnight = TRUE)
  outputdir = getwd()
  fn = "123A_testaccfile.csv"
  sleeplog_fn = "testsleeplogfile.csv"
  desiredtz = "Europe/London"
  dn = "output_test"
  minimumFileSizeMB = 0
  if (file.exists(dn))  unlink(dn, recursive = TRUE)
  for (advanced_sleeplog in c(FALSE, TRUE)) {
    create_test_sleeplog_csv(advanced = advanced_sleeplog, start_date = "2016/06/21")
    if (advanced_sleeplog == FALSE) {
      mode = 1:5
    } else {
      mode = 4
    }
    #--------------------------------------------
    # run GGIR
    GGIR(datadir = fn, outputdir = outputdir, studyname = "test", idloc = 2,
         verbose = FALSE, desiredtz = desiredtz, mode = mode, overwrite = TRUE,
         loglocation = sleeplog_fn, colid = 1, coln1 = 2, 
         nnights = 7, timewindow = "MM")
    if (advanced_sleeplog == FALSE) {
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
    }
    #--------------------------------------------
    # part 4 data contains correct night numbers
    rn = "output_test/results/QC/part4_nightsummary_sleep_full.csv"
    out4 = read.csv(rn)
    expect_equal(nrow(out4), 2)
    expect_equal(out4$guider, rep("sleeplog", 2))
    expect_equal(out4$night, 1:2)
    #--------------------------------------------
    # part 5 data contains 2 complete days
    rn = "output_test/results/part5_daysummary_MM_L40M100V400_T5A5.csv"
    out5 = read.csv(rn)
    expect_equal(nrow(out5), 2)
    expect_true(all(out5$dur_day_spt_min == 1440))
  }
  if (file.exists(dn))  unlink(dn, recursive = TRUE)
  if (file.exists(fn)) unlink(fn)
  if (file.exists(sleeplog_fn)) file.remove(sleeplog_fn)
})
