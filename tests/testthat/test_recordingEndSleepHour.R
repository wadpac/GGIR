library(GGIR)
context("recordingEndSleepHour")
test_that("recordingEndSleepHour works as expected", {
  skip_on_cran()
  #=======================
  create_test_acc_csv(Nmin = 2.5 * 1440) # ends at 20:45
  fn = "123A_testaccfile.csv"
  tz = "Europe/Amsterdam"
  # this should NOT trigger data expansion
  GGIR(mode = 1, datadir = fn, outputdir = getwd(), 
       studyname = "test", overwrite = TRUE, 
       recordingEndSleepHour = 21, desiredtz = tz,
       visualreport = FALSE, do.report = c(), verbose = FALSE)
  rn = dir("output_test/meta/basic/",full.names = TRUE)
  load(rn[1])
  nrow(M$metashort)
  expect_true(nrow(M$metashort) == 43020 )
  # expect_true(M$metashort$timestamp[nrow(M$metashort)] == "2016-06-25T20:44:55+0200")
  
  # errors and warnings work properly
  expect_error( # error from using expand_tail_max_hour instead of new argument
    GGIR(mode = 1, datadir = fn, outputdir = getwd(), 
         studyname = "test", overwrite = TRUE,  desiredtz = tz,
         expand_tail_max_hours = 5, recordingEndSleepHour = NULL,
         visualreport = FALSE, do.report = c(), verbose = FALSE)
  )
  expect_warning( # warning from using both expand_tail_max_hour and recordingEndSleepHour
    GGIR(mode = 1, datadir = fn, outputdir = getwd(), 
         studyname = "test", overwrite = TRUE, desiredtz = tz,
         expand_tail_max_hours = 5, recordingEndSleepHour = 20,
         visualreport = FALSE, do.report = c(), verbose = FALSE)
  )
  
  expect_error( # error from recordingEndSleepHour being too early,
    # this should not produce any output
    GGIR(datadir = fn, outputdir = getwd(),
         studyname = "test", overwrite = TRUE, desiredtz = tz,
         recordingEndSleepHour = 6,
         minimum_MM_length.part5 = 15, verbose = FALSE)
  )
  
  
  # No warning, this should work
  GGIR(datadir = fn, outputdir = getwd(),
         studyname = "test", overwrite = TRUE, desiredtz = tz,
         recordingEndSleepHour = 20,
         minimum_MM_length.part5 = 6, verbose = FALSE)
  rn = dir("output_test/meta/basic/", full.names = TRUE)
  load(rn[1])
  expect_true(nrow(M$metashort) > 43020) # metashort is expanded
  
  # expanded time is not reports
  p2 = read.csv("output_test/results/part2_daysummary.csv")
  expect_equal(p2$N.hours[nrow(p2)], 20.75)  # N hours in last day does not include the expanded time

  p4 = read.csv("output_test/results/part4_nightsummary_sleep_cleaned.csv")
  p4full = read.csv("output_test/results/QC/part4_nightsummary_sleep_full.csv")
  expect_equal(nrow(p4), 2) # Night 3 is not in the part 4 reports
  expect_equal(nrow(p4full), 2) # Night 3 is not in the part 4 reports

  p5 = read.csv("output_test/results/part5_daysummary_MM_L40M100V400_T5A5.csv")
  expect_equal(nrow(p5), 3) # expanded day appears in MM report
  expect_true(p5$dur_day_spt_min[nrow(p5)] < 23*60) # but expanded time is not accounted for in estimates
  # 
  
  if (file.exists("output_test"))  unlink("output_test", recursive = TRUE)
  if (file.exists(fn)) file.remove(fn)
})


