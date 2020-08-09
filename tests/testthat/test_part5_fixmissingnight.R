library(GGIR)
context("g.part5.fixmissingnight")
test_that("is able to identify missing night and retrieve sleeplog estimate", {
  nightsummary = data.frame(ID =c(123,123), night = c(1,3),
                            sleeponset = c(22.92083, 26.92083),
                            wakeup = c(35.99583, 31.16667),
                            SptDuration = c(13.075000, 4.245833),
                            sleepparam = c("T5A5", "T5A5"),
                            guider_onset = c(23, 27),
                            guider_wakeup = c(31, 36),
                            guider_SptDuration= c(8, 9),
                            error_onset = c(-0.07916667, -0.07916667),
                            error_wake = c(4.995833, -4.833333),
                            error_dur = c(5.075000, -4.754167),
                            fraction_night_invalid = c(0.04166667, 0.00000000),
                            SleepDurationInSpt = c(12.8750, 4.1375),
                            duration_sib_wakinghours = c(4.95000 , 14.77917),
                            number_sib_sleepperiod = c(25 , 14),
                            number_sib_wakinghours = c(6, 17),
                            duration_sib_wakinghours_atleast15min = c(3.741667, 11.154167),
                            sleeponset_ts =c("22:55:15","02:55:15"),
                            wakeup_ts = c("11:59:45", "07:10:00"),
                            guider_onset_ts = c("23:00:00", "03:00:00"), 
                            guider_wakeup_ts = c("07:00:00", "17:00:00"),
                            page = c(1, 1),
                            daysleeper = c(0, 0),
                            weekday = c("Thursday", "Saturday"),
                            calendar_date = c("23/6/2016", "25/6/2016"),
                            filename = c("123A_testaccfile.csv.RData", "123A_testaccfile.csv.RData"),
                            cleaningcode = c(0, 0),
                            sleeplog_used =c(1, 1),
                            acc_available = c(1, 1),
                            guider = c("sleeplog", "sleeplog"))
  
  create_test_sleeplog_csv()
  fn = "testsleeplogfile.csv"
  expect_true(file.exists(fn))
  SLEEPLOG = g.loadlog(loglocation=fn, coln1=2,colid=1,nnights=7,sleeplogidnum=TRUE)
  
  out = g.part5.fixmissingnight(summarysleep_tmp2 = nightsummary, sleeplog=SLEEPLOG$sleeplog, ID=123)
  expect_equal(round(out$sleeponset[2],digits=4), 3)
  expect_equal(round(out$wakeup[2],digits=4), 17)
  if (file.exists(fn)) file.remove(fn)
})