library(GGIR)
context("g.part5.fixmissingnight")
test_that("is able to identify missing night and retrieve sleeplog estimate", {
  nightsummary = data.frame(ID = c(123, 123), night = c(1,3),
                            sleeponset = c(22.92083, 26.92083),
                            wakeup = c(35.99583, 31.16667),
                            SptDuration = c(13.075000, 4.245833),
                            sleepparam = c("T5A5", "T5A5"),
                            guider_onset = c(23, 27),
                            guider_wakeup = c(31, 36),
                            guider_SptDuration = c(8, 9), 
                            error_onset = c(-0.07916667, -0.07916667),
                            error_wake = c(4.995833, -4.833333),
                            error_dur = c(5.075000, -4.754167),
                            fraction_night_invalid = c(0.04166667, 0.00000000),
                            SleepDurationInSpt = c(12.8750, 4.1375),
                            duration_sib_wakinghours = c(4.95000 , 14.77917),
                            number_sib_sleepperiod = c(25 , 14),
                            number_sib_wakinghours = c(6, 17),
                            duration_sib_wakinghours_atleast15min = c(3.741667, 11.154167),
                            sleeponset_ts = c("22:55:15", "02:55:15"), 
                            wakeup_ts = c("11:59:45", "07:10:00"),
                            guider_onset_ts = c("23:00:00", "03:00:00"), 
                            guider_wakeup_ts = c("07:00:00", "17:00:00"),
                            page = c(1, 1),
                            daysleeper = c(0, 0),
                            weekday = c("Thursday", "Saturday"),
                            calendar_date = c("23/6/2016", "25/6/2016"),
                            filename = c("123A_testaccfile.csv.RData", "123A_testaccfile.csv.RData"),
                            cleaningcode = c(0, 0),
                            sleeplog_used = c(1, 1), 
                            acc_available = c(1, 1),
                            guider = c("sleeplog", "sleeplog"))
  
  create_test_sleeplog_csv()
  fn = "testsleeplogfile.csv"
  expect_true(file.exists(fn))
  SLEEPLOG = g.loadlog(loglocation = fn, coln1 = 2, colid = 1)
  
  out = g.part5.fixmissingnight(summarysleep = nightsummary,
                                sleeplog = SLEEPLOG$sleeplog,
                                ID = 123)
  expect_equal(round(out$sleeponset[2], digits = 4), 3)
  expect_equal(round(out$wakeup[2], digits = 4), 17)
  
  # also test g.part5.addfirstwake
  N = 54 * 60 * 60 / 5
  t0 = as.POSIXlt("2016-06-22 22:00:00", tz = "Europe/Amsterdam",
                  origin = "1970-01-01")
  # the values of time are not used in the function, we only care about the length the object time
  time = seq(t0, t0 + ((N * 5) - 1), by = 5) 
  time = POSIXtime2iso8601(time, tz = "")
  diur = c(rep(0, 18720), #awake until second midnight (to trigger detection of first wake)
           rep(1, 5040),  #spt until 7 am next day
           rep(0, 12240),
           rep(1, N - 36000))
  detection = rep(0, N)
  set.seed(300)
  detection[which(runif(n = N, min = 0, max = 1) < 0.1)] = 1
  ACC = rnorm(n = N, mean = 0, sd = 40)
  
  ts = data.frame(time = time, ACC = ACC, diur = diur, sibdetection = detection, guider = "unknown")
  nightsi = grep("00:00:00", time)

  ts = g.part5.addfirstwake(ts, summarysleep = nightsummary, 
                            nightsi = nightsi, sleeplog = SLEEPLOG$sleeplog,
                            ID = 123, Nepochsinhour = 720, 
                            SPTE_end = NA)
  
  if (file.exists(fn)) file.remove(fn)
})
