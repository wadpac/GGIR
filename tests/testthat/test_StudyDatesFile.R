library(GGIR)
context("Chainof5parts")
test_that("chainof5parts", {
  skip_on_cran()
  
  Ndays = 3
  create_test_acc_csv(Nmin = Ndays*1440, start_time = "09:00:00")
  fn = "123A_testaccfile.csv"
  desiredtz = "Europe/London"
  dn = "output_test"
  do.parallel = FALSE
  if (file.exists(dn))  unlink(dn, recursive = TRUE)
  minimumFileSizeMB = 0
  #--------------------------------------------
  # create study dates file
  studydates = data.frame(ID = "123A",
                          start = "24-06-2016", # intentional wrong format (hyphen) to force tests on check_log
                          end = "25/06/2016")
  
  studydates = expect_warning(check_log(studydates, dateformat = "%d/%m/%Y", colid = 1, datecols = 2:3,
                                        logPath = "study_dates_file.csv", logtype = "study dates log"),
                              regexp = "You may be mixing slash and hyphen")
  
  write.csv(studydates, "study_dates_file.csv", row.names = FALSE)
  
  
  # PART 1 ------------------------------------------------------------------
  GGIR(datadir = fn, outputdir = getwd(), studyname = "test", mode = 1, verbose = FALSE,
       overwrite = TRUE,
       idloc = 2, desiredtz = desiredtz)
  
  # PART 2 (tests) ----------------------------------------------------------
  
  # Strategy 1 ----
  GGIR(datadir = fn, outputdir = getwd(), studyname = "test", mode = 2, verbose = FALSE,
       # study dates file
       study_dates_file = "study_dates_file.csv",
       study_dates_dateformat = "%d/%m/%Y",
       # strategy
       data_masking_strategy = 1, hrs.del.start = 0,hrs.del.end = 0)
  # load data
  load(dir("output_test/meta/ms2.out/",full.names = TRUE)[1])
  # removed time before 24/06/2016 and after  25/06/2016
  expect_equal(rle(IMP$rout$r4)$lengths[1], 60)
  expect_equal(rle(IMP$rout$r4)$lengths[3], 36)
  # check the total time included is exactly 2 days (= 192 long epochs)
  first_epoch_in_protocol = rle(IMP$rout$r4)$lengths[1] + 1
  last_epoch_in_protocol = max(which(IMP$rout$r4 == 0))
  expect_equal(last_epoch_in_protocol - first_epoch_in_protocol + 1,  192)
  
  GGIR(datadir = fn, outputdir = getwd(), studyname = "test", mode = 2, verbose = FALSE,
       # study dates file
       study_dates_file = "study_dates_file.csv",
       study_dates_dateformat = "%d/%m/%Y",
       # strategy
       data_masking_strategy = 1, hrs.del.start = 4, hrs.del.end = 4)
  # load data
  load(dir("output_test/meta/basic/",full.names = TRUE)[1])
  load(dir("output_test/meta/ms2.out/",full.names = TRUE)[1])
  # first epoch in protocol 24/06/2016 04:00:00
  first_epoch_in_protocol = rle(IMP$rout$r4)$lengths[1] + 1
  expect_equal(M$metalong$timestamp[first_epoch_in_protocol], "2016-06-24T04:00:00+0100")
  # last epoch in protocol 25/06/2016 19:45:00
  last_epoch_in_protocol = max(which(IMP$rout$r4 == 0))
  expect_equal(M$metalong$timestamp[last_epoch_in_protocol], "2016-06-25T19:45:00+0100")
  
  # Strategy 2 ----
  GGIR(datadir = fn, outputdir = getwd(), studyname = "test", mode = 2, verbose = FALSE,
       # study dates file
       study_dates_file = "study_dates_file.csv",
       study_dates_dateformat = "%d/%m/%Y",
       # strategy
       data_masking_strategy = 2, hrs.del.start = 0,hrs.del.end = 0)
  # load data
  load(dir("output_test/meta/basic/",full.names = TRUE)[1])
  load(dir("output_test/meta/ms2.out/",full.names = TRUE)[1])
  # first epoch in protocol 24/06/2016 00:00:00
  first_epoch_in_protocol = rle(IMP$rout$r4)$lengths[1] + 1
  expect_equal(M$metalong$timestamp[first_epoch_in_protocol], "2016-06-24T00:00:00+0100")
  # last epoch in protocol 25/06/2016 23:45:00
  last_epoch_in_protocol = max(which(IMP$rout$r4 == 0))
  expect_equal(M$metalong$timestamp[last_epoch_in_protocol], "2016-06-25T23:45:00+0100")
  
  # Strategy 3 ----
  GGIR(datadir = fn, outputdir = getwd(), studyname = "test", mode = 2, verbose = FALSE,
       # study dates file
       study_dates_file = "study_dates_file.csv",
       study_dates_dateformat = "%d/%m/%Y",
       # strategy
       data_masking_strategy = 3, ndayswindow = 1,
       hrs.del.start = 0,hrs.del.end = 0)
  # load data
  load(dir("output_test/meta/basic/",full.names = TRUE)[1])
  load(dir("output_test/meta/ms2.out/",full.names = TRUE)[1])
  # first and last epochs are within 24/06/2016 and 25/06/2016
  first_epoch_in_protocol = rle(IMP$rout$r4)$lengths[1] + 1
  t0 = iso8601chartime2POSIX(M$metalong$timestamp[first_epoch_in_protocol], tz = "Europe/London")
  last_epoch_in_protocol = max(which(IMP$rout$r4 == 0))
  t1 = iso8601chartime2POSIX(M$metalong$timestamp[last_epoch_in_protocol], tz = "Europe/London")
  expect_true(t0 == as.POSIXct("2016-06-24 14:45:00", tz = "Europe/London"))
  expect_true(t1 == as.POSIXct("2016-06-25 14:30:00", tz = "Europe/London"))
  # check the total time included is exactly 1 day (= 96 long epochs)
  expect_equal(last_epoch_in_protocol - first_epoch_in_protocol + 1,  96)
  
  GGIR(datadir = fn, outputdir = getwd(), studyname = "test", mode = 2, verbose = FALSE,
       # study dates file
       study_dates_file = "study_dates_file.csv",
       study_dates_dateformat = "%d/%m/%Y",
       # strategy
       data_masking_strategy = 3, ndayswindow = 1,
       hrs.del.start = 6,hrs.del.end = 6)
  # load data
  load(dir("output_test/meta/basic/",full.names = TRUE)[1])
  load(dir("output_test/meta/ms2.out/",full.names = TRUE)[1])
  # first and last epochs are within 24/06/2016 and 25/06/2016
  first_epoch_in_protocol = rle(IMP$rout$r4)$lengths[1] + 1
  t0 = iso8601chartime2POSIX(M$metalong$timestamp[first_epoch_in_protocol], tz = "Europe/London")
  last_epoch_in_protocol = max(which(IMP$rout$r4 == 0))
  t1 = iso8601chartime2POSIX(M$metalong$timestamp[last_epoch_in_protocol], tz = "Europe/London")
  expect_true(t0 >= as.POSIXct("2016-06-24 00:00:00", tz = "Europe/London"))
  expect_true(t1 < as.POSIXct("2016-06-26 00:00:00", tz = "Europe/London"))
  # check the total time included is exactly 0.5 days (= 48 long epochs)
  expect_equal(last_epoch_in_protocol - first_epoch_in_protocol + 1,  48)
  
  # Strategy 4 ----
  GGIR(datadir = fn, outputdir = getwd(), studyname = "test", mode = 2, verbose = FALSE,
       # study dates file
       study_dates_file = "study_dates_file.csv",
       study_dates_dateformat = "%d/%m/%Y",
       # strategy
       data_masking_strategy = 4, ndayswindow = 7,
       hrs.del.start = 0,hrs.del.end = 0)
  # load data
  load(dir("output_test/meta/basic/",full.names = TRUE)[1])
  load(dir("output_test/meta/ms2.out/",full.names = TRUE)[1])
  # first epoch in protocol 24/06/2016 00:00:00
  first_epoch_in_protocol = rle(IMP$rout$r4)$lengths[1] + 1
  expect_equal(M$metalong$timestamp[first_epoch_in_protocol], "2016-06-24T00:00:00+0100")
  # last epoch in protocol 25/06/2016 23:45:00
  last_epoch_in_protocol = max(which(IMP$rout$r4 == 0))
  expect_equal(M$metalong$timestamp[last_epoch_in_protocol], "2016-06-25T23:45:00+0100")
  
  # Strategy 5 -----
  GGIR(datadir = fn, outputdir = getwd(), studyname = "test", mode = 2, verbose = FALSE,
       # study dates file
       study_dates_file = "study_dates_file.csv",
       study_dates_dateformat = "%d/%m/%Y",
       # strategy
       data_masking_strategy = 5, ndayswindow = 1,
       hrs.del.start = 0,hrs.del.end = 0)
  # load data
  load(dir("output_test/meta/basic/",full.names = TRUE)[1])
  load(dir("output_test/meta/ms2.out/",full.names = TRUE)[1])
  # the day most active in this recording is 25/06/2016
  first_epoch_in_protocol = rle(IMP$rout$r4)$lengths[1] + 1
  t0 = iso8601chartime2POSIX(M$metalong$timestamp[first_epoch_in_protocol], tz = "Europe/London")
  last_epoch_in_protocol = max(which(IMP$rout$r4 == 0))
  t1 = iso8601chartime2POSIX(M$metalong$timestamp[last_epoch_in_protocol], tz = "Europe/London")
  expect_true(t0 == as.POSIXct("2016-06-25 00:00:00", tz = "Europe/London"))
  expect_true(t1 == as.POSIXct("2016-06-25 23:45:00", tz = "Europe/London"))
  # check the total time included is exactly 1 day (= 96 long epochs)
  expect_equal(last_epoch_in_protocol - first_epoch_in_protocol + 1,  96)
  
  GGIR(datadir = fn, outputdir = getwd(), studyname = "test", mode = 2, verbose = FALSE,
       # study dates file
       study_dates_file = "study_dates_file.csv",
       study_dates_dateformat = "%d/%m/%Y",
       # strategy
       data_masking_strategy = 5, ndayswindow = 1,
       hrs.del.start = 5,hrs.del.end = 5)
  # load data
  load(dir("output_test/meta/basic/",full.names = TRUE)[1])
  load(dir("output_test/meta/ms2.out/",full.names = TRUE)[1])
  # the day most active in this recording is 25/06/2016
  first_epoch_in_protocol = rle(IMP$rout$r4)$lengths[1] + 1
  t0 = iso8601chartime2POSIX(M$metalong$timestamp[first_epoch_in_protocol], tz = "Europe/London")
  last_epoch_in_protocol = max(which(IMP$rout$r4 == 0))
  t1 = iso8601chartime2POSIX(M$metalong$timestamp[last_epoch_in_protocol], tz = "Europe/London")
  expect_true(t0 == as.POSIXct("2016-06-25 05:00:00", tz = "Europe/London"))
  expect_true(t1 == as.POSIXct("2016-06-25 18:45:00", tz = "Europe/London"))
  
  # start date out of recorded dates (warning and do not trim) -----
  # studydates = data.frame(ID = "123A",
  #                         start = "18/06/2016", # intentional wrong format (hyphen) to force tests on check_log
  #                         end = "25/06/2016")
  # write.csv(studydates, "study_dates_file.csv", row.names = FALSE)
  # expect_warning(
  #   GGIR(datadir = fn, outputdir = getwd(), studyname = "test", mode = 2, verbose = FALSE,
  #        # study dates file
  #        study_dates_file = "study_dates_file.csv", 
  #        study_dates_dateformat = "%d/%m/%Y",
  #        # strategy
  #        data_masking_strategy = 1, ndayswindow = 7,
  #        hrs.del.start = 0,hrs.del.end = 0),
  #   regexp = "The start date")
  
  # end date out of recorded dates (warning and do not trim) -----
  # studydates = data.frame(ID = "123A",
  #                         start = "24/06/2016", # intentional wrong format (hyphen) to force tests on check_log
  #                         end = "26/06/2016")
  # write.csv(studydates, "study_dates_file.csv", row.names = FALSE)
  # expect_warning(
  #   GGIR(datadir = fn, outputdir = getwd(), studyname = "test", mode = 2, verbose = FALSE,
  #        # study dates file
  #        study_dates_file = "study_dates_file.csv", 
  #        study_dates_dateformat = "%d/%m/%Y",
  #        # strategy
  #        data_masking_strategy = 1, ndayswindow = 7,
  #        hrs.del.start = 0,hrs.del.end = 0),
  #   regexp = "The end date")
  
  # ID not in study dates file (warning and do not trim) -----
  studydates = data.frame(ID = "dummyID",
                          start = "24/06/2016", # intentional wrong format (hyphen) to force tests on check_log
                          end = "25/06/2016")
  write.csv(studydates, "study_dates_file.csv", row.names = FALSE)
  expect_warning(
    GGIR(datadir = fn, outputdir = getwd(), studyname = "test", mode = 2, verbose = FALSE,
         # study dates file
         study_dates_file = "study_dates_file.csv", 
         study_dates_dateformat = "%d/%m/%Y",
         # strategy
         data_masking_strategy = 1, ndayswindow = 7,
         hrs.del.start = 0,hrs.del.end = 0),
    regexp = "The ID 123A does not appear")
  
  if (file.exists("study_dates_file.csv")) file.remove("study_dates_file.csv")
  if (dir.exists(dn))  unlink(dn, recursive = TRUE)
  if (file.exists(fn)) unlink(fn)
})
