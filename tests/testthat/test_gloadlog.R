library(GGIR)
context("gloadlog")
test_that("gloadlog is able to load different log formats", {
  tempdir = "mytestdir"
  if (dir.exists(tempdir) == FALSE) dir.create(tempdir)
  ID = "123A"

  # test that a sleep log with any separator from the set of [,\t |;] can be successfully parsed
  for (sep in c(",", "\t", " ", "|", ";")) {
    create_test_sleeplog_csv(Nnights = 7, advanced = TRUE, sep = sep)
    fn = "testsleeplogfile.csv"
    expect_true(file.exists(fn))

    rec_starttime = format(as.POSIXct("2016-06-25 20:20:20"), "%Y-%m-%dT%H:%M:%S%z")
    save(ID, rec_starttime, file = "mytestdir/dummyms3.RData")
    cat(paste0("\nLocal timezone: ", Sys.timezone()))
    logs1 = g.loadlog(loglocation = fn, coln1 = 2, colid = 1,
                      meta.sleep.folder = tempdir , desiredtz = "")
    expect_equal(nrow(logs1$sleeplog), 7) 
    expect_equal(ncol(logs1$sleeplog), 5)
    expect_equal(logs1$sleeplog$night, as.character(1:7))
    expect_equal(logs1$sleeplog$sleepwake, c("7:0:2", "7:0:3", "7:0:4", "7:0:5",
                                             "7:0:6", "7:0:7", "7:0:8"))
    expect_equal(nrow(logs1$nonwearlog), 8)
    expect_equal(ncol(logs1$nonwearlog), 4)
    expect_equal(nrow(logs1$naplog), 8)
    expect_equal(ncol(logs1$naplog), 6)
    
    # start of accelerometer recording 6 days earlier
    rec_starttime = format(as.POSIXct("2016-06-20 20:20:20"), "%Y-%m-%dT%H:%M:%S%z")
    save(ID, rec_starttime, file = "mytestdir/dummyms3.RData")
    logs2 = g.loadlog(loglocation = fn, coln1 = 2, colid = 1,
                     meta.sleep.folder = tempdir , desiredtz = "")
    expect_equal(nrow(logs2$sleeplog), 7)
    expect_equal(ncol(logs2$sleeplog), 5)
    expect_equal(logs2$sleeplog$night, as.character(6:12)) # these are nights in the acc recording
    expect_equal(logs2$sleeplog$sleepwake, c("7:0:2", "7:0:3", "7:0:4", "7:0:5",
                                             "7:0:6", "7:0:7", "7:0:8"))
    expect_equal(nrow(logs2$nonwearlog), 8)
    expect_equal(ncol(logs2$nonwearlog), 4)
    expect_equal(nrow(logs2$naplog), 8)
    expect_equal(ncol(logs2$naplog), 6)
    
    # start of accelerometer recording 3 days later
    rec_starttime = format(as.POSIXct("2016-06-29 20:20:20"), "%Y-%m-%dT%H:%M:%S%z")
    save(ID, rec_starttime, file = "mytestdir/dummyms3.RData")
    logs3 = g.loadlog(loglocation = fn, coln1 = 2, colid = 1,
                      meta.sleep.folder = tempdir , desiredtz = "")
    expect_equal(nrow(logs3$sleeplog), 3)
    expect_equal(ncol(logs3$sleeplog), 5)
    expect_equal(logs3$sleeplog$night, as.character(1:3)) # 1:3 because these are the first three nights of the acc recording
    expect_equal(logs3$sleeplog$sleeponset, c("23:0:5", "23:0:6", "23:0:7"))
    expect_equal(nrow(logs3$nonwearlog), 4)
    expect_equal(ncol(logs3$nonwearlog), 4)
    expect_equal(nrow(logs3$naplog), 4)
    expect_equal(ncol(logs3$naplog), 6)
  }
  #============================================================
  # Imputation of sleeplog data
  
  # Test imputation for basic sleeplog with only onset missing for first night:
  create_test_sleeplog_csv(Nnights = 7, advanced = FALSE, sep = ",", missingOnset = TRUE)
  fn = "testsleeplogfile.csv"
  expect_true(file.exists(fn))
  rec_starttime = format(as.POSIXct("2016-06-25 20:20:20"), "%Y-%m-%dT%H:%M:%S%z")
  save(ID, rec_starttime, file = "mytestdir/dummyms3.RData")
  cat(paste0("\nLocal timezone: ", Sys.timezone()))
  logs4 = g.loadlog(loglocation = fn, coln1 = 2, colid = 1,
                    meta.sleep.folder = tempdir , desiredtz = "",
                    impute_sleeplog_onsetwake = TRUE)
  expect_equal(nrow(logs4$sleeplog), 7) 
  expect_equal(ncol(logs4$sleeplog), 5)
  expect_equal(logs4$sleeplog$night, as.character(1:7))
  expect_equal(logs4$sleeplog$sleepwake, c("07:00:01", "17:0:0", "7:0:3", "7:0:4",
                                           "7:0:5", "7:0:6", "7:0:7"))
  expect_equal(logs4$sleeplog$duration, c("7.99875", "14", "8", "8", "8", "8", "8"))

  
  # Test imputation for basic sleeplog with both onset and wake missing for first night:
  create_test_sleeplog_csv(Nnights = 7, advanced = FALSE, sep = ",",
                           missingOnset = TRUE,  missingWake = TRUE)
  fn = "testsleeplogfile.csv"
  expect_true(file.exists(fn))
  rec_starttime = format(as.POSIXct("2016-06-25 20:20:20"), "%Y-%m-%dT%H:%M:%S%z")
  save(ID, rec_starttime, file = "mytestdir/dummyms3.RData")
  cat(paste0("\nLocal timezone: ", Sys.timezone()))
  logs5 = g.loadlog(loglocation = fn, coln1 = 2, colid = 1,
                    meta.sleep.folder = tempdir , desiredtz = "",
                    impute_sleeplog_onsetwake = TRUE)
  expect_equal(nrow(logs5$sleeplog), 7) 
  expect_equal(ncol(logs5$sleeplog), 5)
  expect_equal(logs5$sleeplog$night, as.character(1:7))
  expect_equal(logs5$sleeplog$sleepwake, c("7:0:4.5", "17:0:0", "7:0:3", "7:0:4",
                                           "7:0:5", "7:0:6", "7:0:7"))
  expect_equal(logs5$sleeplog$sleeponset, c("23:0:5.5", "3:0:0", "23:0:3", "23:0:4",
                                            "23:0:5", "23:0:6", "23:0:7"))
  
  
  # Test imputation for advanced sleeplog with both onset and wake missing for first night::
  create_test_sleeplog_csv(Nnights = 7, advanced = TRUE, sep = ",",
                           missingOnset = TRUE, missingWake = TRUE)
  fn = "testsleeplogfile.csv"
  expect_true(file.exists(fn))
  rec_starttime = format(as.POSIXct("2016-06-25 20:20:20"), "%Y-%m-%dT%H:%M:%S%z")
  save(ID, rec_starttime, file = "mytestdir/dummyms3.RData")
  cat(paste0("\nLocal timezone: ", Sys.timezone()))
  logs6 = g.loadlog(loglocation = fn, coln1 = 2, colid = 1,
                    meta.sleep.folder = tempdir , desiredtz = "",
                    impute_sleeplog_onsetwake = TRUE)
  expect_equal(nrow(logs6$sleeplog), 8) 
  expect_equal(ncol(logs6$sleeplog), 5)
  expect_equal(logs6$sleeplog$night, as.character(1:8))
  expect_equal(logs6$sleeplog$sleepwake, c("07:00:02", "7:0:3", "7:0:4", "7:0:5",
                                           "7:0:6", "7:0:7", "7:0:8", "7:0:5"))
  expect_equal(logs6$sleeplog$sleeponset, c("23:0:5", "23:0:2", "23:0:3", "23:0:4",
                                            "23:0:5", "23:0:6", "23:0:7", "23:00:08"))
  
  #============================================================
  # Handling single night recordings
  
  # Test basic sleeplog with only 1 night:
  create_test_sleeplog_csv(Nnights = 1, advanced = FALSE, sep = ",")
  fn = "testsleeplogfile.csv"
  expect_true(file.exists(fn))
  rec_starttime = format(as.POSIXct("2016-06-25 20:20:20"), "%Y-%m-%dT%H:%M:%S%z")
  save(ID, rec_starttime, file = "mytestdir/dummyms3.RData")
  logs4 = g.loadlog(loglocation = fn, coln1 = 2, colid = 1,
                    meta.sleep.folder = tempdir , desiredtz = "")
  expect_equal(nrow(logs4$sleeplog), 1)
  expect_equal(ncol(logs4$sleeplog), 5)
  expect_equal(logs4$sleeplog$night, "1")
  
  
  # Test advanced sleeplog with only 1 night:
  create_test_sleeplog_csv(Nnights = 1, advanced = TRUE, sep = ",")
  fn = "testsleeplogfile.csv"
  expect_true(file.exists(fn))
  rec_starttime = format(as.POSIXct("2016-06-25 20:20:20"), "%Y-%m-%dT%H:%M:%S%z")
  save(ID, rec_starttime, file = "mytestdir/dummyms3.RData")
  logs5 = g.loadlog(loglocation = fn, coln1 = 2, colid = 1,
                    meta.sleep.folder = tempdir , desiredtz = "")
  expect_equal(nrow(logs5$sleeplog), 1)
  expect_equal(ncol(logs4$sleeplog), 5)
  expect_equal(logs4$sleeplog$night, "1")
  
  if (dir.exists(tempdir)) unlink(tempdir, recursive = TRUE)
  if (file.exists(fn)) unlink(fn, recursive = TRUE)
})
