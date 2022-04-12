library(GGIR)
context("gloadlog")
test_that("gloadlog is able to load different log formats", {
  create_test_sleeplog_csv(Nnights = 7, advanced = TRUE)
  fn = "testsleeplogfile.csv"
  expect_true(file.exists(fn))
  tempdir = "mytestdir"
  if (dir.exists(tempdir) == FALSE) dir.create(tempdir)
  ID = "123A"
  rec_starttime = "2016-06-25T20:20:20+0200"
  save(ID, rec_starttime, file = "mytestdir/dummyms3.RData")
  logs1 = g.loadlog(loglocation = fn, coln1 = 2, colid = 1, nnights = 7, sleeplogidnum = FALSE,
                   sleeplogsep = ",", meta.sleep.folder = tempdir , desiredtz = "")
  expect_equal(nrow(logs1$sleeplog), 7)
  expect_equal(ncol(logs1$sleeplog), 5)
  expect_equal(logs1$sleeplog$night, as.character(1:7))
  expect_equal(nrow(logs1$nonwearlog), 7)
  expect_equal(ncol(logs1$nonwearlog), 4)
  expect_equal(nrow(logs1$naplog), 7)
  expect_equal(ncol(logs1$naplog), 6)
  # start of accelerometer recording 6 days earlier
  rec_starttime = "2016-06-20T20:20:20+0200"
  save(ID, rec_starttime, file = "mytestdir/dummyms3.RData")
  logs2 = g.loadlog(loglocation = fn, coln1 = 2, colid = 1, nnights = 7, sleeplogidnum = FALSE,
                   sleeplogsep = ",", meta.sleep.folder = tempdir , desiredtz = "")
  expect_equal(nrow(logs2$sleeplog), 7)
  expect_equal(ncol(logs2$sleeplog), 5)
  expect_equal(logs2$sleeplog$night, as.character(6:12))
  expect_equal(nrow(logs2$nonwearlog), 7)
  expect_equal(ncol(logs2$nonwearlog), 4)
  expect_equal(nrow(logs2$naplog), 7)
  expect_equal(ncol(logs2$naplog), 6)
  if (dir.exists(tempdir)) unlink(tempdir, recursive = TRUE)
})