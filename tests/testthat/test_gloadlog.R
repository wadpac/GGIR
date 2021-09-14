library(GGIR)
context("gloadlog")
test_that("gloadlog is able to load different log formats", {
  create_test_sleeplog_csv(Nnights=7, advanced=TRUE)
  fn = "testsleeplogfile.csv"
  expect_true(file.exists(fn))
  tempdir = "mytestdir"
  if (dir.exists(tempdir) == FALSE) dir.create(tempdir)
  ID = "123A"
  rec_starttime = "2016-06-25T20:20:20+0200"
  save(ID, rec_starttime, file = "mytestdir/dummyms3.RData")
  logs = g.loadlog(loglocation=fn, coln1=2, colid=1, nnights=7, sleeplogidnum=FALSE,
                   sleeplogsep=",", meta.sleep.folder=tempdir , desiredtz = "")
  
  expect_equal(nrow(logs$sleeplog), 7)
  expect_equal(ncol(logs$sleeplog), 5)
  expect_equal(nrow(logs$nonwearlog), 7)
  expect_equal(ncol(logs$nonwearlog), 4)
  expect_equal(nrow(logs$naplog), 7)
  expect_equal(ncol(logs$naplog), 6)
  if (dir.exists(tempdir)) unlink(tempdir, recursive=TRUE)
})