library(GGIR)
context("g.part5.definedays")
test_that("g.part5.definedays considers qwindow to generate segments", {
  skip_on_cran()
  
  # set windows
  timestamp = seq.POSIXt(from = as.POSIXct("2022-01-01 00:00:00"), 
                         to = as.POSIXct("2022-01-04 23:59:00"), by = 60)
  ts = data.frame(timestamp = timestamp,
                  ACC = 0)
  nightsi = grep("00:00:00", format(ts$timestamp))
  qwindow = c(7, 18)
  
  definedays = g.part5.definedays(nightsi = nightsi, wi = 1, indjump = 1, 
                                  nightsi_bu = nightsi, epochSize = 60, 
                                  qqq_backup=c(), 
                                  ts = ts, 
                                  timewindowi = "MM", Nwindows = 1, 
                                  qwindow = qwindow, ID = NULL)
  
  expect_false(is.null(definedays$segments))
  expect_true(is.list(definedays$segments))
  expect_equal(length(definedays$segments), 4)
  expect_equal(names(definedays$segments)[1], "00:00:00-23:59:00")
  expect_equal(definedays$segments[[1]], c(1, 1440))
  expect_equal(names(definedays$segments)[2], "00:00:00-06:59:00")
  expect_equal(definedays$segments[[2]], c(1, 420))
  expect_equal(names(definedays$segments)[3], "07:00:00-17:59:00")
  expect_equal(definedays$segments[[3]], c(421, 1080))
  expect_equal(names(definedays$segments)[4], "18:00:00-23:59:00")
  expect_equal(definedays$segments[[4]], c(1081, 1440))
  expect_equal(definedays$segments_names, c("MM", "segment1", "segment2", "segment3"))
  
  # With slash as separator
  actlog = data.frame(id = c("1RAW"),
                      date = c("01/01/2022"),
                      work = c("7:45:00"),
                      travelhome = c("17:00:00"),
                      home = c("17:30:00"))
  fn = "testactlog.csv"
  write.csv(x = actlog, file = fn, row.names = FALSE)
  LOG = g.conv.actlog(qwindow = fn, qwindow_dateformat = "%d/%m/%Y")
  
  # definedays
  definedays = g.part5.definedays(nightsi = nightsi, wi = 1, indjump = 1, 
                                  nightsi_bu = nightsi, epochSize = 60, 
                                  qqq_backup=c(), 
                                  ts = ts, timewindowi = "MM", Nwindows = 1, 
                                  qwindow = LOG, ID = "1RAW")
  
  expect_false(is.null(definedays$segments))
  expect_true(is.list(definedays$segments))
  expect_equal(length(definedays$segments), 5)
  expect_equal(names(definedays$segments)[1], "00:00:00-23:59:00")
  expect_equal(definedays$segments[[1]], c(1, 1440))
  expect_equal(names(definedays$segments)[2], "00:00:00-07:44:00")
  expect_equal(definedays$segments[[2]], c(1, 465))
  expect_equal(names(definedays$segments)[3], "07:45:00-16:59:00")
  expect_equal(definedays$segments[[3]], c(466, 1020))
  expect_equal(names(definedays$segments)[4], "17:00:00-17:29:00")
  expect_equal(definedays$segments[[4]], c(1021, 1050))
  expect_equal(names(definedays$segments)[5], "17:30:00-23:59:00")
  expect_equal(definedays$segments[[5]], c(1051, 1440))
  expect_equal(definedays$segments_names, c("MM", "daystart-work", 
                                            "work-travelhome", 
                                            "travelhome-home", "home-dayend"))
  
  # With times not multiple of epoch size and date format %Y-%m-%d
  actlog = data.frame(id = c("1RAW"),
                      date = c("2022-01-01"),
                      work = c("08:09:59"),
                      travelhome = c("16:30:43"),
                      home = c("17:30:00"))
  fn = "testactlog.csv"
  write.csv(x = actlog, file = fn, row.names = FALSE)
  LOG = g.conv.actlog(qwindow = fn, qwindow_dateformat = "%Y-%m-%d")
  
  # definedays
  definedays = g.part5.definedays(nightsi = nightsi, wi = 1, indjump = 1, 
                                  nightsi_bu = nightsi, epochSize = 60, 
                                  qqq_backup=c(), 
                                  ts = ts, timewindowi = "MM", Nwindows = 1, 
                                  qwindow = LOG, ID = "1RAW")
  
  expect_false(is.null(definedays$segments))
  expect_true(is.list(definedays$segments))
  expect_equal(length(definedays$segments), 5)
  expect_equal(names(definedays$segments)[1], "00:00:00-23:59:00")
  expect_equal(definedays$segments[[1]], c(1, 1440))
  expect_equal(names(definedays$segments)[2], "00:00:00-08:09:00")
  expect_equal(definedays$segments[[2]], c(1, 490))
  expect_equal(names(definedays$segments)[3], "08:10:00-16:30:00")
  expect_equal(definedays$segments[[3]], c(491, 991))
  expect_equal(names(definedays$segments)[4], "16:31:00-17:29:00")
  expect_equal(definedays$segments[[4]], c(992, 1050))
  expect_equal(names(definedays$segments)[5], "17:30:00-23:59:00")
  expect_equal(definedays$segments[[5]], c(1051, 1440))
  expect_equal(definedays$segments_names, c("MM", "daystart-work", 
                                            "work-travelhome", 
                                            "travelhome-home", "home-dayend"))
  
  expect_true(file.exists(fn))
  if (file.exists(fn)) file.remove(fn)
  
})



