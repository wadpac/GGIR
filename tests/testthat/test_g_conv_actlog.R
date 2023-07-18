library(GGIR)
context("Convert activity log")
test_that("g.conv.actlog loads activity log and puts it in data.frame", {
  
  # With slash as separator
  actlog = data.frame(id = c("1RAW", "2RAW"),
                      date = c("15/08/2022", "20/08/2022"),
                      work = c("7:45:30", "7:45:00"),
                      travelhome = c("17:00:00", "17:00:00"),
                      home = c("17:30:00", "18:00:00"),
                      date = c("16/08/2022", "21/08/2022"),
                      work = c("7:45:15", "7:45:00"),
                      travelhome = c("17:00:00", "17:00:00"),
                      home = c("17:30:00", "18:00:00"),
                      sport = c("", "19:00:00"),
                      home = c("", "20:00:00"))
  fn = "testactlog.csv"
  write.csv(x = actlog, file = fn, row.names = FALSE)
  LOG = g.conv.actlog(qwindow = fn, qwindow_dateformat = "%d/%m/%Y")
  
  expect_true(is.list(LOG$qwindow_times))
  expect_true(is.list(LOG$qwindow_values))
  expect_true(is.list(LOG$qwindow_names))
  expect_equal(length(LOG$qwindow_times[[1]]), 5)
  expect_equal(length(LOG$qwindow_times[[4]]), 7)
  expect_equal(length(LOG$qwindow_values[[1]]), 5)
  expect_equal(length(LOG$qwindow_values[[2]]), 5)
  expect_equal(length(LOG$qwindow_names[[3]]), 5)
  expect_equal(length(LOG$qwindow_names[[4]]), 7)
  expect_equal(LOG$qwindow_names[[3]], c("daystart", "work", "travelhome", "home", "dayend"))
  expect_equal(LOG$qwindow_values[[2]], c(0.00, 7.754167, 17.00, 17.50, 24.00), tolerance = 3)
  expect_equal(as.character(LOG$qwindow_times[[1]]), c("00:00", "7:45:30", "17:00:00", "17:30:00", "24:00:00"))
  
  # make sure it scales seconds when no compatible with epoch size
  actlog = data.frame(id = c("1RAW", "2RAW"),
                      date = c("15/08/2022", "20/08/2022"),
                      work = c("7:45:32", "7:45:14"),
                      travelhome = c("17:00:04", "17:00:44"),
                      home = c("17:30:31", "18:00:03"),
                      date = c("16/08/2022", "21/08/2022"),
                      work = c("7:45:14", "7:45:34"),
                      travelhome = c("17:00:12", "17:00:53"),
                      home = c("17:30:22", "18:00:34"),
                      sport = c("", "19:00:00"),
                      home = c("", "20:00:00"))
  fn = "testactlog.csv"
  write.csv(x = actlog, file = fn, row.names = FALSE)
  LOG = g.conv.actlog(qwindow = fn, qwindow_dateformat = "%d/%m/%Y", epochSize = 5)
  seconds = lapply(LOG$qwindow_times, FUN = function(x) substr(x, nchar(x), nchar(x)))
  seconds = as.numeric(unlist(seconds))
  expect_true(sum(seconds %in% c(0, 5)) == length(seconds))
  expect_equal(as.character(LOG$qwindow_times[[1]]), c("00:00", "07:45:35", "17:00:05", "17:30:35", "24:00:00"))
  
  expect_true(file.exists(fn))
  if (file.exists(fn)) file.remove(fn)
  
  # With hyphen as separator
  actlog = data.frame(id = c("1RAW", "2RAW"),
                      date = c("15-08-2022", "20-08-2022"),
                      work = c("7:45:00", "7:45:00"),
                      travelhome = c("17:00:00", "17:00:00"),
                      home = c("17:30:00", "18:00:00"),
                      date = c("16-08-2022", "21-08-2022"),
                      work = c("7:45:00", "7:45:00"),
                      travelhome = c("17:00:00", "17:00:00"),
                      home = c("17:30:00", "18:00:00"),
                      sport = c("", "19:00:00"),
                      home = c("", "20:00:00"))
  fn = "testactlog.csv"
  write.csv(x = actlog, file = fn, row.names = FALSE)
  LOG = g.conv.actlog(qwindow = fn, qwindow_dateformat = "%d-%m-%Y")
  
  expect_true(is.list(LOG$qwindow_times))
  expect_true(is.list(LOG$qwindow_values))
  expect_true(is.list(LOG$qwindow_names))
  expect_equal(length(LOG$qwindow_times[[1]]), 5)
  expect_equal(length(LOG$qwindow_times[[4]]), 7)
  expect_equal(length(LOG$qwindow_values[[1]]), 5)
  expect_equal(length(LOG$qwindow_values[[2]]), 5)
  expect_equal(length(LOG$qwindow_names[[3]]), 5)
  expect_equal(length(LOG$qwindow_names[[4]]), 7)
  expect_equal(LOG$qwindow_names[[3]], c("daystart", "work", "travelhome", "home", "dayend"))
  expect_equal(LOG$qwindow_values[[2]], c(0.00, 7.75, 17.00, 17.50, 24.00))
  expect_equal(as.character(LOG$qwindow_times[[1]]), c("00:00", "7:45:00", "17:00:00", "17:30:00", "24:00:00"))
  
  expect_true(file.exists(fn))
  if (file.exists(fn)) file.remove(fn)
})
