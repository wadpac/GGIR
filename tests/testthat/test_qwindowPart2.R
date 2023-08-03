library(GGIR)
context("qwindow")
test_that("qwindow implementation in part 2", {
  skip_on_cran()
  
  Ndays = 2
  create_test_acc_csv(Nmin = Ndays*1440)
  fn = "123A_testaccfile.csv"
  actlog = data.frame(id = c("123A", "124A"),
                      date = c("23/06/2016", "23/06/2016"),
                      work = c("7:45:30", "7:45:00"),
                      travelhome = c("17:00:00", "17:00:00"),
                      home = c("17:30:00", "18:00:00"),
                      date = c("24/06/2016", "24/06/2016"),
                      work = c("7:45:15", "7:45:00"),
                      travelhome = c("17:00:00", "17:00:00"),
                      home = c("17:30:00", "18:00:00"),
                      sport = c("19:00:00", "19:00:00"),
                      home = c("20:00:00", "20:00:00"))
  actlog_fn = "testactlog.csv"
  write.csv(x = actlog, file = actlog_fn, row.names = FALSE)
  
  #--------------------------------------------
  # tests
  
  # part 2
  GGIR(mode = 1:2, datadir = fn, outputdir = getwd(), studyname = "test",
       idloc = 2,
       qwindow = actlog_fn, qwindow_dateformat = "%d/%m/%Y",
       do.report = 2, visualreport = FALSE, verbose = FALSE)
  
  # long format generated
  long_fn = paste(getwd(), "output_test", "results",
                  "part2_daysummary_longformat.csv", sep = .Platform$file.sep)
  expect_true(file.exists(long_fn))
  out = read.csv(long_fn)
  expect_true("qwindow_name" %in% colnames(out))
  expect_true("daystart-dayend" %in% out$qwindow_name)
  expect_true("sport-home" %in% out$qwindow_name)
  expect_true("work-travelhome" %in% out$qwindow_name)

  # remove generated files
  if (file.exists(fn)) file.remove(fn)
  if (file.exists(actlog_fn)) file.remove(actlog_fn)
  if (dir.exists(file.path(getwd(), "output_test"))) unlink(file.path(getwd(), "output_test"), recursive = TRUE)
})
