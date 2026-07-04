library(GGIR)
context("check_log")

test_that("check_log coerces fread IDate date columns to character", {
  # data.table::fread() auto-parses ISO-format dates as IDate when reading a
  # study dates file, which previously broke the text-based date checks.
  log = data.frame(ID = "123A",
                   start = data.table::as.IDate("2016-06-24"),
                   end = data.table::as.IDate("2016-06-25"),
                   stringsAsFactors = FALSE)
  expect_s3_class(log$start, "IDate")

  out = check_log(log, dateformat = "%Y-%m-%d", colid = 1, datecols = 2:3,
                  logPath = "study_dates_file.csv", logtype = "study dates log")

  expect_type(out$start, "character")
  expect_type(out$end, "character")
  expect_equal(out$start, "2016-06-24")
  expect_equal(out$end, "2016-06-25")
})
