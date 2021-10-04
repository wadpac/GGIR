library(GGIR)
context("CalcSleepRegularityIndex")
test_that("CalcSleepRegularityIndex gives output as expected", {
  # Create dummy data:
  tz = "Europe/Amsterdam"
  time = seq(from = as.POSIXlt(x = "2020-01-01 00:01:10", tz = tz), 
             to = as.POSIXlt(x = "2020-01-06 00:01:10", tz = tz), by = 5)
  time = POSIXtime2iso8601(x = time, tz = tz)
  NID =  12*1440
  sleep = rep(0, length(time))
  R1 = round(NID*(4/10))
  R2 = round(NID*(5/10))
  R3 = round(NID*(8/10))
  R4 = round(NID*(9/10))
  sleep[R1:R3] = 1
  sleep[(NID + R2):(NID + R3)] = 1
  sleep[(NID*2 + R2):(NID*2 + R4)] = 1
  sleep[(NID*3 + R1):(NID*3 + R4)] = 1
  sleep[(NID*4 + R1):(NID*4 + R3)] = 1
  sleep[(NID*5 + R2):(NID*5 + R3)] = 1
  sleep = sleep[1:length(time)]
  testdata = data.frame(time = time, invalid = rep(0, length(time)), sleep = sleep)
  # Use dummy data in function call:
  SRI = CalcSleepRegularityIndex(data = testdata, epochsize = 5, desiredtz = tz)
  expect_equal(nrow(SRI), 5)
  expect_equal(ncol(SRI), 5)
  expect_equal(SRI$SleepRegularityIndex[2], 80)
  expect_equal(SRI$SleepRegularityIndex[3], 80)
  # Scenario 2, perfect regularity
  testdata2 = data.frame(time = time, sleep = rep(0,length(time)))
  SRI2 = CalcSleepRegularityIndex(data = testdata2, epochsize = 5, desiredtz = tz)
  expect_equal(SRI2$SleepRegularityIndex[3], 100)
  expect_equal(SRI2$SleepRegularityIndex[4], 100)
  # Scenario 3, perfect irregularity
  testdata3 = data.frame(time = time, sleep = 1:length(time))
  SRI3 = CalcSleepRegularityIndex(data = testdata3, epochsize = 5, desiredtz = tz)
  expect_equal(SRI3$SleepRegularityIndex[2], -100)
  expect_equal(SRI3$SleepRegularityIndex[3], -100)
})