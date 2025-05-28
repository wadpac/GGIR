library(GGIR)
context("g.part3_correct_guider")
test_that("Guiders can be correct in part 3", {
  skip_on_cran()
  
  # create dummy test data
  desiredtz = "Europe/London"
  epochSize = 5
  time = seq(as.POSIXct("2025-05-21 15:42:55", tz = desiredtz),
             as.POSIXct("2025-05-23 15:42:55", tz = desiredtz), by = epochSize)
  spt_crude_estimate = rep(0, length(time))
  
  # define classified sleep windows
  SPTE_start = c(22, 26)
  SPTE_end = c(28, 32)
  spt_crude_estimate[which(time >= as.POSIXct("2025-05-21 22:00:00", tz = desiredtz) &
                             time < as.POSIXct("2025-05-22 04:00:00", tz = desiredtz))] = 2
  spt_crude_estimate[which(time >= as.POSIXct("2025-05-23 02:00:00", tz = desiredtz) &
                             time < as.POSIXct("2025-05-23 08:00:00", tz = desiredtz))] = 2
  
  # Define additional sleep windows that are missed by guider algorithm
  spt_crude_estimate[which(time >= as.POSIXct("2025-05-22 05:00:00", tz = desiredtz) &
                             time < as.POSIXct("2025-05-22 06:00:00", tz = desiredtz))] = 1
  spt_crude_estimate[which(time >= as.POSIXct("2025-05-23 00:00:00", tz = desiredtz) &
                             time < as.POSIXct("2025-05-23 01:00:00", tz = desiredtz))] = 1
  output = data.frame(time = POSIXtime2iso8601(time, desiredtz),
                      spt_crude_estimate = spt_crude_estimate)
  SLE = list(output = output,
             SPTE_end = SPTE_end,
             SPTE_start = SPTE_start)
  # The test whether missed sleep windows are correctly identified
  SLE = g.part3_correct_guider(SLE, desiredtz, epochSize)
  expect_equal(SLE$SPTE_start, c(22, 24))
  expect_equal(SLE$SPTE_end, c(29.999, 31.999))
  expect_equal(length(SLE$output), 34561)
})
