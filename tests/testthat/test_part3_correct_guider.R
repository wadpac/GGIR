library(GGIR)
context("g.part3_correct_guider")
test_that("Guider that misses half of the night can be corrected", {
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
                      spt_crude_estimate = spt_crude_estimate,
                      invalid = rep(0, length(time)),
                      night = rep(1, length(time)),
                      T5A5 = rep(1, length(time)))
  output$night[which(time >= as.POSIXct("2025-05-22 12:00:00", tz = desiredtz) &
                       time < as.POSIXct("2025-05-23 12:00:00", tz = desiredtz))] = 2
  output$night[which(time >= as.POSIXct("2025-05-23 12:00:00", tz = desiredtz))] = 3

  SLE = list(output = output,
             SPTE_end = SPTE_end,
             SPTE_start = SPTE_start)
  # The test whether missed sleep windows are correctly identified
  params_sleep = GGIR::load_params()$params_sleep
  params_sleep[["guider_cor_do"]] = TRUE # to run correction
  params_sleep[["guider_cor_maxgap_hrs "]] = Inf # to use max gap size
  params_sleep[["guider_cor_meme_min_dys"]] = Inf # to not do meme
  params_sleep[["guider_cor_min_frac_sib"]]  = 0.8
  params_sleep[["guider_cor_min_hrs"]]  = 1
  params_sleep[["guider_cor_meme_min_hrs"]]  = 2
  SLE = g.part3_correct_guider(SLE, desiredtz, epochSize,
                               params_sleep = params_sleep)
  expect_equal(SLE$SPTE_start, c(22, 24))
  expect_equal(SLE$SPTE_end, c(29.999, 31.999))
  expect_equal(nrow(SLE$output), 34561)
})


test_that("Guider that focusses on afternoon nap can be corrected", {
  skip_on_cran()
  #=============================================================
  # One night with sleep in the afternoon
  #=============================================================
  # create dummy test data
  desiredtz = "Europe/London"
  epochSize = 5
  time = seq(as.POSIXct("2025-05-21 15:42:55", tz = desiredtz),
             as.POSIXct("2025-05-25 15:42:55", tz = desiredtz), by = epochSize)
  spt_crude_estimate = rep(0, length(time))
  # define classified sleep windows
  SPTE_start = c(13, 22, 26, 22)
  SPTE_end = c(16, 28, 32, 30)
  spt_crude_estimate[which(time >= as.POSIXct("2025-05-21 13:00:00", tz = desiredtz) &
                             time < as.POSIXct("2025-05-21 16:00:00", tz = desiredtz))] = 2
  spt_crude_estimate[which(time >= as.POSIXct("2025-05-22 22:00:00", tz = desiredtz) &
                             time < as.POSIXct("2025-05-23 04:00:00", tz = desiredtz))] = 2
  spt_crude_estimate[which(time >= as.POSIXct("2025-05-24 02:00:00", tz = desiredtz) &
                             time < as.POSIXct("2025-05-24 08:00:00", tz = desiredtz))] = 2
  spt_crude_estimate[which(time >= as.POSIXct("2025-05-24 22:00:00", tz = desiredtz) &
                             time < as.POSIXct("2025-05-25 06:00:00", tz = desiredtz))] = 2
  
  # Define additional sleep windows that are missed by guider algorithm
  
  spt_crude_estimate[which(time >= as.POSIXct("2025-05-22 01:00:00", tz = desiredtz) &
                             time < as.POSIXct("2025-05-22 03:30:00", tz = desiredtz))] = 1
  spt_crude_estimate[which(time >= as.POSIXct("2025-05-22 05:00:00", tz = desiredtz) &
                             time < as.POSIXct("2025-05-22 06:00:00", tz = desiredtz))] = 1
  output = data.frame(time = POSIXtime2iso8601(time, desiredtz),
                      spt_crude_estimate = spt_crude_estimate,
                      invalid = rep(0, length(time)),
                      night = rep(1, length(time)),
                      T5A5 = rep(1, length(time)))
  
  output$night[which(time < as.POSIXct("2025-05-22 12:00:00", tz = desiredtz))] = 1
  output$night[which(time >= as.POSIXct("2025-05-22 12:00:00", tz = desiredtz) &
                       time < as.POSIXct("2025-05-23 12:00:00", tz = desiredtz))] = 2
  output$night[which(time >= as.POSIXct("2025-05-23 12:00:00", tz = desiredtz) &
                       time < as.POSIXct("2025-05-24 12:00:00", tz = desiredtz))] = 3
  output$night[which(time >= as.POSIXct("2025-05-24 12:00:00", tz = desiredtz))] = 4
  
  SLE = list(output = output,
             SPTE_end = SPTE_end,
             SPTE_start = SPTE_start)
  # The test whether missed sleep windows are correctly identified
  params_sleep = GGIR::load_params()$params_sleep
  params_sleep[["guider_cor_do"]] = TRUE # to run correction
  params_sleep[["guider_cor_maxgap_hrs "]] = 3 # to use max gap size
  params_sleep[["guider_cor_meme_min_dys"]] = 3 # to not do meme
  params_sleep[["guider_cor_min_frac_sib"]]  = 0.8
  params_sleep[["guider_cor_min_hrs"]]  = 1
  params_sleep[["guider_cor_meme_min_hrs"]]  = 2
  SLE = g.part3_correct_guider(SLE, desiredtz, epochSize,
                               params_sleep = params_sleep)
  expect_equal(SLE$SPTE_start, c(25, 22, 26, 22))
  expect_equal(SLE$SPTE_end, c(29.999, 28, 32, 30))
  expect_equal(nrow(SLE$output), 69121)
  
})
