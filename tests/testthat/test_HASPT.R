library(GGIR)
context("HASPT")
test_that("HASPT generate correct output", {
  skip_on_cran()

  # prepare dummy data
  N = 17280
  epochSize = 5
  nomovement = c(5000:6000, 6100:8000)
  
  invalid = rep(0, N)
  invalid[5400:7800] = 1
  
  set.seed(1234)
  activity = abs(rnorm(n = N, mean = 0, sd = 60))
  activity[nomovement] = 0
  
  set.seed(1234)
  angle = rnorm(n = N, mean = 0, sd = 20)
  angle[which(angle > 90)] = 90
  angle[which(angle < -90)] = -90
  angle[nomovement] = angle[5000]
  
  sibs = rep(0, N)
  sibs[nomovement] = 1
  sibs[c(5000:5050, 6000:6050, 7950:8000)] = 0
  
  marker = rep(0, N)
  marker[c(4900, 7900)] = 1
  
  # Test all HASPT.algo setting
  
  # HDCZA with HASPT.ignore.invalid = FALSE
  test = HASPT(angle = angle, spt_min_block = 30, spt_max_gap = 60,
               ws3 = epochSize,
               HASPT.algo = "HDCZA", HDCZA_threshold = c(), invalid = invalid,
               HASPT.ignore.invalid = FALSE, activity = activity ,
               marker = marker,
               sibs = sibs,
               try_marker_button = FALSE)
  expect_equal(test$SPTE_start, 5000)
  expect_equal(test$SPTE_end, 8000)
  
  
  # HDCZA with HASPT.ignore.invalid = TRUE
  test = HASPT(angle = angle, spt_min_block = 30, spt_max_gap = 60,
               ws3 = epochSize,
               HASPT.algo = "HDCZA", HDCZA_threshold = c(), invalid = invalid,
               HASPT.ignore.invalid = TRUE, activity = activity,
               marker = marker,
               sibs = sibs,
               try_marker_button = FALSE)
  expect_equal(test$SPTE_start, 5000)
  expect_equal(test$SPTE_end, 5400)
  
  # nutused
  test = HASPT(angle = angle, spt_min_block = 30, spt_max_gap = 60,
               ws3 = epochSize,
               HASPT.algo = "notused", HDCZA_threshold = c(), invalid = invalid,
               HASPT.ignore.invalid = FALSE, activity = activity ,
               marker = marker,
               sibs = sibs,
               try_marker_button = FALSE)
  expect_null(test$SPTE_start)
  expect_null(test$SPTE_end)
  
  # HorAngle
  test = HASPT(angle = angle, spt_min_block = 30, spt_max_gap = 60,
               ws3 = epochSize,
               HASPT.algo = "HorAngle", HDCZA_threshold = c(), invalid = invalid,
               HASPT.ignore.invalid = FALSE, activity = activity ,
               marker = marker,
               sibs = sibs,
               try_marker_button = FALSE, HorAngle_threshold = 60)
  expect_equal(test$SPTE_start, 2415)
  expect_equal(test$SPTE_end, 17097)
  

  # NotWorn
  test = HASPT(angle = angle, spt_min_block = 30, spt_max_gap = 60,
               ws3 = epochSize,
               HASPT.algo = "NotWorn", HDCZA_threshold = c(), invalid = invalid,
               HASPT.ignore.invalid = FALSE, activity = activity ,
               marker = marker,
               sibs = sibs,
               try_marker_button = FALSE)
  expect_equal(test$SPTE_start, 5028)
  expect_equal(test$SPTE_end, 7971)
  expect_equal(test$tib.threshold, 0.2835165, tolerance = 0.001)
  
  # HLRB
  test = HASPT(angle = angle, spt_min_block = 30, spt_max_gap = 60,
               ws3 = epochSize,
               HASPT.algo = "HLRB", HDCZA_threshold = c(), invalid = invalid,
               HASPT.ignore.invalid = FALSE, activity = activity ,
               marker = marker,
               sibs = sibs,
               try_marker_button = FALSE)
  expect_equal(test$SPTE_start, 5052)
  expect_equal(test$SPTE_end, 7950)
  expect_equal(test$tib.threshold, 0)
  
  # marker button
  marker[c(4900, 4901, 4902, 7900, 7901, 7902)] = 1
  downsample = seq(1, N, by = 3)
  test = HASPT(angle = NULL, spt_min_block = 30, spt_max_gap = 60,
               ws3 = 15,
               HASPT.algo = "MotionWare", HDCZA_threshold = c(), invalid = invalid[downsample],
               HASPT.ignore.invalid = FALSE, activity = activity[downsample],
               marker = marker[downsample],
               sibs = sibs[downsample],
               try_marker_button = TRUE)
  expect_equal(test$SPTE_start * 3, 4902) # note: times 3 because these are indices in the downsampled data
  expect_equal(test$SPTE_end * 3, 7902)
  expect_equal(test$part3_guider, "markerbutton")
  
  
  # MotionWare
  # As we do not know whether implementation is correct
  # this unit-test also has limited value. I am keeping it to at least
  # make an attempt to monitor changes in functionality
  marker[c(4900, 4901, 4902, 7900, 7901, 7902)] = 1
  downsample = seq(1, N, by = 3)
  test = HASPT(angle = NULL, spt_min_block = 30, spt_max_gap = 60,
               ws3 = 15,
               HASPT.algo = "MotionWare", HDCZA_threshold = c(), invalid = invalid[downsample],
               HASPT.ignore.invalid = FALSE, activity = activity[downsample] / 1000,
               marker = marker[downsample],
               sibs = sibs[downsample],
               try_marker_button = FALSE)
  expect_equal(test$SPTE_start, 0) # note: times 3 because these are indices in the downsampled data
  expect_equal(test$SPTE_end, 5761)
  expect_equal(test$part3_guider, "MotionWare")
})