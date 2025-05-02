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
  params_sleep = load_params()$params_sleep
  params_sleep[["spt_min_block_dur"]] = 30
  params_sleep[["spt_max_gap_dur"]] = 60
  params_sleep[["HDCZA_threshold"]] = NULL
  params_sleep[["HASPT.ignore.invalid"]] = FALSE
  params_sleep[["try_marker_button"]] = FALSE
  test = HASPT(angle = angle, params_sleep = params_sleep,
               ws3 = epochSize, HASPT.algo = "HDCZA", invalid = invalid,
               activity = activity, marker = marker, sibs = sibs)
  expect_equal(test$SPTE_start, 5000)
  expect_equal(test$SPTE_end, 8000)
  
  # HDCZA with HASPT.ignore.invalid = FALSE, spt_max_gap_dur = 1 and spt_max_gap_ratio = 0.5
  params_sleep = load_params()$params_sleep
  params_sleep[["spt_min_block_dur"]] = 30
  params_sleep[["spt_max_gap_dur"]] = 120
  params_sleep[["spt_max_gap_ratio"]] = 0.5
  params_sleep[["HDCZA_threshold"]] = NULL
  params_sleep[["HASPT.ignore.invalid"]] = FALSE
  params_sleep[["try_marker_button"]] = FALSE
  test = HASPT(angle = angle, params_sleep = params_sleep,
               HASPT.algo = "HDCZA", 
               ws3 = epochSize, invalid = invalid, activity = activity,
               marker = marker, sibs = sibs)
  expect_equal(test$SPTE_start, 5000)
  expect_equal(test$SPTE_end, 8000)
  
  # HDCZA with HASPT.ignore.invalid = TRUE
  params_sleep = load_params()$params_sleep
  params_sleep[["spt_min_block_dur"]] = 30
  params_sleep[["spt_max_gap_dur"]] = 60
  params_sleep[["HDCZA_threshold"]] = NULL
  params_sleep[["HASPT.ignore.invalid"]] = TRUE
  params_sleep[["try_marker_button"]] = FALSE
  test = HASPT(angle = angle, params_sleep = params_sleep,
               ws3 = epochSize, HASPT.algo = "HDCZA", invalid = invalid,
               activity = activity, marker = marker, sibs = sibs)
  expect_equal(test$SPTE_start, 5000)
  expect_equal(test$SPTE_end, 5400)
  
  # notused
  params_sleep = load_params()$params_sleep
  params_sleep[["spt_min_block_dur"]] = 30
  params_sleep[["spt_max_gap_dur"]] = 60
  params_sleep[["HDCZA_threshold"]] = NULL
  params_sleep[["HASPT.ignore.invalid"]] = FALSE
  params_sleep[["try_marker_button"]] = FALSE
  test = HASPT(angle = angle, params_sleep = params_sleep,
               ws3 = epochSize, HASPT.algo = "notused", invalid = invalid,
               activity = activity, marker = marker, sibs = sibs)
  expect_null(test$SPTE_start)
  expect_null(test$SPTE_end)
  
  # HorAngle
  params_sleep = load_params()$params_sleep
  params_sleep[["spt_min_block_dur"]] = 30
  params_sleep[["spt_max_gap_dur"]] = 60
  params_sleep[["HDCZA_threshold"]] = NULL
  params_sleep[["HASPT.ignore.invalid"]] = FALSE
  params_sleep[["try_marker_button"]] = FALSE
  params_sleep[["HorAngle_threshold"]] = 60
  test = HASPT(angle = angle, params_sleep = params_sleep,
               ws3 = epochSize, HASPT.algo = "HorAngle", invalid = invalid,
               activity = activity, marker = marker, sibs = sibs)
  expect_equal(test$SPTE_start, 2415)
  expect_equal(test$SPTE_end, 17097)
  
  # NotWorn
  params_sleep = load_params()$params_sleep
  params_sleep[["spt_min_block_dur"]] = 30
  params_sleep[["spt_max_gap_dur"]] = 60
  params_sleep[["HDCZA_threshold"]] = NULL
  params_sleep[["HASPT.ignore.invalid"]] = FALSE
  params_sleep[["try_marker_button"]] = FALSE
  params_sleep[["HorAngle_threshold"]] = 60
  test = HASPT(angle = angle, params_sleep = params_sleep,
               ws3 = epochSize, HASPT.algo = "NotWorn",
               invalid = invalid, activity = activity,
               marker = marker, sibs = sibs)
  expect_equal(test$SPTE_start, 5028)
  expect_equal(test$SPTE_end, 7971)
  expect_equal(test$tib.threshold, 0.2835165, tolerance = 0.001)
  
  # HLRB
  params_sleep = load_params()$params_sleep
  params_sleep[["spt_min_block_dur"]] = 30
  params_sleep[["spt_max_gap_dur"]] = 60
  params_sleep[["HDCZA_threshold"]] = NULL
  params_sleep[["HASPT.ignore.invalid"]] = FALSE
  params_sleep[["try_marker_button"]] = FALSE
  test = HASPT(angle = angle, params_sleep = params_sleep,
               ws3 = epochSize, HASPT.algo = "HLRB", invalid = invalid,
               activity = activity, marker = marker, sibs = sibs)
  expect_equal(test$SPTE_start, 5052)
  expect_equal(test$SPTE_end, 7950)
  expect_equal(test$tib.threshold, 0)
  
  # marker button
  params_sleep = load_params()$params_sleep
  params_sleep[["spt_min_block_dur"]] = 30
  params_sleep[["spt_max_gap_dur"]] = 60
  params_sleep[["HDCZA_threshold"]] = NULL
  params_sleep[["HASPT.ignore.invalid"]] = FALSE
  params_sleep[["consider_marker_button"]] = TRUE
  marker[c(4900, 4901, 4902, 7900, 7901, 7902)] = 1
  downsample = seq(1, N, by = 3)
  test = HASPT(angle = angle, params_sleep = params_sleep,
               ws3 = epochSize,
               HASPT.algo = "markerbutton", invalid = invalid,
               activity = activity, marker = marker, sibs = sibs)
  expect_equal(test$SPTE_start, 4902) # note: times 3 because these are indices in the downsampled data
  expect_equal(test$SPTE_end, 7902)
  expect_equal(test$part3_guider, "markerbutton")
  
  # MotionWare
  # As we do not know whether implementation is correct
  # this unit-test also has limited value. I am keeping it to at least
  # make an attempt to monitor changes in functionality
  params_sleep = load_params()$params_sleep
  params_sleep[["spt_min_block_dur"]] = 30
  params_sleep[["spt_max_gap_dur"]] = 60
  params_sleep[["HDCZA_threshold"]] = NULL
  params_sleep[["HASPT.ignore.invalid"]] = FALSE
  params_sleep[["consider_marker_button"]] = FALSE
  marker[c(4900, 4901, 4902, 7900, 7901, 7902)] = 1
  downsample = seq(1, N, by = 3)
  test = HASPT(angle = NULL, params_sleep = params_sleep,
               ws3 = 15,
               HASPT.algo = "MotionWare", invalid = invalid[downsample],
               activity = activity[downsample] / 1000,
               marker = marker[downsample], sibs = sibs[downsample])
  expect_equal(test$SPTE_start, 0) # note: times 3 because these are indices in the downsampled data
  expect_equal(test$SPTE_end, 5761)
  expect_equal(test$part3_guider, "MotionWare")
})