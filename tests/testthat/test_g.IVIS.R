library(GGIR)
context("g.IVIS")
test_that("g.IVIS returns expected output value without missing data", {
  set.seed(1234)
  xx = rnorm(n = 1440)
  Xi1 = rep(xx, 7)
  Xi2 = rep(0:7, each = 1440)
  T1 = g.IVIS(Xi1, epochsizesecondsXi = 60, IVIS_windowsize_minutes = 60, IVIS.activity.metric = 1, IVIS_per_daypair = FALSE)
  T2 = g.IVIS(Xi2, epochsizesecondsXi = 60, IVIS_windowsize_minutes = 60, IVIS.activity.metric = 1, IVIS_per_daypair = FALSE)
  expect_equal(T1$InterdailyStability, 1, tolerance = 0.01) # every day is the same, so we expect IS = 1
  expect_equal(T2$InterdailyStability, 0, tolerance = 0.01) # every day is entirely different, so IS = 0
  expect_equal(T1$IntradailyVariability, 1.97, tolerance = 0.05) #within the day there is variation, so IV>0
  expect_equal(T2$IntradailyVariability, 0, tolerance = 0.01) #within the day there is no variation, so IV=0
  
  
})

test_that("g.IVIS returns expected output value with missing data", {
  set.seed(1234)
  xx = rnorm(n = 1440)
  Xi1 = rep(xx, 7)
  Xi2 = rep(0:7, each = 1440)
  is.na(Xi1[1441:2880]) = TRUE
  is.na(Xi2[1441:2880]) = TRUE
  T3 = g.IVIS(Xi1, epochsizesecondsXi = 60, IVIS_windowsize_minutes = 60, IVIS.activity.metric = 1, IVIS_per_daypair = TRUE)
  T4 = g.IVIS(Xi2, epochsizesecondsXi = 60, IVIS_windowsize_minutes = 60, IVIS.activity.metric = 1, IVIS_per_daypair = TRUE)
  expect_equal(T3$InterdailyStability, 1, tolerance = 0.01) # every day is the same, so we expect IS = 1
  expect_equal(T4$InterdailyStability, 0, tolerance = 0.01) # every day is entirely different, so IS = 0
  expect_equal(T3$IntradailyVariability, 1.97, tolerance = 0.05) #within the day there is variation, so IV>0
  expect_equal(T4$IntradailyVariability, 0, tolerance = 0.01) #within the day there is no variation, so IV=0
})