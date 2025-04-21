library(GGIR)
context("g.IVIS")
test_that("g.IVIS returns expected output value without missing data", {
  set.seed(1234)
  xx = rnorm(n = 1440) * 1000
  Xi1 = rep(xx, 7)
  Xi2 = rep(0:7, each = 1440)
  T1 = g.IVIS(Xi1, epochSize = 60)
  T2 = g.IVIS(Xi2, epochSize = 60)
  expect_equal(T1$InterdailyStability, 1, tolerance = 0.01) # every day is the same, so we expect IS = 1
  expect_equal(T2$InterdailyStability, 0, tolerance = 0.01) # every day is entirely different, so IS = 0
  expect_equal(T1$IntradailyVariability, 2.244335, tolerance = 0.05) #within the day there is variation, so IV>0
  expect_equal(T2$IntradailyVariability, 0, tolerance = 0.01) #within the day there is no variation, so IV=0
  expect_equal(T1$phi, -0.1453851, tolerance = 0.001)
  expect_equal(T2$phi, 0.9982517, tolerance = 0.001)
})

test_that("g.IVIS returns expected output value with missing data", {
  set.seed(1234)
  xx = abs(rnorm(n = 1440))
  Xi1 = rep(xx, 7)
  Xi2 = rep(0:7, each = 1440)
  is.na(Xi1[1441:2880]) = TRUE
  is.na(Xi2[1441:2880]) = TRUE
  T3 = g.IVIS(Xi1, epochSize = 60)
  T4 = g.IVIS(Xi2, epochSize = 60)
  expect_equal(T3$InterdailyStability, 1, tolerance = 0.01) # every day is the same, so we expect IS = 1
  expect_equal(T4$InterdailyStability, 0, tolerance = 0.01) # every day is entirely different, so IS = 0
  expect_equal(T3$IntradailyVariability, 2.51238, tolerance = 0.05) #within the day there is variation, so IV>0
  expect_equal(T4$IntradailyVariability, 0, tolerance = 0.01) #within the day there is no variation, so IV=0
  expect_equal(T3$phi, -0.2577584, tolerance = 0.001)
  expect_equal(T4$phi, 0.9972666, tolerance = 0.001)
})