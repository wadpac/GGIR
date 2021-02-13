library(GGIR)
context("g.IVIS")
test_that("g.IVIS returns expected output value", {
  set.seed(1234)
  xx = rnorm(n = 1440)
  Xi1 = rep(xx, 7)
  Xi2 = rep(0:7, each=1440)
  T1 = g.IVIS(Xi1, epochsizesecondsXi = 60, IVIS_windowsize_minutes = 60, IVIS.activity.metric = 1)
  T2 = g.IVIS(Xi2, epochsizesecondsXi = 60, IVIS_windowsize_minutes = 60, IVIS.activity.metric = 1)
  expect_equal(round(T1$InterdailyStability,digits=2),1) # every day is the same, so we expect IS = 1
  expect_equal(round(T2$InterdailyStability,digits=2),0) # every day is entirely different, so IS = 0
  expect_equal(round(T1$IntradailyVariability,digits=2),1.94) #within the day there is variation, so IV>0
  expect_equal(round(T2$IntradailyVariability,digits=2),0) #within the day there is no variation, so IV=0
})