library(GGIR)
context("lux_per_segment")
test_that("lux_per_segment is correctly calculated", {
  skip_on_cran()
  N = 10000
  t0 = as.POSIXlt("2021-05-27 22:08:28", tz="Europe/Amsterdam", origin="1970-01-01")
  time = seq(t0, t0 + ((N * 60)-1), by=60) # the values of time are not used in the function, we only care about the length the object time
  diur = c(rep(1,1000),rep(0,8000),rep(1, 1000))
  detection = rep(0, N)
  set.seed(300)
  detection[which(runif(n = N,min=0,max=1)<0.1)] = 1
  ACC = rnorm(n = N,mean=0,sd=40)
  lightpeak_imputationcode = lightpeak = ((1:N) / N) * 1000
  lightpeak[c(4:10)] = NA
  lightpeak_imputationcode[c(4:10)] = 2
  
  ts = data.frame(ACC=ACC,time=time, diur=diur,sibdetection=detection, 
                  lightpeak=lightpeak, lightpeak_imputationcode=lightpeak_imputationcode)
  sse = 1001:9000
  LUX_day_segments = c(9, 15, 24)
  ws3new = 60
  luxperseg = g.part5.lux_persegment(ts, sse, LUX_day_segments, ws3new)
  expect_equal(round(luxperseg$values[3]), 12)
  expect_equal(round(luxperseg$values[4]), 540)
  expect_equal(round(luxperseg$values[5]), 3)
  expect_equal(round(luxperseg$values[10]), 0)
  expect_equal(luxperseg$names[10], "LUX_ignored_15_24hr_day")
  expect_equal(luxperseg$names[3], "LUX_timeawake_9_15hr_day" )
  
})



