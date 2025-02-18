library(GGIR)
context("HASIB")
test_that("HASIB generate correct output", {
  time = seq(as.POSIXlt(x = "2021-3-3 15:00:00", tz = "Europe/Amsterdam"),
             as.POSIXlt(x = "2021-3-3 16:05:00", tz = "Europe/Amsterdam"), by = 5)
  tmp = c(rep(0, 10), 10, 20, 30, rep(30, 310), 20, 10, -5, -20, 0, -15, -30, rep(-40, 400))
  anglez = c(tmp, rep(0, length(time) - length(tmp)))
  set.seed(1245)
  zeroCrossingCount = round(abs(rnorm(n = length(anglez), mean = 0, sd = 50)))
  zeroCrossingCount[40:500] = 0
  vanHees2015 = HASIB(HASIB.algo = "vanHees2015", timethreshold = 5, anglethreshold = 5, 
                   time = time, anglez = anglez, ws3 = 5, zeroCrossingCount = c())
  Sadeh1994 = HASIB(HASIB.algo = "Sadeh1994", timethreshold = c(), anglethreshold = c(), 
                      time = time, anglez = c(), ws3 = 5, zeroCrossingCount = zeroCrossingCount)
  ColeKripke1992 = HASIB(HASIB.algo = "ColeKripke1992", timethreshold = c(), anglethreshold = c(), 
                    time = time, anglez = c(), ws3 = 5, zeroCrossingCount = zeroCrossingCount)
  Galland2012 = HASIB(HASIB.algo = "Galland2012", timethreshold = c(), anglethreshold = c(), 
                    time = time, anglez = c(), ws3 = 5, zeroCrossingCount = zeroCrossingCount)
  Oakley1997 = HASIB(HASIB.algo = "Oakley1997", timethreshold = c(), anglethreshold = c(), 
                     time = time, anglez = c(), ws3 = 5, zeroCrossingCount = zeroCrossingCount,
                     oakley_threshold = 20)
  
  expect_equal(nrow(vanHees2015), 781)
  expect_equal(nrow(Sadeh1994), 781)
  expect_equal(nrow(Galland2012), 781)
  expect_equal(nrow(ColeKripke1992), 781)
  expect_equal(nrow(Oakley1997), 781)
  expect_equal(length(which(vanHees2015[,1] == 1)), 713)
  expect_equal(length(which(Sadeh1994[,1] == 1)), 372)
  expect_equal(length(which(ColeKripke1992[,1] == 1)), 372)
  expect_equal(length(which(Galland2012[,1] == 1)), 384)
  expect_equal(length(which(Oakley1997[,1] == 1)), 414)
})