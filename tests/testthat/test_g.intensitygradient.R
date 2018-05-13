library(GGIR)
context("g.intensitygradient")
options(encoding = "UTF-8")
test_that("g.intensitygradient returns expected output value", {
  x = c(seq(12.5,4000,by=25),6000)
  y = (x * -1.5) + 10000
  IG = g.intensitygradient(x = x, y = y)
  expect_equal(round(IG$gradient,digits=2),-0.23)
  expect_equal(round(IG$y_intercept,digits=2),10.50)
  expect_equal(round(IG$rsquared,digits=2),0.59)
})