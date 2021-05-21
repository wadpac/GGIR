library(GGIR)
context("identify_levels")
test_that("identify_levels", {
  time = rep(0,10000) # the values of time are not used in the function, we only care about the length the object time
  diur = c(rep(0,2000),rep(1,4000),rep(0,4000))
  detection = time
  set.seed(300)
  detection[which(runif(n = 10000,min=0,max=1)<0.1)] = 1
  ACC = rnorm(n = 10000,mean=0,sd=40)
  ts = data.frame(ACC=ACC,time=time, diur=diur,sibdetection=detection)
  levels = identify_levels(ts, TRLi=40,TRMi=100,TRVi=400,
                             boutdur.mvpa=10,boutcriter.mvpa=0.8,
                             boutdur.lig=5,boutcriter.lig=0.8,
                             boutdur.in=10,boutcriter.in=0.9,
                             ws3=5,bout.metric=4)
  expect_that(as.numeric(table(levels$LEVELS))[1],equals(403))
  expect_that(as.numeric(table(levels$LEVELS)[3]),equals(553))
  expect_that(as.numeric(table(levels$LEVELS)[5]),equals(3522))
  expect_that(as.numeric(table(levels$LEVELS)[7]),equals(24))
  expect_that(as.numeric(table(levels$OLEVELS)[2]),equals(5085))
  expect_that(as.numeric(table(levels$OLEVELS)[4]),equals(28))
  expect_that(levels$Lnames[12],equals("day_LIG_bts_5"))
  expect_that(as.numeric(table(as.numeric(levels$bc.in))[2]),equals(1752))
  
})



