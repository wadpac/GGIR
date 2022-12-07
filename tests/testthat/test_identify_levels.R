library(GGIR)
context("identify_levels")
test_that("identify_levels", {
  time = rep(0, 10000) # the values of time are not used in the function, we only care about the length the object time
  diur = c(rep(0, 2000), rep(1, 4000), rep(0, 4000))
  detection = time
  set.seed(300)
  detection[which(runif(n = 10000, min = 0, max = 1) < 0.1)] = 1
  set.seed(1000)
  ACC = rnorm(n = 10000, mean = 0, sd = 40)
  ts = data.frame(ACC = ACC,time = time, diur = diur, sibdetection = detection)
  levels = identify_levels(ts, TRLi = 40, TRMi = 100, TRVi = 400,
                             boutdur.mvpa = 10, boutcriter.mvpa = 0.8,
                             boutdur.lig = 5, boutcriter.lig = 0.8,
                             boutdur.in = 10, boutcriter.in = 0.9,
                             ws3 = 5)
  expect_equal(as.numeric(table(levels$LEVELS)), c(403, 3012, 564, 21, 3792, 779, 34, 1395))
  expect_equal(levels$Lnames[12], "day_LIG_bts_5")
})



