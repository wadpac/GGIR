library(GGIR)
context("g.sibreportc")
test_that("g.sibreport creates expected object", {
  ts = data.frame(sibdetection = c(0, 0, 1, 1, 1, 0, 0),
             diur = rep(0, 7),
             time = seq(as.POSIXlt(x = "2021-3-3 15:00:00", tz="Europe/Amsterdam"),
                        as.POSIXlt(x = "2021-3-3 15:06:00", tz="Europe/Amsterdam"), by = 60),
             ACC = 1:7, angle = 7:1)
  ID = 12345
  epochlength  = 60
  SIBREPORT = g.sibreport(ts, ID, epochlength)
  expect_equal(nrow(SIBREPORT), 1)
  expect_equal(SIBREPORT$mean_acc, 4)
  expect_equal(SIBREPORT$sd_ang, 1)
  expect_equal(SIBREPORT$duration, 3)
})