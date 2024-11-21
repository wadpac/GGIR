library(GGIR)
context("g.sibreportc")
test_that("g.sibreport creates expected object", {
  ts = data.frame(sibdetection = c(0, 0, 1, 1, 1, 0, 0),
                  diur = rep(0, 7),
                  time = seq(as.POSIXlt(x = "2021-3-3 15:00:00", tz = "Europe/Amsterdam"),
                             as.POSIXlt(x = "2021-3-3 15:06:00", tz = "Europe/Amsterdam"),
                             by = 60),
                  ACC = 1:7, angle = 7:1)
  ID = 12345
  epochlength  = 60
  nonwearlog = data.frame(ID = rep(ID, 5),
                          date = seq(as.Date("2021-3-3"), as.Date("2021-3-7"), by = 1),
                          nonwear1 = rep("15:00:00", 5),
                          nonwear2 = rep("15:30:00", 5))
  naplog = data.frame(ID = rep(ID, 2),
                      date = seq(as.Date("2021-3-3"),as.Date("2021-3-4"), by = 1),
                      nonwear1 = rep("13:00:00", 2),
                      nonwear2 = rep("13:30:00", 2))
  
  logs_diaries = list(sleeplog = c(), nonwearlog = nonwearlog, 
                      naplog = naplog, dateformat = "%Y-%m-%d")
  
  SIBREPORT = g.sibreport(ts, ID, epochlength, logs_diaries)
  
  expect_equal(nrow(SIBREPORT), 8)
  expect_equal(mean(SIBREPORT$duration, na.rm = TRUE), 26.625)
  expect_equal(mean(SIBREPORT$mean_acc_1min_before, na.rm = TRUE), 2)
  expect_equal(mean(SIBREPORT$mean_acc_1min_after, na.rm = TRUE), 6)
  expect_equal(format(SIBREPORT$start[8]), "2021-03-03 15:02:00")
  expect_equal(format(SIBREPORT$end[8]), "2021-03-03 15:04:00")
})