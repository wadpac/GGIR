library(GGIR)
context("g.part5.classifyNaps")
test_that("is able to classifyNaps", {
  starttimes = as.POSIXlt(c("2020-04-04 10:15:00", "2020-04-04 12:00:00",
                            "2020-04-04 12:18:00", "2020-04-04 19:39:00"))
  endtimes = as.POSIXlt(c("2020-04-04 10:40:00", "2020-04-04 12:16:00", 
                          "2020-04-04 12:47:00", "2020-04-04 19:59:00"))
  sibreport = data.frame(ID = 1:4, type = rep("sib", 4),
                         start = starttimes,
                         end = endtimes,
                         mean_acc_1min_before = c(2, 2, 100, 100), mean_acc_1min_after = c(2, 2, 100, 100))
  sibreport$duration = sibreport$end - sibreport$start
  
  classifiedNAPS  = g.part5.classifyNaps(sibreport = sibreport, desiredtz = "", 
                       possible_nap_window = c(9, 18),
                       possible_nap_dur = c(15, 240),
                       nap_model = "hip3yr", HASIB.algo = "vanHees2015")
  expect_equal(nrow(classifiedNAPS), 2)
  expect_equal(ncol(classifiedNAPS), 12)
  expect_equal(classifiedNAPS$probability_nap, c(1, 0))
  expect_equal(classifiedNAPS$mean_acc_1min_after, c(2, 100))
  expect_equal(classifiedNAPS$mean_acc_1min_before, c(2, 2))
  expect_equal(classifiedNAPS$gap2next, c(4800, 120))
})