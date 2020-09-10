library(GGIR)
context("fragmentation")
test_that("fragmentation calculates the expected fragmentation metric values", {
  x2 = c(rep(25,3), rep(75,5), rep(25, 5), rep(200,8),
         rep(25,4), rep(75,6), rep(25, 4), rep(200,9),
         rep(25,3), rep(75,5), rep(25, 5), rep(200,8),
         rep(25,4), rep(75,6), rep(25, 4), rep(200,9),
         rep(25,3), rep(75,5), rep(25, 5), rep(200,8),
         rep(25,4), rep(75,6), rep(25, 4), rep(200,9),
         rep(25,3), rep(75,5), rep(25, 5), rep(200,8),
         rep(25,4), rep(75,6), rep(25, 4), rep(200,9),
         rep(25,3), rep(75,5), rep(25, 5), rep(200,8),
         rep(25,4), rep(75,6), rep(25, 4), rep(200,9),
         rep(25,3), rep(75,5), rep(25, 5), rep(200,8),
         rep(25,4), rep(75,6), rep(25, 4), rep(200,9),
         rep(25,3), rep(75,5), rep(25, 5), rep(200,8),
         rep(25,4), rep(75,6), rep(25, 4), rep(200,9),
         rep(25,3), rep(75,5), rep(25, 5), rep(200,8),
         rep(25,4), rep(75,6), rep(25, 4), rep(200,9))
  ACC = x2
  # x2 = as.integer(x2)
  x = as.integer(ifelse(x2 < 50, yes = 1,
                        no = ifelse(x2 > 50 & x2 < 100, yes = 2,
                                    no = ifelse(x2 > 100 & x2 < 300, yes = 3,
                                                no = ifelse(x2 > 300, yes = 3, no = 0)))))
  intensity.thresholds = c(50,100,400)
  frag.classes = 1
  out = g.fragmentation(frag.metrics = c("all"),
                        ACC=ACC, 
                        intensity.thresholds=intensity.thresholds,
                        do.multiclass=TRUE)

  
  
  expect_equal(round(out$mean_vol_0, digits=2), 8393.75)
  expect_equal(round(out$mean_vol_1, digits=1), 412.5)
  expect_equal(round(out$towardsTP_vol, digits=5), 0.00012)
  expect_equal(round(out$awayTP_vol, digits=5), 0.00242)
  expect_equal(round(out$Gini_vol_0, digits=3), 0.408)
  expect_equal(round(out$Gini_vol_1, digits=4), 0.1877)
  
  expect_equal(round(out$mean_acc_0, digits=1), 1056.2)
  expect_equal(round(out$mean_acc_1, digits=1), 100)
  expect_equal(round(out$towardsTP_acc, digits=5), 0.00095)
  expect_equal(round(out$awayTP_acc, digits=2), 0.01)
  expect_equal(round(out$Gini_acc_0, digits=3), 0.331)
  expect_equal(round(out$Gini_acc_1, digits=4), 0.0968)
  
  expect_equal(round(out$mean_dur_0, digits=3), 7)
  expect_equal(round(out$mean_dur_1, digits=3), 4)
  expect_equal(round(out$towardsTP_dur, digits=4), 0.1429)
  expect_equal(round(out$awayTP_dur, digits=4), 0.25)
  expect_equal(round(out$Gini_dur_0, digits=4), 0.129)
  expect_equal(round(out$Gini_dur_1, digits=4), 0.0968)
  
  expect_equal(round(out$alpha_dur_0, digits=4), 4.2255)
  expect_equal(round(out$x0.5_dur_0, digits=4), 1.2397)
  expect_equal(round(out$W0.5_dur_1, digits=4), 1)

  expect_equal(round(out$IN2MVPA_TPsum, digits=4), 0.1406)
  expect_equal(round(out$IN2LIPA_TPsum, digits=4), 0.1094)
  expect_equal(round(out$IN2MVPA_TPlen, digits=4), 0.125)
  expect_equal(round(out$IN2LIPA_TPlen, digits=4), 0.125)
  expect_equal(round(out$IN2PA_TP, digits=4), 0.25)
  
})