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
                        intensity.thresholds=intensity.thresholds)

  
  
  expect_equal(round(out$mean_dur_PA, digits=3), 7)
  expect_equal(round(out$mean_dur_IN, digits=3), 4)
  expect_equal(round(out$Gini_dur_PA, digits=4), 0.129)
  expect_equal(round(out$Gini_dur_IN, digits=4), 0.0968)
  
  expect_equal(round(out$alpha_dur_PA, digits=4), 1.521)
  expect_equal(round(out$x0.5_dur_PA, digits=4), 3.7828)
  expect_equal(round(out$W0.5_dur_IN, digits=4), 1)

  expect_equal(round(out$TP_IN2MVPA, digits=4), 0.1406)
  expect_equal(round(out$TP_IN2LIPA, digits=4), 0.1094)
  expect_equal(round(out$TP_IN2PA, digits=4), 0.25)
  
})