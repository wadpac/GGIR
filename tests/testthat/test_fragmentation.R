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

  expect_equal(round(out$mean_0, digits=3), 7)
  expect_equal(round(out$mean_1, digits=3), 4)
  expect_equal(round(out$towardsTP, digits=4), 0.1429)
  expect_equal(round(out$awayTP, digits=4), 0.25)
  expect_equal(round(out$Gini_0, digits=4), 0.129)
  expect_equal(round(out$Gini_1, digits=4), 0.0968)
  expect_equal(round(out$alpha_0, digits=4), 3.4074)
  expect_equal(round(out$alpha_1, digits=4), 3.2033)
  expect_equal(round(out$h_0, digits=4), 0.5208)
  expect_equal(round(out$h_1, digits=4), 0.6389)
  expect_equal(round(out$x0.5_0, digits=4), 4.2191)
  expect_equal(round(out$x0.5_1, digits=4), 2.5697)
  expect_equal(round(out$W0.5_0, digits=4), 1)
  expect_equal(round(out$W0.5_1, digits=4), 1)
  expect_equal(round(out$dfa, digits=4), 0.8884)
  expect_equal(round(out$InfEn_binary, digits=4), 0.9457)
  expect_equal(round(out$SampEn_multiclass, digits=4), 0.0146)
  expect_equal(round(out$InfEn_multiclass, digits=4), 0.9847)
  expect_equal(round(out$FastApEn_contin, digits=4), 0.8031)
  
  expect_equal(round(out$IN2MVPA_TPsum, digits=4), 0.1406)
  expect_equal(round(out$IN2LIPA_TPsum, digits=4), 0.1094)
  expect_equal(round(out$IN2MVPA_TPlen, digits=4), 0.125)
  expect_equal(round(out$IN2LIPA_TPlen, digits=4), 0.125)
  expect_equal(round(out$IN2PA_TP, digits=4), 0.25)
  # expect_that(round(out$REC_rqa_contin, digits=4),equals(0.0098))
  # expect_that(round(out$DET_rqa_contin, digits=4),equals(0.904))
  # # expect_that(round(out$RATIO_rqa_contin, digits=4),equals(92.3454))
  # # expect_that(round(out$DIV_rqa_contin, digits=4),equals(Inf))
  # expect_that(round(out$Lmax_rqa_contin, digits=4),equals(0))
  # expect_that(round(out$Lmean_rqa_contin, digits=4),equals(113))
  # expect_that(round(out$ENTR_rqa_contin, digits=4),equals(0))
  # expect_that(round(out$LAM_rqa_contin, digits=4),equals(0))
  # expect_that(round(out$Vmax_rqa_contin, digits=4),equals(0))
  # expect_that(round(out$Vmean_rqa_contin, digits=4),equals(0))
  
})