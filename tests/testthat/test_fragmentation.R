library(GGIR)
context("fragmentation")
test_that("fragmentation calculates the expected fragmentation metric values", {
  
  x = as.integer(c(rep(0,3),rep(1,20),rep(0,4),rep(1,12),rep(0,1),rep(1,15),rep(0,3),rep(1,9),
        rep(0,2),rep(1,2),rep(0,7),rep(1,16),rep(0,9),rep(1,1),rep(0,2),rep(1,8),
        rep(0,3),rep(1,20),rep(0,4),rep(1,12),rep(0,1),rep(1,15),rep(0,3),rep(1,9),
        rep(0,2),rep(1,2),rep(0,7),rep(1,16),rep(0,9),rep(1,1),rep(0,2),rep(1,8)))
  frag.classes = 1
  set.seed(300)
  
  x2 = c(rep(25,3), rep(75,4), rep(25, 5), rep(200,3),
         rep(25,7), rep(75,9), rep(25, 2), rep(200,4),
         rep(25,5), rep(75,8), rep(25, 4), rep(200,3),
         rep(25,6), rep(75,7), rep(25, 3), rep(200,5),
         rep(25,3), rep(75,4), rep(25, 5), rep(200,3),
         rep(25,7), rep(75,9), rep(25, 2), rep(200,4),
         rep(25,5), rep(75,8), rep(25, 4), rep(200,3),
         rep(25,6), rep(75,7), rep(25, 3), rep(200,5),
         rep(25,3), rep(75,4), rep(25, 5), rep(200,3),
         rep(25,7), rep(75,9), rep(25, 2), rep(200,4),
         rep(25,5), rep(75,8), rep(25, 4), rep(200,3),
         rep(25,6), rep(75,7), rep(25, 3), rep(200,5),
         rep(25,3), rep(75,4), rep(25, 5), rep(200,3),
         rep(25,7), rep(75,9), rep(25, 2), rep(200,4),
         rep(25,5), rep(75,8), rep(25, 4), rep(200,3),
         rep(25,6), rep(75,7), rep(25, 3), rep(200,5))
  ACC = x2[1:length(x)]
  intensity.thresholds = c(50,100,400)
  out = g.fragmentation(x=x, frag.classes=frag.classes, frag.metrics = c("all"),
                        ACC=ACC,
                        intensity.thresholds=intensity.thresholds)
  expect_equal(round(out$mean_0, digits=3), 3.875)
  expect_equal(round(out$mean_1, digits=3), 10.375)
  expect_equal(round(out$towardsTP, digits=4), 0.2581)
  expect_equal(round(out$awayTP, digits=4), 0.0964)
  expect_equal(round(out$Gini_0, digits=4), 0.3742)
  expect_equal(round(out$Gini_1, digits=4), 0.3647)
  expect_equal(round(out$alpha_0, digits=4), 1.5458)
  expect_equal(round(out$alpha_1, digits=4), 1.3725)
  expect_equal(round(out$h_0, digits=4), 0.4407)
  expect_equal(round(out$h_1, digits=4), 0.3397)
  expect_equal(round(out$x0.5_0, digits=4), 3.561)
  expect_equal(round(out$x0.5_1, digits=4), 6.4287)
  expect_equal(round(out$W0.5_0, digits=4), 0.6452)
  expect_equal(round(out$W0.5_1, digits=4), 0.9639)
  expect_equal(round(out$dfa, digits=4), 1.1009)
  expect_equal(round(out$InfEn_binary, digits=4), 0.8442)
  expect_equal(round(out$SampEn_multiclass, digits=4), 0.023)
  expect_equal(round(out$InfEn_multiclass, digits=4), 0.9387)
  expect_equal(round(out$FastApEn_contin, digits=4), 0.7886)
  expect_equal(round(out$h_inac2mvpa, digits=4), 0.5057)
  expect_equal(round(out$h_inac2light, digits=4), 0.5208)
  expect_equal(round(out$inac2mvpaTP, digits=4), 2.5385)
  expect_equal(round(out$inac2lightTP, digits=4), 2.7692)
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