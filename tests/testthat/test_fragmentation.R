library(GGIR)
context("fragmentation")
test_that("fragmentation calculates the expected fragmentation metric values", {
  skip_on_cran()
  Lnames = c("spt_sleep", "spt_wake_IN", "spt_wake_LIG", "spt_wake_MOD", "spt_wake_VIG",
             "day_IN_unbt", "day_LIG_unbt", "day_MOD_unbt", "day_VIG_unbt", "day_MVPA_bts_10", "day_IN_bts_30",
             "day_IN_bts_10_30", "day_LIG_bts_10")
  x = c(6, 5, 6, 7, 6, 6, 7, 6, 6, 5, 6, 6, 6, 5, 7, 6, 6, 5, 5, 5, 6, 7, 6,
        6, 6, 6, 7, 6, 5, 5, 5, 5, 5, 6, 6, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6,
        7, 7, 6, 5, 6, 5, 6, 5, rep(12, 11), 5, 6, 6, 6, 5, 6, rep(9, 14), 6,
        5, 7, 7, 6, 7, 7, 7, 6, 6, 6, 5, 6, 5, 5, 5, 6, 5, 5, 5, 5, 5, 5, 5,
        5, 6, 7, 7, 7, 6, 7, 7, 6, 5, 5, 5, 6, 6, 5, 5, 5, 6, 7, 7, 7, 7, 7,
        6, 7, 6, 6, 6, 6, 6, 6, 6, 5, 5, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6,
        5, 5, 5, 5, 5, 5, 6, 6, 6, 6, rep(11, 11), 7, 6, 7, 6, 6, 7, 6, 5, 5,
        6, rep(11, 12), 6, rep(11, 11), 6, 6, 7, 6, 6, 6, 6, rep(11, 12), 6,
        5, 5, 5, 5, 5, 6, 5, 6, 6, 6, 5, 5, 6, 6, 7, 6, 6, 6, 5, 5, 6, 5, 5, 6,
        6, rep(10, 98), 6, 6, 5, 5, 5, 5, 5, 5, 6, 6, 5, 5, 5, 5, 6, rep(11, 15),
        6, rep(11, 21), 6, 6, 6, 6, 6, 6, 5, 6, 5, 6, 5, 12, 12, 12, 12, 12, 12,
        12, 12, 12, 12, 12, 12, 7,  rep(10, 55), 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5,
        6, 6, 6,  rep(11, 22), 6, 6, rep(10, 100), 6, rep(11, 23), 6, 5, 5, 5, 5,
        5, 7, rep(10, 133), 6, rep(10, 119), 6, 6, 5, 6, 6, 6, 6, 6, 7, 7, 7, 6,
        5, 6, 6, 6, 5, 5, 5)
  Lnames = c("spt_sleep", "spt_wake_IN", "spt_wake_LIG", "spt_wake_MOD", "spt_wake_VIG",
             "day_IN_unbt", "day_LIG_unbt", "day_MOD_unbt", "day_VIG_unbt", "day_MVPA_bts_10", "day_IN_bts_30",
             "day_IN_bts_10_30", "day_LIG_bts_10")
  out = g.fragmentation(frag.metrics = "all",
                        LEVELS = x,
                        Lnames=Lnames)
  
  expect_equal(round(out$mean_dur_PA, digits=3), 4.02)
  expect_equal(round(out$mean_dur_IN, digits=3), 14.857)
  expect_equal(round(out$Gini_dur_PA, digits=4), 0.5063)
  expect_equal(round(out$Gini_dur_IN, digits=4), 0.7685)
  
  expect_equal(round(out$alpha_dur_PA, digits=4), 2.0551)
  expect_equal(round(out$x0.5_dur_PA, digits=4), 1.9289)
  expect_equal(round(out$W0.5_dur_IN, digits=4), 0.9629)
  
  expect_equal(round(out$TP_IN2MVPA, digits=4), 0.0017)
  expect_equal(round(out$TP_IN2LIPA, digits=4), 0.0654)
  expect_equal(round(out$TP_IN2PA, digits=4), 0.0673)
  expect_equal(out$Nfrag_PA, 49)
  expect_equal(out$Nfrag_IN2LIPA, 44)
  expect_equal(out$Nfrag_IN2MVPA, 4)
  expect_equal(out$Nfrag_IN, 49)
  
})