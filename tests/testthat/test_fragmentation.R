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
  

  out = g.fragmentation(frag.metrics = "all",
                        LEVELS = x,
                        Lnames = Lnames, mode = "day")
  
  expect_equal(out$mean_dur_PA, 4.020408, tolerance = 0.0001)
  expect_equal(out$mean_dur_IN, 15.10417, tolerance = 0.0001)
  expect_equal(out$Gini_dur_PA, 0.506352, tolerance = 0.0001)
  expect_equal(out$Gini_dur_IN, 0.768544, tolerance = 0.0001)
  
  expect_equal(out$alpha_dur_PA, 2.055102, tolerance = 0.0001)
  expect_equal(out$x0.5_dur_PA, 1.928897, tolerance = 0.0001)
  expect_equal(out$W0.5_dur_IN, 0.9629121, tolerance = 0.0001)
  
  expect_equal(out$TP_IN2MVPA, 0.005502, tolerance = 0.0001)
  expect_equal(out$TP_IN2LIPA, 0.060523, tolerance = 0.0001)
  expect_equal(out$TP_IN2PA, 0.066025, tolerance = 0.0001)
  expect_equal(out$Nfrag_PA, 49)
  expect_equal(out$Nfrag_IN2LIPA, 44)
  expect_equal(out$Nfrag_IN2MVPA, 4)
  expect_equal(out$Nfrag_IN, 48)
  
  x = c(rep(1, 15), rep(0, 10), rep(1, 15), rep(2, 15),
        rep(0, 15), rep(1, 10), rep(2, 10),
        rep(0, 25), rep(1, 10))
  
  out = g.fragmentation(frag.metrics = "all",
                        LEVELS = x,
                        Lnames = Lnames, mode = "spt")
  
  expect_equal(out$Nfrag_PA, 2)
  expect_equal(out$Nfrag_IN, 2)
  expect_equal(out$TP_PA2IN, 0.08, tolerance = 0.0001)
  expect_equal(out$TP_IN2PA, 0.020202, tolerance = 0.0001)
  expect_equal(out$Nfrag_wake, 3)
  expect_equal(out$Nfrag_sleep, 3)
  expect_equal(out$TP_wake2sleep, 0.040541, tolerance = 0.0001)
  expect_equal(out$TP_sleep2wake, 0.06, tolerance = 0.0001)
  
  
})