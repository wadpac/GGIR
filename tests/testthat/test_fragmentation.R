library(GGIR)
context("fragmentation")
test_that("fragmentation calculates the expected fragmentation metric values", {
  skip_on_cran()
  
  Lnames = c("spt_sleep", "spt_wake_IN", "spt_wake_LIG", "spt_wake_MOD", "spt_wake_VIG",
             "day_IN_unbt", "day_LIG_unbt", "day_MOD_unbt", "day_VIG_unbt",
             "day_MVPA_bts_10", "day_IN_bts_30",
             "day_IN_bts_10_30", "day_LIG_bts_10")
  
  # Fragmentation of daytime behaviour
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
  
  expect_equal(out$CoV_dur_IN, 1.035708, tolerance = 0.0001)
  expect_equal(out$CoV_dur_PA, 0.9859804, tolerance = 0.0001)
  
  expect_equal(out$TP_IN2MVPA, 0.005502, tolerance = 0.0001)
  expect_equal(out$TP_IN2LIPA, 0.060523, tolerance = 0.0001)
  expect_equal(out$TP_IN2PA, 0.066025, tolerance = 0.0001)
  expect_equal(out$Nfrag_PA, 49)
  expect_equal(out$Nfrag_IN2LIPA, 44)
  expect_equal(out$Nfrag_IN2MVPA, 4)
  expect_equal(out$Nfrag_IN, 48)
  
  # Fragmentation of nighttime (SPT) behaviour
  # classes: wake 1, sleep 0
  x = c(rep(1, 15), rep(0, 10), rep(1, 30),
        rep(0, 15), rep(1, 20), rep(0, 25), rep(1, 10))
  
  out = g.fragmentation(frag.metrics = "all",
                        LEVELS = x,
                        Lnames = Lnames, mode = "spt")
  expect_equal(out$Nfrag_wake, 3)
  expect_equal(out$Nfrag_sleep, 3)
  expect_equal(out$TP_wake2sleep, 0.040541, tolerance = 0.0001) # 3+epsilon / 65+epsilon
  expect_equal(out$TP_sleep2wake, 0.06, tolerance = 0.0001) # 3+epsilon / 50+epsilon

  # SPT: Test ability to handle NA blocks as needed for g.part6
  # classes: wake 1, sleep 0
  x = c(rep(1, 5), rep(0, 5), rep(1, 5), rep(0, 5), rep(NA, 15),
        rep(0, 5), rep(1, 5), rep(0, 5), rep(1, 5), rep(NA, 15),
        rep(1, 5), rep(0, 5), rep(1, 5), rep(0, 5))

  out = g.fragmentation(frag.metrics = "all",
                        LEVELS = x,
                        Lnames = Lnames, mode = "spt")
  expect_equal(out$Nfrag_sleep, 4)
  expect_equal(out$Nfrag_wake, 5)
  expect_equal(out$TP_sleep2wake, 0.142857, tolerance = 0.0001)
  expect_equal(out$TP_wake2sleep, 0.172414, tolerance = 0.0001)
  
  
  # day: Test ability to handle NA blocks as needed for g.part6
  # 9 is MVPA, 10 is IN
  x = c(rep(10, 5), rep(9, 5), rep(10, 5), rep(9, 5), rep(NA, 15),
        rep(9, 5), rep(10, 5), rep(9, 5), rep(10, 5), rep(NA, 15),
        rep(10, 5), rep(9, 5), rep(10, 5), rep(9, 5))
  
  out = g.fragmentation(frag.metrics = "all",
                        LEVELS = x,
                        Lnames = Lnames, mode = "day")
  expect_equal(out$Nfrag_PA2IN, 4) # 4+epsilon/20+epsilon
  expect_equal(out$Nfrag_IN2PA, 5) # 5+epsilon/25+epsilon
  expect_equal(out$TP_PA2IN, 0.142857, tolerance = 0.0001)
  expect_equal(out$TP_IN2PA, 0.172414, tolerance = 0.0001)
  
  
  # same but now with more variation in segment length
  x = c(rep(10, 4), rep(9, 6), rep(10, 9), rep(9, 2), rep(NA, 12),
        rep(9, 8), rep(10, 5), rep(9, 3), rep(10, 20), rep(NA, 19),
        rep(10, 3), rep(9, 2), rep(10, 7), rep(9, 7))
  
  out = g.fragmentation(frag.metrics = "all",
                        LEVELS = x,
                        Lnames = Lnames, mode = "day")
  expect_equal(out$Nfrag_PA2IN, 4) # 4+epsilon/20+epsilon
  expect_equal(out$Nfrag_IN2PA, 5) # 5+epsilon/25+epsilon
  expect_equal(out$TP_PA2IN, 0.153846, tolerance = 0.0001)
  expect_equal(out$TP_IN2PA, 0.106383, tolerance = 0.0001)
  
})