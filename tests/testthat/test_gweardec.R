library(GGIR)
context("g.weardec detection")
test_that("g.weardec relabels data correctly", {
  skip_on_cran()
  ws2 = 900
  wearthreshold = 2
  Ntotal = 300
  createNW = function(Ntotal = 300, Nnonwear = 0, invert = FALSE) {
    wearhalf = ((Ntotal - Nnonwear) / 2)
    N1 = floor(wearhalf)
    N2 = ceiling(wearhalf)
    N3 = Ntotal - (N1 + N2)
    if (invert == FALSE) {
      ts = c(rep(0, N1), rep(3, N3), rep(0, N2))
    } else {
      ts = c(rep(3, N1), rep(0, N3), rep(3, N2))
    }
    return(ts)
  }
  params_cleaning = load_params()$params_cleaning
  # No non-wear, no clipping
  metalong = data.frame(nonwearscore = createNW(),
                        clippingscore = rep(0, Ntotal))
  M = list(metalong = metalong)
  out = g.weardec(M, wearthreshold, ws2, params_cleaning = params_cleaning)
  expect_equal(sum(out$r1), 0)
  expect_equal(sum(out$r2), 0)
  expect_equal(sum(out$r3), 0)
  expect_equal(out$LC, 0)
  expect_equal(out$LC2, 0)
  
  # Non-wear a few epoch in the first 3 hours, no clipping
  metalong = data.frame(nonwearscore = c(rep(0, 3), rep(3, 2), rep(0, Ntotal - 5)),
                        clippingscore = rep(0, Ntotal))
  M = list(metalong = metalong)
  out = g.weardec(M, wearthreshold, ws2, params_cleaning = params_cleaning)
  expect_equal(sum(out$r1), 2)
  expect_equal(sum(out$r3), 3)
  expect_equal(sum(out$r1 + out$r3), 5)
  
  # Non-wear a few epoch in the last 3 hours, no clipping
  metalong = data.frame(nonwearscore = c(rep(0, Ntotal - 5), rep(3, 2), rep(0, 3)),
                        clippingscore = rep(0, Ntotal))
  M = list(metalong = metalong)
  out = g.weardec(M, wearthreshold, ws2, params_cleaning = params_cleaning)
  expect_equal(sum(out$r1), 2)
  expect_equal(sum(out$r3), 3)
  expect_equal(sum(out$r1 + out$r3), 5)
  
  # Non-wear 20 in middle + clipping
  metalong = data.frame(nonwearscore = createNW(Nnonwear = 20),
                        clippingscore = c(rep(0, Ntotal - 100), rep(1, 100)))
  M = list(metalong = metalong)
  out = g.weardec(M, wearthreshold, ws2, params_cleaning = params_cleaning)
  expect_equal(sum(out$r1), 20)
  expect_equal(sum(out$r2), 100)
  expect_equal(sum(out$r3), 0)
  
  # Wear 20 in the middle + clipping
  metalong = data.frame(nonwearscore = createNW(Nnonwear = 20, invert = TRUE),
                        clippingscore = c(rep(0, Ntotal - 200), rep(1, 200)))
  M = list(metalong = metalong)
  out = g.weardec(M, wearthreshold, ws2, params_cleaning = params_cleaning)
  expect_equal(sum(out$r1), 280)
  expect_equal(sum(out$r2), 200)
  expect_equal(sum(out$r3), 20)
  
  
  # Non-wear 100 in middle + clipping
  metalong = data.frame(nonwearscore = createNW(Nnonwear = 100),
                        clippingscore = c(rep(0, Ntotal - 15), rep(1, 15)))
  M = list(metalong = metalong)
  out = g.weardec(M, wearthreshold, ws2, params_cleaning = params_cleaning)
  expect_equal(sum(out$r1), 100)
  expect_equal(sum(out$r2), 15)
  expect_equal(sum(out$r3), 0)
  
  # Wear 100 in the middle + clipping
  metalong = data.frame(nonwearscore = createNW(Nnonwear = 100, invert = TRUE),
                        clippingscore = c(rep(0, Ntotal - 280), rep(1, 280)))
  M = list(metalong = metalong)
  out = g.weardec(M, wearthreshold, ws2, params_cleaning = params_cleaning)
  expect_equal(sum(out$r1), 200)
  expect_equal(sum(out$r2), 280)
  expect_equal(sum(out$r3), 0)
  
  # Filter short nonwear during the night: defined with a set window
  params_cleaning$nonwearFiltermaxHours = 12
  params_cleaning$nonwearFilterWindow = c(19, 9)
  alternatingNonwear = rep(c(0, 0, 0, 0, 3, 3, 3, 3), ceiling(Ntotal / 8))
  metalong = data.frame(nonwearscore = alternatingNonwear[1:Ntotal],
                        clippingscore = c(rep(0, Ntotal - 280), rep(1, 280)))
  tz = "Europe/Amsterdam"
  timestamp = POSIXtime2iso8601(seq(as.POSIXct("2024-10-29 00:00:00", tz = tz),
                                    by = ws2, length.out = nrow(metalong)), tz = tz)
  metalong$timestamp = timestamp
  M = list(metalong = metalong)
  out = g.weardec(M, wearthreshold, ws2, params_cleaning = params_cleaning)
  expect_equal(sum(out$r1), 60)
  expect_equal(sum(out$r2), 280)
  expect_equal(sum(out$r3), 48)
})



