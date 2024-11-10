library(GGIR)
context("cosinor_IS_IV_Analyses")

test_that("cosinorAnalyses provides expected output", {
  ActCosDummy = function(epochSizeSeconds, missingdata = FALSE, timeOffsetHours = 0) {
    N = 1440 * (60 / epochSizeSeconds) # Number of epochs per day
    if (timeOffsetHours == 0) {
      time = seq(1/N, 7, by = 1 / N)
    } else {
      time = seq(-timeOffsetHours/24 + (1/N), -(timeOffsetHours/24) + 7, by = 1 / N)
    }
    counts = sin(time * 2 * pi) + 1
    if (missingdata == TRUE) {
      is.na(counts[N:floor(N * 2)]) = TRUE
    }
    # Plot code below commented out, but useful for debugging to 
    # understand how dummy time series looks like.
    # x11()
    # plot(log((counts * 1000) + 1), type = "l")
    mod = cosinor_IS_IV_Analyses(Xi = counts * 1000, epochsize = epochSizeSeconds, timeOffsetHours = timeOffsetHours)
    mod$coefext = mod$coefext[which(names(mod$coefext) != "cosinor_ts")]
    return(mod)
  }
  # no missing data
  coef60 = ActCosDummy(epochSizeSeconds = 60)
  coef300 = ActCosDummy(epochSizeSeconds = 300)
  coef60_Offset9 = ActCosDummy(epochSizeSeconds = 60, timeOffsetHours = 9)
  coef60_Offset17 = ActCosDummy(epochSizeSeconds = 60, timeOffsetHours = 17)
  
  expect_equal(coef60_Offset9$coef, coef60$coef, tolerance = 0.001)
  expect_equal(coef60_Offset17$coef, coef60$coef, tolerance = 0.001)
  expect_equal(coef60_Offset9$coefext, coef60$coefext, tolerance = 0.001)
  expect_equal(coef60_Offset17$coefext, coef60$coefext, tolerance = 0.001)
  
  # standard cosinor analyses
  expect_equal(coef60$coef$params$mes, 6.259326, tolerance = 0.01)
  expect_equal(coef60$coef$params$amp, 1.912535, tolerance = 0.01)
  expect_equal(coef60$coef$params$acr, pi * 1/2, tolerance = 0.01)
  expect_equal(coef60$coef$params$acrotime, 6, tolerance = 0.01)
  expect_equal(coef60$coef$params$ndays, 7)
  expect_equal(coef60$coef, coef60$coef, tolerance = 0.01)
  expect_equal(coef60$coef, coef300$coef, tolerance = 0.01)
  expect_equal(coef60$IVIS, coef300$IVIS, tolerance = 0.01)
  
  # extended cosinor analyses
  expect_equal(coef60$coefext$params$minimum, 0)
  expect_equal(coef60$coefext$params$amp, 7.245069, tolerance = 0.01)
  expect_equal(coef60$coefext$params$alpha, -0.921499, tolerance = 0.01)
  expect_equal(coef60$coefext$params$beta, 5.939461, tolerance = 0.01)
  expect_equal(coef60$coefext$params$UpMesor, 19.52358, tolerance = 0.01)
  expect_equal(coef60$coefext$params$DownMesor, 16.47642, tolerance  = 0.01)
  expect_equal(coef60$coefext$params$MESOR, 3.622534, tolerance  = 0.01)
  expect_equal(coef60$coefext$params$R2, 0.8976208, tolerance  = 0.01)
  
  # IV IS
  expect_equal(coef60$IVIS$InterdailyStability, 1, tolerance  = 0.01)
  expect_equal(coef60$IVIS$IntradailyVariability, 0.06774028, tolerance  = 0.01)
  
  fields_to_compare = c("minimum", "amp", "alpha", "beta", "acrotime", "DownMesor", "MESOR")
  expect_equal(coef60$coefext[fields_to_compare], coef300$coefext[fields_to_compare], tolerance = 0.1)
  
  # handling of missing data
  coef60_missingdata = ActCosDummy(epochSizeSeconds = 60, missingdata = TRUE)
  expect_equal(coef60$coef, coef60_missingdata$coef, tolerance = 0.1)
  expect_equal(coef60$coefext[fields_to_compare], coef60_missingdata$coefext[fields_to_compare], tolerance = 0.5)
})
