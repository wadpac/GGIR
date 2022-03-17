library(GGIR)
context("cosinorAnalyses")

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
      is.na(counts[N:floor(N * 1.5)]) = TRUE
    }
    return(cosinorAnalyses(Xi = counts, epochsize = epochSizeSeconds, timeOffsetHours = timeOffsetHours))
  }
  # no missing data
  coef5 = ActCosDummy(epochSizeSeconds = 5)
  coef60 = ActCosDummy(epochSizeSeconds = 60)
  coef300 = ActCosDummy(epochSizeSeconds = 300)
  coef60_Offset9 = ActCosDummy(epochSizeSeconds = 60, timeOffsetHours = 9)
  coef60_Offset17 = ActCosDummy(epochSizeSeconds = 60, timeOffsetHours = 17)
  expect_equal(coef60_Offset9$coef, coef60$coef, tolerance = 0.001)
  expect_equal(coef60_Offset17$coef, coef60$coef, tolerance = 0.001)
  expect_equal(coef60_Offset9$coefext, coef60$coefext, tolerance = 0.001)
  expect_equal(coef60_Offset17$coefext, coef60$coefext, tolerance = 0.001)
  
  # standard cosinor analyses
  expect_equal(coef5$coef$mes, 1, tolerance = 0.01)
  expect_equal(coef5$coef$amp, 1, tolerance = 0.01)
  expect_equal(coef5$coef$acr, pi * 3/2, tolerance = 0.01)
  expect_equal(coef5$coef$acrotime, 6, tolerance = 0.01)
  expect_equal(coef5$coef$ndays, 7)
  expect_equal(coef5$coef, coef60$coef, tolerance = 0.01)
  expect_equal(coef5$coef, coef300$coef, tolerance = 0.01)
  
  # extended cosinor analyses
  expect_equal(coef5$coefext$minimum, 0)
  expect_equal(coef5$coefext$amp, 2.126, tolerance = 0.01)
  expect_equal(coef5$coefext$alpha, 0.06468, tolerance = 0.01)
  expect_equal(coef5$coefext$beta, 2.4317, tolerance = 0.01)
  expect_equal(coef5$coefext$UpMesor, 0.2549, tolerance = 0.01)
  expect_equal(coef5$coefext$DownMesor, 11.7603, tolerance  = 0.01)
  expect_equal(coef5$coefext$MESOR, 1.063105, tolerance  = 0.01)
  fields_to_compare = c("minimum", "amp", "alpha", "beta", "acrotime", "DownMesor", "MESOR")
  expect_equal(coef5$coefext[fields_to_compare], coef60$coefext[fields_to_compare], tolerance = 0.1)
  expect_equal(coef60$coefext[fields_to_compare], coef300$coefext[fields_to_compare], tolerance = 0.1)
  
  # handling of missing data
  coef60_missingdata = ActCosDummy(epochSizeSeconds = 60, missingdata = TRUE)
  expect_equal(coef60$coef, coef60_missingdata$coef)
  expect_equal(coef60$coefext[fields_to_compare], coef60_missingdata$coefext[fields_to_compare], tolerance = 0.5)
})
