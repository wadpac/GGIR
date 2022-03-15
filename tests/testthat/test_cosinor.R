library(GGIR)
context("cosinorAnalyses")
test_that("cosinorAnalyses provides expected output", {
  ActCosDummy = function(epochSizeSeconds, missingdata = FALSE) {
    N = 1440 * (60 / epochSizeSeconds) # Number of epochs per day
    time = seq(1/N, 7, by = 1 / N)
    counts = sin(time * 2 * pi) + 1
    if (missingdata == TRUE) {
      is.na(counts[N:floor(N * 1.5)]) = TRUE
    }
    return(cosinorAnalyses(Xi = counts, epochsize = epochSizeSeconds))
  }
  # no missing data
  coef5 = ActCosDummy(epochSizeSeconds = 5)
  coef60 = ActCosDummy(epochSizeSeconds = 60)
  coef300 = ActCosDummy(epochSizeSeconds = 300)
  
  # standard cosinor analyses
  expect_equal(coef5$coef$mes, 1, tolerance = 0.01)
  expect_equal(coef5$coef$amp, 1, tolerance = 0.01)
  expect_equal(coef5$coef$acr, pi * 3/2, tolerance = 0.01)
  expect_equal(coef5$coef$acrotime, 6, tolerance = 0.01)
  expect_equal(coef5$coef$ndays, 7)
  expect_equal(coef5$coef$acro, 6, tolerance  = 0.01)
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
 