library(GGIR)
context("tidyup_df")
test_that("tidyup_df returns a data frame with the numeric columns rounded", {
  # POSIXtime2iso8601
  df = data.frame(letters = c("a", "b"), nums = as.character(c(1.543218, 8.216856483)), 
                  factor = factor(c("f1", "f2")), missing = c(NA, "NaN"))
  df_converted = tidyup_df(df = df, digits = 3)
  expect_equal(df_converted$letters, df$letters)
  expect_equal(df_converted$nums, c(1.543, 8.217))
  expect_equal(df_converted$factor, df$factor)
  expect_equal(df_converted$missing, c("", ""))
})