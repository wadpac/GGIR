library(GGIR)
context("round_decimals_df")
test_that("round_decimals_df returns a data frame with the numeric columns rounded", {
  # POSIXtime2iso8601
  df = data.frame(letters = c("a", "b"), nums = as.character(c(1.543218, 8.216856483)))
  df_converted = round_decimals_df(df = df, digits = 3)
  expect_equal(df_converted$letters, c("a", "b"))
  expect_equal(df_converted$nums, c(1.543, 8.217))
})