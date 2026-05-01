library(GGIR)
context("g.part3_correct_guider")
test_that("Align index vectors", {
  skip_on_cran()
  N = 280
  x = c(1, 100, 200)
  y = c(80, 180, 280)
  for (i in 1:11) {
    a = c(20, 120, 220)
    b = c(60, 160, 260)
    if (i == 1) {
      # no corrections expected
    } else if (i == 2) {
      # a one longer beyond N
      a = c(20, 120, 220, 320)
    } else if (i == 3) {
      # a one longer under N
      a = c(20, 120, 220, 275)
    } else if (i == 4) {
      # a and b one longer beyond N
      a = c(20, 120, 220, 320)
      b = c(60, 160, 260, 360)
    } else if (i == 5) {
      # b one longer equal to N
      b = c(60, 160, 260, N)
    } else if (i == 6) {
      # a one longer equal to N
      a = c(20, 120, 220, N)
    } else if (i == 7) {
      # a and b with missing start
      # a one longer equal to N
      a = c(120, 220, N)
      b = c(160, 260)
    } else if (i == 8) {
      # a and b with missing start
      a = c(120, 220)
      b = c(160, 260)
    } else if (i == 9) {
      # a with missing start
      a = c(120, 220)
    } else if (i == 10) {
      # a and b with missing end
      a = c(20, 120)
      b = c(60, 160)
    } else if (i == 11) {
      # b with missing start
      b = c(20, 120)
    }
    test = g.part3_alignIndexVectors(x, y, a, b, N)
    expect_equal(length(test$x), 3)
    expect_equal(length(test$a), 3)
    expect_equal(length(test$b), 3)
    expect_equal(length(test$y), 3)
    testthat::expect_true(all(test$x <= test$a))
    testthat::expect_true(all(test$a <= test$b))
    testthat::expect_true(all(test$b <= test$y))
  }
})
