library(GGIR)
context("fragmentation")
test_that("fragmentation calculates the expected fragmentation metric values", {
  
  x = as.integer(c(rep(0,3),rep(1,20),rep(0,4),rep(1,12),rep(0,1),rep(1,15),rep(0,3),rep(1,9),
        rep(0,2),rep(1,2),rep(0,7),rep(1,16),rep(0,9),rep(1,1),rep(0,2),rep(1,8)))
  
  out = g.fragmentation(x=x,frag.metrics = c("all"))
  expect_that(round(out$mean_0, digits=3),equals(3.875))
  expect_that(round(out$mean_1, digits=3),equals(10.375))
  expect_that(round(out$towardsTP, digits=4),equals(0.2581))
  expect_that(round(out$awayTP, digits=4),equals(0.0964))
  expect_that(round(out$Gini_0, digits=4),equals(0.4009))
  expect_that(round(out$Gini_1, digits=4),equals(0.3907))
  expect_that(round(out$alpha_0, digits=4),equals(1.5458))
  expect_that(round(out$alpha_1, digits=4),equals(1.3725))
  # expect_that(round(out$h_0, digits=4),equals(0.4407))
  # expect_that(round(out$h_1, digits=4),equals(0.3397))
})