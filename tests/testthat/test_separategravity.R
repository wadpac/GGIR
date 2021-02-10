library(GGIR)
context("separategravity")
test_that("separategravity", {
  library(testthat)
  set.seed(123)
  N = 10000
  acc = matrix(rnorm(n=3*N, mean = 0, sd=0.2),ncol=3)
  acc[c(200:300, 700:800),] = 0
  acc[,1] = acc[,1]+1
  set.seed(345)
  gyr = matrix(rnorm(n=3*N, mean = 0, sd=100),ncol=3)
  sf = 20
  
  sg = separategravity(acc = acc, gyr = gyr, sf=sf)
  
  expect_equal(round(sg$acclocal[2,],digits=3),c(0.954, -0.033, -0.044))
  expect_equal(sum(rowSums(round(sg$gvector,digits=3))), 522.951)
  expect_equal(sum(rowSums(round(sg$acclocal,digits=3))), 9431.448)
})