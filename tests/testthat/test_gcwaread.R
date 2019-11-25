library(GGIR)
context("g.cwaread")
test_that("g.cwaread reads data in file correctly", {
  skip_on_cran()
  cwafile  = system.file("testfiles/ax3_testfile.cwa", package = "GGIR")[1]
  AX3 = g.cwaread(fileName = cwafile,desiredtz = "Europe/Berlin", start = 1, end = 4)
  expect_equal(AX3$header$device,"Axivity")
  expect_equal(nrow(AX3$data),900)
  expect_equal(ncol(AX3$data),7)
  expect_equal(AX3$data$time[5],1551174909)
  expect_equal(AX3$data$temp[3],18.65)
  expect_equal(floor(sum(abs(AX3$data[,2:4]))),1407)
})