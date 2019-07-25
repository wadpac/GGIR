library(GGIR)
context("getFirstTimestamp")
test_that("getFirstTimestamp extracts correct first timestamp"[1], {
  GAfile  = system.file("binfile/GENEActiv_testfile.bin", package = "GGIR")[1]
  P = expect_warning(GENEAread::read.bin(binfile = GAfile, start=1,end=2))
  FT = expect_warning(getFirstTimestamp(f=GAfile, p1=P$data.out[1,1]))
  expect_equal(as.vector(as.character(FT[1])),"2013-05-30 12:12:54.5")
})