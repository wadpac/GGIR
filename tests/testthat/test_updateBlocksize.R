library(GGIR)
context("updateBlocksize")
test_that("updateBlocksize works is able to recognize available memory and update blocksize", {
  currenttime = Sys.time()
  out = updateBlocksize(blocksize = 1000, 
                        bsc_qc=data.frame(time=as.character(currenttime), 
                                          size=2, stringsAsFactors = FALSE))
  ra = round(out$blocksize/100)
  eval = (ra == 10 | ra == 8)
  expect_equal(eval,TRUE)
})