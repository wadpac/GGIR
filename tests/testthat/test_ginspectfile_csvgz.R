library(GGIR)
testthat::context("g.inspectfile CSV")

# adapted from R.utils::compressFile
quick_gzip = function(filename, BFR.SIZE=1e+07, ...) {
  fext = strsplit(filename, "[.]")[[1]]
  fext = fext[length(fext)]
  destnameT = tempfile(fileext = paste0(".", fext, ".gz"))
  inn <- file(filename, open = "rb")
  on.exit(if (!is.null(inn)) close(inn))
  outComplete <- FALSE
  out <- gzfile(destnameT, open = "wb", ...)
  on.exit({
    if (!is.null(out)) close(out)
    if (!outComplete) file.remove(destnameT)
  }, add = TRUE)
  nbytes <- 0
  repeat {
    bfr <- readBin(inn, what = raw(0L), size = 1L, n = BFR.SIZE)
    n <- length(bfr)
    if (n == 0L)
      break
    nbytes <- nbytes + n
    writeBin(bfr, con = out, size = 1L)
    bfr <- NULL
  }
  outComplete <- TRUE
  close(out)
  out <- NULL
  return(destnameT)
}

testthat::test_that("g.inspectfile works with a CSV.gz", {
  testthat::skip_on_cran()

  Ax3CsvFile  = system.file("testfiles/ax3_testfile_unix_timestamps.csv", package = "GGIR")[1]
  Ax6CsvFile  = system.file("testfiles/ax6_testfile_formatted_timestamps.csv", package = "GGIR")[1]

  testthat::expect_no_error({
    ax3out = g.inspectfile(Ax3CsvFile)
  })
  testthat::expect_no_error({
    ax6out = g.inspectfile(Ax6CsvFile)
  })

  testthat::expect_no_error({
    ax3out_gz = g.inspectfile(quick_gzip(Ax3CsvFile))
  })
  testthat::expect_no_error({
    ax6out_gz = g.inspectfile(quick_gzip(Ax6CsvFile))
  })

  ax3out$filename = ax3out_gz$filename =  NULL
  ax6out$filename = ax6out_gz$filename =  NULL

  testthat::expect_equal(ax3out, ax3out_gz)
  testthat::expect_equal(ax6out, ax6out_gz)
})
