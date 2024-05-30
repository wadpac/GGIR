library(GGIR)
testthat::context("g.inspectfile CSV")

quick_gzip = function(filename) {
  fext = strsplit(filename, "[.]")[[1]]
  fext = fext[length(fext)]
  destnameT = tempfile(fileext = paste0(".", fext, ".gz"))
  x = readLines(filename, warn = FALSE)
  gzcon = gzfile(destnameT, "w");
  on.exit({
    try({close(gzcon)})
  })
  write(x, gzcon);
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
