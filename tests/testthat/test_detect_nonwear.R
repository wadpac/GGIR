library(GGIR)
context("detect_nonwear_clipping")
test_that("the wear column is processed correctly", {
  skip_on_cran()

  N = 3000
  sf = 10

  op = options(digits.secs = 4)
  on.exit(options(op))
  
  timeseq = ((0:(N - 1))/sf)

  set.seed(100)
  accx = rnorm(N)
  set.seed(200)
  accy = rnorm(N)
  set.seed(300)
  accz = rnorm(N)
  wear = c(rep(TRUE,N/3),rep(FALSE,N/4),rep(TRUE,N/3),rep(FALSE,N/12))

  # create a csv file with a wear channel
  S = data.frame(x = accx, y = accy, z = accz, time = timeseq, wear = wear, stringsAsFactors = TRUE)
  testfile = "testcsv.csv"
  write.csv(S, file = testfile, row.names = FALSE)
  on.exit({if (file.exists(testfile)) file.remove(testfile)}, add = TRUE)


  I_obj = g.inspectfile(testfile,
                        rmc.dec = ".",
                        rmc.firstrow.acc = 1, rmc.firstrow.header = c(), rmc.unit.time = "UNIXsec",
                        rmc.col.acc = 1:3, rmc.col.temp = c(), rmc.col.time = 4,
                        rmc.sf = sf, desiredtz = "",
                        rmc.col.wear = 5)

  filequality = data.frame(filetooshort = FALSE, filecorrupt = FALSE,
                           filedoesnotholdday = FALSE, NFilePagesSkipped = 0)
  
  M_obj = g.getmeta(datafile = testfile, desiredtz = "", windowsizes = c(1,60,120),
                    inspectfileobject = I_obj,
                    rmc.dec = ".",
                    rmc.firstrow.acc = 1, rmc.firstrow.header = c(), rmc.unit.time = "UNIXsec",
                    rmc.col.acc = 1:3, rmc.col.temp = c(), rmc.col.time = 4,
                    rmc.sf = sf, desiredtz = "",
                    rmc.col.wear = 5)
  expect_equal(M_obj$metalong$nonwearscore, c(3,0,3,3,3))
})
