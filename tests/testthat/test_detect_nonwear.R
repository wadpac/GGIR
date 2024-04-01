library(GGIR)
context("detect_nonwear_clipping")
test_that("detects non wear time", {
  skip_on_cran()
  
  
  # This will produce a 2-day long acc file with a 2-hour block of nonwear
  # starting at the 5th minute of every day
  Ndays = 2
  sf = 3
  create_test_acc_csv(Nmin = Ndays * 1440, sf = sf)
  
  data = as.matrix(read.csv("123A_testaccfile.csv", skip = 10))
  colnames(data) = c("x", "y", "z")

  # 2013 algorithm ------
  # clipthres to 1.7 to test the clipping detection
  NWCW = detect_nonwear_clipping(data = data, nonwear_approach = "2013", 
                                 sf = sf, clipthres = 1.7)
  CW = NWCW$CWav; NW = NWCW$NWav
  NW_rle_2013 = rle(NW)
  CW = sum(NWCW$CWav > 0)

  # 2023 algorithm ------
  NWCW = detect_nonwear_clipping(data = data, nonwear_approach = "2023", sf = sf)
  NW = NWCW$NWav
  NW_rle_2023 = rle(NW)
  # tests ----------------
  # Does it find the 2 periods of nonwear?
  expect_equal(sum(NW_rle_2013$values == 3), 2)
  expect_equal(sum(NW_rle_2023$values == 3), 2)
  
  # Expect the 2023 algorithm finds more nonwear than the 2013
  total_nonwear_2013 = sum(NW_rle_2013$lengths[which(NW_rle_2013$values == 3)])
  total_nonwear_2023 = sum(NW_rle_2023$lengths[which(NW_rle_2023$values == 3)])
  expect_true(total_nonwear_2023 > total_nonwear_2013)
  
  # Expect six ws2 windows with some clipping (values over 1.7 in this test)
  expect_equal(CW, 6)
  
  # remove generated file ------
  if (file.exists("123A_testaccfile.csv")) file.remove("123A_testaccfile.csv")
})

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
