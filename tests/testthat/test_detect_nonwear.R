library(GGIR)
context("detect_nonwear_clipping")
test_that("detects non wear time", {
  skip_on_cran()
  
  # This will produce a 2-day long acc file with a 2-hour block of nonwear
  # starting at the 5th minute of every day
  Ndays = 2
  create_test_acc_csv(Nmin = Ndays*1440)
  
  data = as.matrix(read.csv("123A_testaccfile.csv", skip = 10))

  # 2013 algorithm ------
  # clipthres to 1.7 to test the clipping detection
  NWCW = detect_nonwear_clipping(data = data, nonwear_approach = "2013", 
                                 sf = 3, clipthres = 1.7)
  CW = NWCW$CWav; NW = NWCW$NWav
  NW_rle_2013 = rle(NW)
  CW = sum(NWCW$CWav > 0)
  
  # 2023 algorithm ------
  NWCW = detect_nonwear_clipping(data = data, nonwear_approach = "2023", sf = 3)
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
