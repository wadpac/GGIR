library(GGIR)
context("Lux handling in part 5")
test_that("lux_per_segment is correctly calculated", {
  skip_on_cran()
  
  # This test also covers: dayborder != 0 and part5_agg2_60seconds = TRUE
  
  # test data -----
  dn = "output_test"
  if (file.exists(dn)) unlink(dn, recursive = TRUE)
  create_part1_meta(desired_outputdir = "output_test",
                    Ndays = 2, windowsizes = c(5, 900, 3600),
                    lux = TRUE)

  # now run parts 2:5 -----------
  GGIR(mode = 2:5, datadir = "test", outputdir = getwd(), studyname = "test",
       overwrite = FALSE, do.report = c(2, 4, 5), verbose = FALSE,
       LUXthresholds = c(0, 500, 1000), 
       LUX_cal_constant = 1, 
       LUX_cal_exponent = 0.2, 
       LUX_day_segments = c(9, 15, 24), 
       dayborder = 23, part5_agg2_60seconds = TRUE,
       save_ms5rawlevels = TRUE, save_ms5raw_without_invalid = FALSE,
       save_ms5raw_format = "RData", nonwear_approach = "2013",
       visualreport = FALSE)
  
  # tests -----
  p5_fn = paste(getwd(), "output_test", "results", 
                "part5_daysummary_WW_L40M100V400_T5A5.csv",
                sep = .Platform$file.sep)
  df = read.csv(p5_fn)
  
  expect_equal(length(which(grepl("^LUX", colnames(df)))), 22) # max and mean day, mean spt, mean mvpa, ranges, segments
  expect_equal(df$LUX_max_day, 4.2)
  expect_equal(df$LUX_mean_day, 1.6)
  expect_equal(df$LUX_mean_day_mvpa, 1.8)
  expect_equal(df$LUX_min_0_500_day, df$dur_day_min) #in this artificial data, all LUX values in the day are expected to be below 500
  
  # dayborder and ws3 = 60
  p5raw_fn = paste(getwd(), "output_test", "meta", 
                   "ms5.outraw", "40_100_400",
                   "123A_testaccfile_T5A5.RData",
                   sep = .Platform$file.sep)
  expect_true(file.exists(p5raw_fn))
  mdat = c()
  load(p5raw_fn)
  expect_equal(diff(mdat[1:2, "timenum"]), 60) #epoch = 60
  pm11 = grep("23:00:00", as.character(mdat$timestamp))[1]
  expect_equal(diff(mdat[(pm11 - 1):pm11, "window"]), 1) #dayborder = 23 (change in window at 23:00)
  
  outfolder = paste(getwd(), "output_test", sep = .Platform$file.sep)
  if (file.exists(outfolder))  unlink(outfolder, recursive = TRUE)
  if (file.exists(dn)) unlink(dn, recursive = TRUE)
})
