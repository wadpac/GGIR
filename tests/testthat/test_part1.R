library(GGIR)
context("Part 1 function")
test_that("Different routines of part 1 work properly", {
  skip_on_cran()
  
  # 0) Generate data for tests -----------------------------------------
  # Using sf = 8 Hertz because metric zc filters at 4 Hertz 
  create_test_acc_csv(Nmin = 720, sf = 8)
  fn = "123A_testaccfile.csv"
  metadatadir = paste0(getwd(), "/output_test")
  desiredtz = "Europe/London"
  dn = "output_test"
  if (file.exists(dn))  unlink(dn, recursive = TRUE)
  
  # 1) various metrics + external function + select days file -------
  # external function
  exampleExtFunction = function(data=c(), parameters=c()) {
    data = data.frame(data, agglevel = round((1:nrow(data)) / (30 * 60 * 15)))
    output = aggregate(data, by = list(data$agglevel), FUN = mean)
    output = output[, -c(1, 2, ncol(output))]
    return(output)
  }
  myfun =  list(FUN = exampleExtFunction,
                parameters = 1.1,
                expected_sample_rate = 30, # resample data to 30 Hertz before applying function
                expected_unit = "mg",
                minlength = 15,
                outputres = 15,
                colnames = c("A", "B", "C"),
                outputtype = "numeric", #"numeric" (averaging is possible), "category" (majority vote)
                aggfunction = mean,
                timestamp = as.numeric(Sys.time())) # for unit test only
  
  # create selectdaysfile
  SDF = matrix("", 1, 3)
  SDF[1, 1] = "MOS2D12345678"
  SDF[1, 2:3] =  c("23-05-2016", "24-05-2016")
  colnames(SDF) = c("Monitor", "Day1", "Day2")
  selectdaysfile = "selectdaysfile.csv"
  write.csv(SDF, file = selectdaysfile)
  
  # run part 1
  g.part1(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1, overwrite = TRUE, desiredtz = desiredtz,
          do.cal = FALSE, windowsizes = c(60,3600,3600),
          # We are not doing all the metrics, because Travis-CI cannot allocate enough memory
          myfun = myfun, do.anglex = TRUE, do.angley = TRUE,
          do.en = TRUE, do.enmo = TRUE,do.lfenmo = TRUE,
          do.bfen = TRUE, do.hfenplus = TRUE,
          do.mad = TRUE, do.zcx = TRUE, #do.brondcounts = TRUE,
          do.bfx = TRUE, do.bfy = TRUE, do.bfz = TRUE, do.hfen = TRUE,
          do.hfx = TRUE, do.hfy = TRUE, do.hfz = TRUE, do.lfen = TRUE,
          do.roll_med_acc_x = TRUE, do.roll_med_acc_y = TRUE, do.roll_med_acc_z = TRUE,
          do.dev_roll_med_acc_x = TRUE, do.dev_roll_med_acc_y = TRUE, do.dev_roll_med_acc_z = TRUE,
          do.enmoa = TRUE, 
          verbose = FALSE)
  
  # tests
  rn = dir("output_test/meta/basic/",full.names = TRUE)
  load(rn[1])
  expect_equal(ncol(M$metashort), 29)
  expect_true(nrow(M$metashort) == 2820)
  expect_equal(mean(M$metashort$B, na.rm = T), 24.673, tolerance = 3)
  expect_equal(mean(M$metashort$C, na.rm = T), -6.642, tolerance = 3)
  expect_equal(mean(M$metashort$BFEN),  0.0458, tolerance = 4)
  expect_equal(mean(M$metashort$LFENMO),  0.0447, tolerance = 4)
  expect_equal(mean(M$metashort$HFENplus),  0.0914, tolerance = 4)
  expect_equal(mean(M$metashort$MAD),  0.0073, tolerance = 4)
  expect_equal(mean(M$metashort$anglex),  57.4683, tolerance = 4)
  expect_equal(mean(M$metashort$anglez),  0.3522, tolerance = 4)
  expect_equal(mean(M$metashort$ZCX),  14.94, tolerance = 2)
  expect_equal(mean(M$metashort$EN, na.rm = T), 1.029, tolerance = 3)
  expect_equal(mean(M$metashort$angley, na.rm = T), 0.765, tolerance = 3)
  expect_equal(mean(M$metashort$roll_med_acc_x, na.rm = T), 0.729, tolerance = 3)
  expect_equal(mean(M$metashort$roll_med_acc_z, na.rm = T), 0.007, tolerance = 3)
  expect_equal(mean(M$metashort$dev_roll_med_acc_x, na.rm = T), 0.007, tolerance = 3)
  expect_equal(mean(M$metashort$ENMOa, na.rm = T), 0.03, tolerance = 3)
  
  # 2) Detect nonwear and clipping ------------------
  data = as.matrix(read.csv(fn, skip = 10))
  
  # 2013 algorithm ------
  # clipthres to 1.7 to test the clipping detection
  NWCW = detect_nonwear_clipping(data = data, nonwear_approach = "2013", 
                                 sf = 8, clipthres = 1.7)
  CW = NWCW$CWav; NW = NWCW$NWav
  NW_rle_2013 = rle(NW)
  CW = sum(NWCW$CWav > 0)
  
  # 2023 algorithm ------
  NWCW = detect_nonwear_clipping(data = data, nonwear_approach = "2023", sf = 8)
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
 
  # end ----------
  if (file.exists(fn)) file.remove(fn)
  if (file.exists(selectdaysfile)) file.remove(selectdaysfile)
  if (dir.exists("output_test"))  unlink("output_test", recursive = TRUE)
})
