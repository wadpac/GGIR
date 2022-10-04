library(GGIR)
context("g.part1")
test_that("Part 1 can run with all metrics", {
  skip_on_cran()
  #=======================
  # Part 1 with all metrics
  Ndays = 2
  # Using sf = 30 Hertz  because metric zc filters at 4 Hertz and brondcounts requires 30 Hertz
  create_test_acc_csv(Nmin = Ndays * 1440, sf = 30)
  fn = "123A_testaccfile.csv"
  metadatadir = paste0(getwd(), "/output_test")
  desiredtz = "Europe/London"
  dn = "output_test"
  if (file.exists(dn))  unlink(dn, recursive = TRUE)
  g.part1(datadir = fn, outputdir = getwd(), f0 = 1, f1 = 1, overwrite = TRUE, desiredtz = desiredtz,
          studyname = "test", do.cal = FALSE, do.anglex = TRUE,
          # We are not doing all the metrics, because Travis-CI cannot allocate enough memory
          do.enmo = TRUE,do.lfenmo = TRUE,
          do.bfen = TRUE, do.hfenplus = TRUE,
          do.mad = TRUE, do.zcx = TRUE, do.brondcounts = TRUE,
   windowsizes = c(15,3600,3600), do.parallel = FALSE,
   minimumFileSizeMB = 0, expand_tail_max_hours = 2)
  rn = dir("output_test/meta/basic/",full.names = TRUE)
  load(rn[1])
  expect_equal(ncol(M$metashort), 12)
  expect_true(nrow(M$metashort) > 16000)
  expect_equal(mean(M$metashort$BFEN[1:11280]),  0.04581394, tolerance = 4)
  expect_equal(mean(M$metashort$LFENMO[1:11280]),  0.04465999, tolerance = 4)
  expect_equal(mean(M$metashort$HFENplus[1:11280]),  0.09142312, tolerance = 4)
  expect_equal(mean(M$metashort$MAD[1:11280]),  0.007271498, tolerance = 4)
  expect_equal(mean(M$metashort$anglex[1:11280]),  57.46836, tolerance = 4)
  expect_equal(mean(M$metashort$anglez[1:11280]),  0.3521823, tolerance = 4)
  expect_equal(mean(M$metashort$ZCX[1:11280]),  14.93954, tolerance = 2)
  expect_equal(sum(M$metashort$BrondCount_x), 17690)
  expect_equal(sum(M$metashort$BrondCount_y), 60971)
  expect_equal(sum(M$metashort$BrondCount_z), 957584)
  if (file.exists(fn)) file.remove(fn)
})
