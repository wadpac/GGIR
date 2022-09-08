library(GGIR)
context("g.part1")
test_that("Part 1 can run with all metrics", {
  skip_on_cran()
  #=======================
  # Part 1 with all metrics
  Ndays = 2
  # Using sf = 30 Hertz  because metric zc filters at 4 Hertz and brondcounts requires 30 Hertz
  create_test_acc_csv(Nmin = Ndays * 1440, sf = 30)
  create_test_sleeplog_csv()
  fn = "123A_testaccfile.csv"
  sleeplog_fn = "testsleeplogfile.csv"
  metadatadir = paste0(getwd(), "/output_test")
  desiredtz = "Europe/London"
  dn = "output_test"
  if (file.exists(dn))  unlink(dn, recursive = TRUE)
  g.part1(datadir = fn, outputdir = getwd(), f0 = 1, f1 = 1, overwrite = TRUE, desiredtz = desiredtz,
          studyname = "test", do.cal = FALSE,
          # We are not doing all the metrics, because Travis-CI cannot allocate enough memory
          do.enmo = TRUE,do.lfenmo = TRUE,
          do.bfen = TRUE, do.hfenplus = TRUE,
          do.mad = TRUE, do.zcx = TRUE, do.brondcounts = TRUE,
   windowsizes = c(15,3600,3600), do.parallel = FALSE,
   minimumFileSizeMB = 0)
  rn = dir("output_test/meta/basic/",full.names = TRUE)
  load(rn[1])
  expect_that(ncol(M$metashort),equals(11))
  expect_that(nrow(M$metashort),equals(11280))
  expect_that(round(mean(M$metashort$BFEN), digits = 4), equals(0.0458))
  expect_that(round(mean(M$metashort$LFENMO), digits = 4), equals(0.0447))
  expect_that(round(mean(M$metashort$HFENplus), digits = 4), equals(0.0914))
  expect_that(round(mean(M$metashort$MAD), digits = 4), equals(0.0073))
  expect_that(round(mean(M$metashort$ZCX), digits = 2), equals(14.94))
  expect_equal(sum(M$metashort$BrondCount_x), 17690)
  expect_equal(sum(M$metashort$BrondCount_y), 60971)
  expect_equal(sum(M$metashort$BrondCount_z), 957584)

  if (file.exists(fn)) file.remove(fn)
  if (file.exists(sleeplog_fn)) file.remove(sleeplog_fn)
})
