library(GGIR)
context("HASIB with count data and HorAngle algorithm")
test_that("Checks that HASIB with count data is calculated and HorAngle algorithm applied", {
  skip_on_cran()
  
  # test data -----
  Ndays = 2
  create_test_acc_csv(Nmin = Ndays*1440, sf = 10)
  fn = "123A_testaccfile.csv"
  desiredtz = "Europe/London"
  dn = "output_test"
  if (file.exists(dn)) unlink(dn, recursive = TRUE)

  #----------------------------------------------------------------------
  # Parts 3:4 with HorAngle and SIB with Sadeh1994
  # generate file with sf = 30 to calculate zcy for using Sadeh1994
  GGIR(mode = c(1:4), datadir = fn, outputdir = getwd(),
       studyname = "test", f0 = 1, f1 = 1,
       do.report = c(), do.visual = FALSE,
       overwrite = FALSE, visualreport = FALSE, 
       verbose = FALSE, storefolderstructure = TRUE,
       do.zcy = TRUE,
       do.anglex = TRUE,
       do.angley = TRUE,
       do.anglez = TRUE,
       HASIB.algo = "Sadeh1994",
       Sadeh_axis = "Y",
       HASPT.algo = "HorAngle",
       longitudinal_axis = NULL,
       sleepwindowType = "TimeInBed",
       sensor.location = "hip") # so that it also covers the identification of the longitudinal axis
  dirname = "output_test/meta/ms4.out/"
  rn = dir(dirname,full.names = TRUE)
  load(rn[1])
  expect_equal(unique(nightsummary$sleepparam), "Sadeh1994_ZC")
  expect_equal(unique(nightsummary$guider), "HorAngle")
  
})

