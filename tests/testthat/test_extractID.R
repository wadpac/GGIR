library(GGIR)
context("extractID")
test_that("extractID recognizes IDs correctly", {
  skip_on_cran()
  
  hvars = list(ID = "123A_testaccfile.csv",
               iID = "not extracted",
               IDd = "123A",
               HN = "not extracted",
               sensor.location = "not extracted",
               SX = "not available",
               deviceSerialNumber = "MOS2D12345678_firmware_NA")
  
  # idloc = 2
  ID = extractID(hvars, idloc = 2, fname = "123A_testaccfile.csv")
  expect_equal(ID, "123A")
  
  # idloc = 3
  ID = extractID(hvars, idloc = 3, fname = "123A_testaccfile.csv")
  expect_equal(ID, "123A_testaccfile.csv")
  
  hvars$ID = "123A-testaccfile.csv"
  ID = extractID(hvars, idloc = 3, fname = "123A_testaccfile.csv")
  expect_equal(ID, "123A")
  
  # idloc = 4
  ID = extractID(hvars, idloc = 4, fname = "123A_testaccfile.csv")
  expect_equal(ID, "123A")
  
  # idloc = 5
  ID = extractID(hvars, idloc = 5, fname = "123A testaccfile.csv")
  expect_equal(ID, "123A")
  
  # idloc = 6
  ID = extractID(hvars, idloc = 6, fname = "123A.testaccfile.csv")
  expect_equal(ID, "123A")
  
  # idloc = 7
  ID = extractID(hvars, idloc = 7, fname = "123A-testaccfile.csv")
  expect_equal(ID, "123A")
  
  # NAs
  hvars$ID = "NA"
  ID = extractID(hvars, idloc = 1, fname = "123A-testaccfile.csv")
  expect_equal(ID, "notextracted")
  
  hvars$iID = "NA"
  hvars$ID = "123A_testaccfile.csv"
  ID = extractID(hvars, idloc = 1, fname = "123A-testaccfile.csv")
  expect_equal(ID, "123A_testaccfile.csv")
  
  # no ID
  hvars$ID = hvars$IDd = NULL
  expect_warning(extractID(hvars, idloc = 1, 
                           fname = "123A-testaccfile.csv"),
                 regexp = "Unable to extract ID")
  
})
