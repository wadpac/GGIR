library(GGIR)
context("shor functions only used in MCS (Milennium COhort study)")
test_that("MCS functions still produce same output", {
  
  # These functions were written by Joe Heywood around 2016
  # and only used for Milennium cohort data
  # The function include hard coded references to timezone Europe/London 
  # and are not well documented
  # possibly we can remove them from the package at some point
  # or revise the functionality to be more generally useful.
  # For now, it seems good to have at least some unit tests in place:
  
  GAfile  = system.file("testfiles/GENEActiv_testfile.bin", package = "GGIR")[1]
  d = "30/4/2013"
  options(warn=-1)
  hhr = GENEAread::header.info(binfile = GAfile)
  out = getStartEndNumeric(d, hhr, startHour = 20) 
  options(warn=0)
  expect_equal(nrow(out), 1)
  expect_equal(out[1,2], 1)
  
  options(warn=-1)
  out2 = getFirstTimestamp(f=GAfile, p1=100)
  options(warn=0)
  expect_equal(class(out2)[1], "POSIXct")
  
  out3 = getStartEnd(d, startHour = 2)
  expect_equal(nrow(out3), 1)
  expect_equal(out3[1,2], "01/05/2013 02:00:00")
})
 