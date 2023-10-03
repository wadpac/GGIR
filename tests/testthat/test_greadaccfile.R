library(GGIR)
context("g.readaccfile")
test_that("g.readaccfile and g.inspectfile can read gt3x, cwa, and actigraph csv files correctly", {
  skip_on_cran()
  
  cat("\nActigraph .csv")
  for (filename in c("ActiGraph13.csv", "ActiGraph61.csv")) {
    csvfile = system.file(paste0("testfiles/", filename), package = "GGIR")[1]

    Icsv = g.inspectfile(csvfile, desiredtz = "Europe/London")
    expect_equal(Icsv$monc, MONITOR$ACTIGRAPH)
    expect_equal(Icsv$dformc, FORMAT$CSV)
  }

  cwafile  = system.file("testfiles/ax3_testfile.cwa", package = "GGIRread")[1]
  GAfile  = system.file("testfiles/GENEActiv_testfile.bin", package = "GGIRread")[1]
  gt3xfile  = system.file("testfiles/actigraph_testfile.gt3x", package = "GGIR")[1]
  
  desiredtz = "Europe/London"
  params_rawdata = list(frequency_tol = 0.1, interpolationType = 1,
                        desiredtz = "Europe/London", configtz = "Europe/London")
  cat("\nActigraph .gt3x")
  # actigraph .gt3x
  Igt3x = g.inspectfile(gt3xfile, desiredtz = desiredtz)
  expect_equal(Igt3x$monc, 3)
  expect_equal(Igt3x$dformc, 6)
  expect_equal(Igt3x$sf, 30)
  EHV = g.extractheadervars(Igt3x)
  expect_equal(EHV$deviceSerialNumber, "MOS2E39180594_firmware_1.9.2")
  Mgt3x = g.getmeta(datafile = gt3xfile, desiredtz = desiredtz, windowsize = c(1,300,300),
                    inspectfileobject = Igt3x)
  expect_true(Mgt3x$filetooshort)
  expect_false(Mgt3x$filecorrupt)
  cat("\nAxivity .cwa")
  # axivity .cwa
  Icwa = g.inspectfile(cwafile, desiredtz = desiredtz, params_rawdata = params_rawdata)
  expect_equal(Icwa$monc, 4)
  expect_equal(Icwa$dformc, 4)
  expect_equal(Icwa$sf, 100)
  EHV = g.extractheadervars(Icwa)
  expect_equal(EHV$deviceSerialNumber,"39434")
  Mcwa = g.getmeta(cwafile, desiredtz = desiredtz, windowsize = c(1,300,300),
                   inspectfileobject = Icwa)
  expect_true(Mcwa$filetooshort)
  expect_false(Mcwa$filecorrupt)
 
  cat("\nGENEActiv .bin")
  # GENEActiv .bin
  IGA = g.inspectfile(GAfile, desiredtz = desiredtz)
  expect_equal(IGA$monc,2)
  expect_equal(IGA$dformc,1)
  expect_equal(IGA$sf,85.7)

  EHV = g.extractheadervars(IGA)
  expect_equal(EHV$deviceSerialNumber,"012967")
  MGA = g.getmeta(GAfile, desiredtz = desiredtz, windowsize = c(1,300,300), verbose = FALSE,
                  inspectfileobject = IGA)
  expect_true(MGA$filetooshort)
  
  
  # test decimal separator recognition extraction
  decn =  g.dotorcomma(cwafile,dformat = 4, mon = 4, desiredtz = desiredtz)
  expect_equal(decn,".")
  decn =  g.dotorcomma(GAfile, dformat = 1, mon = 2, desiredtz = desiredtz)
  expect_equal(decn,".")
  filequality = list(filecorrupt = FALSE, filetooshort = FALSE)
  dayborder = 0
  
  cwa_read = g.readaccfile(cwafile, blocksize = 10, blocknumber = 1, filequality = filequality,
                           dayborder,ws = 3, desiredtz = desiredtz, 
                           PreviousEndPage = 1, inspectfileobject = Icwa,
                           params_rawdata = params_rawdata)
  GA_read = g.readaccfile(GAfile, blocksize = 2, blocknumber = 1, filequality = filequality,
                          dayborder = dayborder, ws = 3,
                          desiredtz = desiredtz, PreviousEndPage = 1, inspectfileobject = IGA)
  expect_equal(cwa_read$P$header$blocks, 145)
  expect_equal(round(cwa_read$P$data[200, 6], digits = 4), 0)
  
  # As of R 4.0, an extra header row is extracted, which affects the positioning of the values.
  # expect_equal(as.numeric(as.character(wav_read$P$header$hvalues[7])),17) 
  expect_equal(round(sum(GA_read$P$data.out[, 2:4]), digits = 2), -467.59)
  # print(GA_read$P$header)
  # expect_equal(as.character(unlist(GA_read$P$header[3, 1])), "216 Hours")
  
  #also test one small other function:
  datadir  = system.file("testfiles", package = "GGIR")[1]
  fnames = datadir2fnames(datadir = datadir, filelist = FALSE)
  expect_equal(length(fnames$fnames), 6)
  expect_equal(length(fnames$fnamesfull), 6)
})
