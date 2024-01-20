library(GGIR)
context("g.readaccfile")
test_that("g.readaccfile and g.inspectfile can read movisens, gt3x, cwa, Axivity csv, and actigraph csv files correctly", {
  skip_on_cran()
  
  desiredtz = "Pacific/Auckland"
  configtz = "Europe/Berlin"
  params = extract_params(input = list(frequency_tol = 0.1, interpolationType = 1,
                                       desiredtz = desiredtz, configtz = configtz))
  params_rawdata = params$params_rawdata
  params_general = params$params_general

  filequality = list(filecorrupt = FALSE, filetooshort = FALSE)
  dayborder = 0

  # For Axivity csv files, we'll be able to read files with both unix and formatted (Y-M-D h:m:s) timestamps
  Ax3CsvFile  = system.file("testfiles/ax3_testfile_unix_timestamps.csv", package = "GGIR")[1]
  Ax6CsvFile  = system.file("testfiles/ax6_testfile_formatted_timestamps.csv", package = "GGIR")[1]

  cwafile  = system.file("testfiles/ax3_testfile.cwa", package = "GGIRread")[1]
  GAfile  = system.file("testfiles/GENEActiv_testfile.bin", package = "GGIRread")[1]
  gt3xfile  = system.file("testfiles/actigraph_testfile.gt3x", package = "GGIR")[1]
  
  cat("\nActigraph .csv")

  create_test_acc_csv()
  filename = "123A_testaccfile.csv"
  on.exit({if (file.exists(filename)) file.remove(filename)}, add = TRUE)

  Icsv = g.inspectfile(filename, desiredtz = desiredtz)
  expect_equal(Icsv$monc, MONITOR$ACTIGRAPH)
  expect_equal(Icsv$dformc, FORMAT$CSV)

  csv_read = g.readaccfile(filename, blocksize = 10, blocknumber = 1, filequality = filequality,
                           dayborder = dayborder, ws = 3,
                           PreviousEndPage = 1, inspectfileobject = Icsv,
                           params_rawdata = params_rawdata, params_general = params_general)
  expect_equal(nrow(csv_read$P$data), 3000)
  expect_false(csv_read$filequality$filecorrupt)
  expect_false(csv_read$filequality$filetooshort)
  expect_equal(sum(csv_read$P$data), 3151.11, tolerance = .01, scale = 1)

  cat("\nActigraph .gt3x")
  # actigraph .gt3x
  Igt3x = g.inspectfile(gt3xfile, desiredtz = desiredtz)
  expect_equal(Igt3x$monc, MONITOR$ACTIGRAPH)
  expect_equal(Igt3x$dformc, FORMAT$GT3X)
  expect_equal(Igt3x$sf, 30)
  EHV = g.extractheadervars(Igt3x)
  expect_equal(EHV$deviceSerialNumber, "MOS2E39180594_firmware_1.9.2")

  gt3x_read = g.readaccfile(gt3xfile, blocksize = 3000, blocknumber = 1, filequality = filequality,
                            dayborder = dayborder, ws = 3,
                            PreviousEndPage = 1, inspectfileobject = Igt3x,
                            params_rawdata = params_rawdata, params_general = params_general)
  expect_equal(nrow(gt3x_read$P$data), 17640)
  expect_false(gt3x_read$filequality$filecorrupt)
  expect_false(gt3x_read$filequality$filetooshort)
  expect_equal(sum(gt3x_read$P$data[c("x","y","z")]), 2732.35, tolerance = .01, scale = 1)

  Mgt3x = g.getmeta(datafile = gt3xfile, desiredtz = desiredtz, windowsize = c(1,300,300),
                    inspectfileobject = Igt3x)
  expect_true(Mgt3x$filetooshort)
  expect_false(Mgt3x$filecorrupt)
  
  cat("\nAxivity .cwa")
  # axivity .cwa
  Icwa = g.inspectfile(cwafile, params_rawdata = params_rawdata, params_general = params_general)
  expect_equal(Icwa$monc, MONITOR$AXIVITY)
  expect_equal(Icwa$dformc, FORMAT$CWA)
  expect_equal(Icwa$sf, 100)
  EHV = g.extractheadervars(Icwa)
  expect_equal(EHV$deviceSerialNumber,"39434")

  cwa_read = g.readaccfile(cwafile, blocksize = 10, blocknumber = 1, filequality = filequality,
                           dayborder = dayborder, ws = 3, 
                           PreviousEndPage = 1, inspectfileobject = Icwa,
                           params_rawdata = params_rawdata, params_general = params_general)
  expect_equal(cwa_read$P$header$blocks, 145)
  expect_equal(sum(cwa_read$P$data[c("x","y","z")]), 280.53, tolerance = .01, scale = 1)

  Mcwa = g.getmeta(cwafile, desiredtz = desiredtz, windowsize = c(1,300,300),
                   inspectfileobject = Icwa)
  expect_true(Mcwa$filetooshort)
  expect_false(Mcwa$filecorrupt)

  cat("\nAxivity .csv")

  for (csvData in list(list(Ax3CsvFile, 2881, 2370.08), list(Ax6CsvFile, 2875, 1064.66))) {
    IAxivityCsv = g.inspectfile(csvData[[1]], params_rawdata = params_rawdata, params_general = params_general)
    expect_equal(IAxivityCsv$monc, MONITOR$AXIVITY)
    expect_equal(IAxivityCsv$dformc, FORMAT$CSV)

    csv_read = g.readaccfile(csvData[[1]], blocksize = 10, blocknumber = 1, filequality = filequality,
                             dayborder = dayborder, ws = 3, 
                             PreviousEndPage = 1, inspectfileobject = IAxivityCsv,
                             params_rawdata = params_rawdata, params_general = params_general)

    # For both ax3 and ax6 files, we expect 4 columns: timestamp and XYZ.
    # All gyro data in ax6 files gets ignored.
    expect_equal(ncol(csv_read$P$data), 4)
    
    expect_equal(nrow(csv_read$P$data), csvData[[2]])
    expect_false(csv_read$filequality$filecorrupt)
    expect_false(csv_read$filequality$filetooshort)
    expect_equal(sum(csv_read$P$data[c("x","y","z")]), csvData[[3]], tolerance = .01, scale = 1)

    MAxCsv = g.getmeta(datafile = Ax3CsvFile, desiredtz = desiredtz, windowsize = c(1,300,300),
                      inspectfileobject = IAxivityCsv)
    expect_true(MAxCsv$filetooshort)
    expect_false(MAxCsv$filecorrupt)   
  }

  cat("\nGENEActiv .bin")
  # GENEActiv .bin
  IGA = g.inspectfile(GAfile, desiredtz = desiredtz)
  expect_equal(IGA$monc, MONITOR$GENEACTIV)
  expect_equal(IGA$dformc, FORMAT$BIN)
  expect_equal(IGA$sf,85.7)

  EHV = g.extractheadervars(IGA)
  expect_equal(EHV$deviceSerialNumber,"012967")

  GA_read = g.readaccfile(GAfile, blocksize = 2, blocknumber = 1, filequality = filequality,
                          dayborder = dayborder, ws = 3,
                          desiredtz = desiredtz, PreviousEndPage = 1, inspectfileobject = IGA)
  
  # As of R 4.0, an extra header row is extracted, which affects the positioning of the values.
  # expect_equal(as.numeric(as.character(wav_read$P$header$hvalues[7])),17) 
  expect_equal(round(sum(GA_read$P$data[, 2:4]), digits = 2), -467.59)
  # print(GA_read$P$header)
  # expect_equal(as.character(unlist(GA_read$P$header[3, 1])), "216 Hours")

  MGA = g.getmeta(GAfile, desiredtz = desiredtz, windowsize = c(1,300,300), verbose = FALSE,
                  inspectfileobject = IGA)
  expect_true(MGA$filetooshort)

  cat("\n Movisens")

  output_dir = "output_unisensExample"
  on.exit({if (file.exists(output_dir)) unlink(output_dir, recursive = TRUE)}, add = TRUE)
  if (file.exists(output_dir)) unlink(output_dir, recursive = TRUE)

  zip_file = "0.3.4.zip"
  on.exit({if (file.exists(zip_file)) unlink(zip_file)}, add = TRUE)
  if (!file.exists(zip_file)) {
    # link to a tagged release of Unisens/unisensR github repo
    movisens_url = "https://github.com/Unisens/unisensR/archive/refs/tags/0.3.4.zip"
    download.file(url = movisens_url, destfile = zip_file)
  }
  
  movisens_dir = "unisensR-0.3.4"
  on.exit({if (file.exists(movisens_dir)) unlink(movisens_dir, recursive = TRUE)}, add = TRUE)
  if (file.exists(movisens_dir)) {
    unlink(movisens_dir, recursive = TRUE)
  }
  unzip(zipfile = zip_file, exdir = ".")
  movisensFile = file.path(getwd(), "unisensR-0.3.4/tests/unisensExample/acc.bin")

  Mcsv = g.inspectfile(movisensFile, desiredtz = desiredtz)
  expect_equal(Mcsv$monc, MONITOR$MOVISENS)
  expect_equal(Mcsv$dformc, FORMAT$BIN)
  expect_equal(Mcsv$sf, 64)

  movisens_blocksize = 3000
  movisens_read = g.readaccfile(movisensFile, blocksize = movisens_blocksize, blocknumber = 1, filequality = filequality,
                                dayborder = dayborder, ws = 3,
                                PreviousEndPage = 1, inspectfileobject = Mcsv,
                                params_rawdata = params_rawdata, params_general = params_general)
  # for Movisens files, we'll read from startpage to endpage inclusive, so there will be blocksize+1 samples returned
  expect_equal(nrow(movisens_read$P$data), movisens_blocksize+1)
  expect_false(movisens_read$filequality$filecorrupt)
  expect_false(movisens_read$filequality$filetooshort)
  expect_equal(sum(movisens_read$P$data[c("x","y","z")]), 4385.29, tolerance = .01, scale = 1)
  expect_equal(movisens_read$endpage, movisens_blocksize + 1)

  # read the next block (set PreviousEndPage to movisens_read$endpage)
  movisens_read2 = g.readaccfile(movisensFile, blocksize = movisens_blocksize, blocknumber = 2, filequality = filequality,
                                dayborder = dayborder, ws = 3,
                                PreviousEndPage = movisens_read$endpage, inspectfileobject = Mcsv,
                                params_rawdata = params_rawdata, params_general = params_general)
  expect_equal(nrow(movisens_read2$P$data), movisens_blocksize+1)
  expect_equal(movisens_read2$endpage, movisens_blocksize * 2 + 2)
  
  # if the 1st sample of 2nd block is identical to the last sample of the 1st block,
  # this means that we calculated the startpage of the 2nd block incorrectly.
  expect_false(any(movisens_read2$P$data[1,] == movisens_read$P$data[nrow(movisens_read$P$data),]))

  # ad-hoc csv file

  # Create test file: No header, with temperature, with time.
  # The first row of the file will contain column names.
  # Have this file contain 6000 samples. We'll read it in 2 sets of 3000 lines,
  # and if there are any lines unnecessarily skipped, then the second attempt to 
  # read a block of 3000 will return fewer than 3000 lines.
  N = 6000
  sf = 30
  x = Sys.time()+((0:(N-1))/sf)
  timestamps = as.POSIXlt(x, origin="1970-1-1", tz = configtz)
  mydata = data.frame(Xcol = rnorm(N), timecol = timestamps, Ycol = rnorm(N), Zcol = rnorm(N),
            tempcol = rnorm(N) + 20)
  testfile = "testcsv1.csv"
  on.exit({if (file.exists(testfile)) file.remove(testfile)}, add = TRUE)

  write.csv(mydata, file = testfile, row.names = FALSE)

  AHcsv = g.inspectfile(testfile, 
                        rmc.dec=".", rmc.sf=30, rmc.unit.time="POSIX",
                        rmc.firstrow.acc = 1, rmc.firstrow.header=c(),
                        rmc.col.acc = c(1,3,4), rmc.col.temp = 5, rmc.col.time=2,
                        rmc.unit.acc = "g", rmc.unit.temp = "C", rmc.origin = "1970-01-01")

  expect_equal(AHcsv$monc, MONITOR$AD_HOC)
  expect_equal(AHcsv$dformc, FORMAT$AD_HOC_CSV)
  expect_equal(AHcsv$sf, 30)

  # Read the file starting with row 1 (rmc.firstrow.acc = 1); this row contains column names.
  # Verify that full 3000 rows are still read.
  csv_read = g.readaccfile(testfile, blocksize = 10, blocknumber = 1, filequality = filequality, # blocksize is # of pages of 300 samples
                           dayborder = dayborder, ws = 3, desiredtz = desiredtz, configtz = configtz,
                           PreviousEndPage = c(), inspectfileobject = AHcsv,
                           rmc.dec=".", rmc.sf=30, rmc.unit.time="POSIX",
                           rmc.firstrow.acc = 1, rmc.firstrow.header=c(),
                           rmc.col.acc = c(1,3,4), rmc.col.temp = 5, rmc.col.time=2,
                           rmc.unit.acc = "g", rmc.unit.temp = "C", rmc.origin = "1970-01-01")
  
  expect_equal(nrow(csv_read$P$data), 3000)
  expect_false(csv_read$filequality$filecorrupt)
  expect_false(csv_read$filequality$filetooshort)
  expect_equal(sum(csv_read$P$data[c("x","y","z")]), -38.90, tolerance = .01, scale = 1)
  # endpage should be (# of rows read + 1) because for the next block we'll need to skip
  # not just the rows read but also the row containing column names.
  expect_equal(csv_read$endpage, 3001)
  
  # since the 1st row of the file contains column names, pointing rmc.firstrow.acc 2
  # should lead to the same eaxt 3000 lines being read (the lines after the column names).
  csv_read2 = g.readaccfile(testfile, blocksize = 10, blocknumber = 1, filequality = filequality,
                           dayborder = dayborder, ws = 3, desiredtz = desiredtz, configtz = configtz,
                           PreviousEndPage = c(), inspectfileobject = AHcsv,
                           rmc.dec=".", rmc.sf=30, rmc.unit.time="POSIX",
                           rmc.firstrow.acc = 2, rmc.firstrow.header=c(),
                           rmc.col.acc = c(1,3,4), rmc.col.temp = 5, rmc.col.time=2,
                           rmc.unit.acc = "g", rmc.unit.temp = "C", rmc.origin = "1970-01-01")

  expect_equal(nrow(csv_read2$P$data), 3000)
  expect_false(csv_read2$filequality$filecorrupt)
  expect_false(csv_read2$filequality$filetooshort)
  expect_equal(sum(csv_read2$P$data[c("x","y","z")]), sum(csv_read$P$data[c("x","y","z")]), tolerance = .01, scale = 1)
  expect_equal(csv_read2$endpage, 3000)

  # reading the next 3000 lines should also give the same result for rmc.firstrow.acc == 1 or 2.
  csv_read3 = g.readaccfile(testfile, blocksize = 10, blocknumber = 2, filequality = filequality,
                           dayborder = dayborder, ws = 3, desiredtz = desiredtz, configtz = configtz,
                           PreviousEndPage = csv_read$endpage, inspectfileobject = AHcsv,
                           rmc.dec=".", rmc.sf=30, rmc.unit.time="POSIX",
                           rmc.firstrow.acc = 1, rmc.firstrow.header=c(),
                           rmc.col.acc = c(1,3,4), rmc.col.temp = 5, rmc.col.time=2,
                           rmc.unit.acc = "g", rmc.unit.temp = "C", rmc.origin = "1970-01-01")

  expect_equal(nrow(csv_read3$P$data), 3000)

  csv_read4 = g.readaccfile(testfile, blocksize = 10, blocknumber = 2, filequality = filequality,
                           dayborder = dayborder, ws = 3, desiredtz = desiredtz, configtz = configtz,
                           PreviousEndPage = csv_read2$endpage, inspectfileobject = AHcsv,
                           rmc.dec=".", rmc.sf=30, rmc.unit.time="POSIX",
                           rmc.firstrow.acc = 2, rmc.firstrow.header=c(),
                           rmc.col.acc = c(1,3,4), rmc.col.temp = 5, rmc.col.time=2,
                           rmc.unit.acc = "g", rmc.unit.temp = "C", rmc.origin = "1970-01-01")

  expect_equal(nrow(csv_read4$P$data), 3000)
  expect_equal(sum(csv_read3$P$data[c("x","y","z")]), sum(csv_read4$P$data[c("x","y","z")]), tolerance = .01, scale = 1)

  # test decimal separator recognition extraction
  decn =  g.dotorcomma(cwafile,dformat = FORMAT$CWA, mon = MONITOR$AXIVITY, desiredtz = desiredtz)
  expect_equal(decn,".")
  decn =  g.dotorcomma(GAfile, dformat = FORMAT$BIN, mon = MONITOR$GENEACTIV, desiredtz = desiredtz)
  expect_equal(decn,".")
    
  #also test one small other function:
  datadir  = system.file("testfiles", package = "GGIR")[1]
  fnames = datadir2fnames(datadir = datadir, filelist = FALSE)
  expect_equal(length(fnames$fnames), 8)
  expect_equal(length(fnames$fnamesfull), 8)
})
