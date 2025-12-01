library(GGIR)
context("g.readaccfile")
test_that("g.readaccfile and g.inspectfile can read movisens, gt3x, cwa, Axivity csv, actigraph csv, ad-hoc csv, and Parmay Matrix files correctly", {
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
  
  Ax3CwaFile  = system.file("testfiles/ax3_testfile.cwa", package = "GGIRread")[1]
  Ax6CwaFile  = system.file("testfiles/ax6_testfile.cwa", package = "GGIRread")[1]
  
  GAfile  = system.file("testfiles/GENEActiv_testfile.bin", package = "GGIRread")[1]
  gt3xfile  = system.file("testfiles/actigraph_testfile.gt3x", package = "GGIR")[1]
  mtxfile  = system.file("testfiles/mtx_25Hz_acc_HR.BIN", package = "GGIRread")[1]
  
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
  
  Icwa = g.inspectfile(Ax3CwaFile, params_rawdata = params_rawdata, params_general = params_general)
  expect_equal(Icwa$monc, MONITOR$AXIVITY)
  expect_equal(Icwa$dformc, FORMAT$CWA)
  expect_equal(Icwa$sf, 100)
  EHV = g.extractheadervars(Icwa)
  expect_equal(EHV$deviceSerialNumber,"39434")
  
  cwa_read = g.readaccfile(Ax3CwaFile, blocksize = 10, blocknumber = 1, filequality = filequality,
                           dayborder = dayborder, ws = 2, 
                           PreviousEndPage = 1, inspectfileobject = Icwa,
                           params_rawdata = params_rawdata, params_general = params_general)
  expect_equal(cwa_read$P$header$blocks, 145)
  expect_equal(sum(cwa_read$P$data[c("x","y","z")]), 280.53, tolerance = .01, scale = 1)
  
  Mcwa = g.getmeta(Ax3CwaFile, desiredtz = desiredtz, windowsize = c(1,300,300),
                   inspectfileobject = Icwa)
  expect_true(Mcwa$filetooshort)
  expect_false(Mcwa$filecorrupt)
  
  ax3_start_timestamp = cwa_read$P$data$time[1]
  
  Icwa = g.inspectfile(Ax6CwaFile, params_rawdata = params_rawdata, params_general = params_general)
  cwa_read = g.readaccfile(Ax6CwaFile, blocksize = 10, blocknumber = 1, filequality = filequality,
                           dayborder = dayborder, ws = 2, 
                           PreviousEndPage = 1, inspectfileobject = Icwa,
                           params_rawdata = params_rawdata, params_general = params_general)
  ax6_start_timestamp = cwa_read$P$data$time[1]
  
  cat("\nAxivity .csv")
  
  for (csvData in list(list(Ax3CsvFile, 200, -11.80, ax3_start_timestamp),
                       list(Ax6CsvFile, 200, 14.84, ax6_start_timestamp))) {
    IAxivityCsv = g.inspectfile(csvData[[1]], params_rawdata = params_rawdata, params_general = params_general)
    expect_equal(IAxivityCsv$monc, MONITOR$AXIVITY)
    expect_equal(IAxivityCsv$dformc, FORMAT$CSV)
    
    csv_read = g.readaccfile(csvData[[1]], blocksize = 1, blocknumber = 1, filequality = filequality,
                             dayborder = dayborder, ws = 1, 
                             PreviousEndPage = 1, inspectfileobject = IAxivityCsv,
                             params_rawdata = params_rawdata, params_general = params_general)
    
    # For both ax3 and ax6 files, we expect 4 columns: timestamp and XYZ.
    # All gyro data in ax6 files gets ignored.
    expect_equal(ncol(csv_read$P$data), 4)
    
    expect_equal(nrow(csv_read$P$data), csvData[[2]])
    expect_false(csv_read$filequality$filecorrupt)
    expect_false(csv_read$filequality$filetooshort)
    expect_equal(sum(csv_read$P$data[c("x","y","z")]), csvData[[3]], tolerance = .01, scale = 1)
    
    # check that the timestamps for the Axivity csv look the same as they did for
    # the original cwa version of the same file (this verifies that timestamp conversion
    # worked the same for both formats)
    expect_equal(csv_read$P$data$time[1], csvData[[4]], tolerance = .01, scale = 1)
    
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
  expect_equal(IGA$sf, 86)
  
  EHV = g.extractheadervars(IGA)
  expect_equal(EHV$deviceSerialNumber,"012967")
  
  GA_num_blocks = 2
  GA_read = g.readaccfile(GAfile, blocksize = GA_num_blocks, blocknumber = 1, filequality = filequality,
                          dayborder = dayborder, ws = 3,
                          desiredtz = desiredtz, PreviousEndPage = 1, inspectfileobject = IGA)
  
  # As of R 4.0, an extra header row is extracted, which affects the positioning of the values.
  # expect_equal(as.numeric(as.character(wav_read$P$header$hvalues[7])),17) 
  
  expect_equal(round(sum(GA_read$P$data[, 2:4]), digits = 2), -271.35)
  expect_equal(GA_read$endpage, GA_num_blocks)
  
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
    download.file(url = movisens_url, destfile = zip_file, quiet = TRUE)
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
  expect_equal(nrow(movisens_read$P$data), movisens_blocksize)
  expect_false(movisens_read$filequality$filecorrupt)
  expect_false(movisens_read$filequality$filetooshort)
  expect_equal(sum(movisens_read$P$data[c("x","y","z")]), 4383.67, tolerance = .01, scale = 1)
  expect_equal(movisens_read$endpage, movisens_blocksize)
  
  # read the next block (set PreviousEndPage to movisens_read$endpage)
  movisens_read2 = g.readaccfile(movisensFile, blocksize = movisens_blocksize, blocknumber = 2, filequality = filequality,
                                 dayborder = dayborder, ws = 3,
                                 PreviousEndPage = movisens_read$endpage, inspectfileobject = Mcsv,
                                 params_rawdata = params_rawdata, params_general = params_general)
  expect_equal(nrow(movisens_read2$P$data), movisens_blocksize)
  expect_equal(movisens_read2$endpage, movisens_blocksize * 2)
  
  # if the 1st sample of 2nd block is identical to the last sample of the 1st block,
  # this means that we calculated the startpage of the 2nd block incorrectly.
  expect_false(any(movisens_read2$P$data[1,] == movisens_read$P$data[nrow(movisens_read$P$data),]))
  cat("\n ad-hoc csv file")
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
  testfile = "testcsv.csv"
  on.exit({if (file.exists(testfile)) file.remove(testfile)}, add = TRUE)
  
  write.csv(mydata, file = testfile, row.names = FALSE)
  
  # check that for files with no header, g.inspectfile() errors out if sampling rate 
  # is not specified as rmc.sf, or if rmc.sf == 0
  expect_error(g.inspectfile(testfile, 
                             rmc.dec=".", rmc.unit.time="POSIX",
                             rmc.firstrow.acc = 1, rmc.firstrow.header=c(), 
                             rmc.col.acc = c(1,3,4), rmc.col.temp = 5, rmc.col.time=2,
                             rmc.unit.acc = "g", rmc.unit.temp = "C", rmc.origin = "1970-01-01"),
               regexp = "File header doesn't specify sample rate. Please provide rmc.sf value to process")
  expect_error(g.inspectfile(testfile, 
                             rmc.dec=".", rmc.sf=0, rmc.unit.time="POSIX",
                             rmc.firstrow.acc = 1, rmc.firstrow.header=c(),
                             rmc.col.acc = c(1,3,4), rmc.col.temp = 5, rmc.col.time=2,
                             rmc.unit.acc = "g", rmc.unit.temp = "C", rmc.origin = "1970-01-01"),
               regexp = "File header doesn't specify sample rate. Please provide a non-zero rmc.sf value to process")
  
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
  
  # Create test file: 2-column header, with time,
  # but sample rate not specified in the header
  
  N = 6000
  sf = 30
  x = Sys.time()+((0:(N-1))/sf)
  timestamps = as.POSIXlt(x, origin="1970-1-1", tz = configtz)
  mydata = data.frame(Xcol = rnorm(N), timecol = timestamps, Ycol = rnorm(N), Zcol = rnorm(N))
  S1 = as.matrix(mydata)
  
  hd_NR = 10
  hd = matrix("", hd_NR + 1, ncol(S1))
  hd[1, 1:2] = c("ID","12345")
  hd[2, 1:2] = c("serial_number","30")
  hd[3, 1:2] = c("bit","8")
  hd[4, 1:2] = c("dynamic_range","6")
  
  S1 = rbind(hd, S1)
  S1[hd_NR + 1,] = colnames(S1)
  colnames(S1) = NULL
  
  testfile_two_col = "testcsv2col.csv"
  on.exit({if (file.exists(testfile_two_col)) file.remove(testfile_two_col)}, add = TRUE)
  write.table(S1, file = testfile_two_col, col.names = FALSE, row.names = FALSE)
  
  # Create test file: 1-column header, with time,
  # but sample rate not specified in the header
  S1 = as.matrix(mydata)
  hd = matrix("", hd_NR + 1, ncol(S1))
  hd[1, 1:2] = c("ID: 12345", "")
  hd[2, 1:2] = c("serial_number: 4321", "")
  hd[3, 1:2] = c("bit: 8", "")
  hd[4, 1:2] = c("dynamic_range: 6", "")
  
  S1 = rbind(hd, S1)
  S1[hd_NR + 1,] = colnames(S1)
  colnames(S1) = NULL
  
  testfile_one_col = "testcsv1col.csv"
  on.exit({if (file.exists(testfile_one_col)) file.remove(testfile_one_col)}, add = TRUE)
  write.table(S1, file = testfile_one_col, col.names = FALSE, row.names = FALSE)
  
  for (csvData in list(list(testfile_one_col, ": "),
                       list(testfile_two_col, c()))) {
    # check that for a file whose header doesn't specify sampling rate,
    # g.inspectfile() errors out if sampling rate is not specified as rmc.sf, or if rmc.sf==0
    expect_error(g.inspectfile(csvData[[1]],
                               rmc.dec=".", rmc.unit.time="POSIX",
                               rmc.firstrow.acc = 11, rmc.firstrow.header = 1,
                               rmc.col.acc = c(1,3,4), rmc.col.time=2,
                               rmc.unit.acc = "g", rmc.origin = "1970-01-01",
                               rmc.header.structure = csvData[[2]]),
                 regexp = "File header doesn't specify sample rate. Please provide rmc.sf value to process")
    expect_error(g.inspectfile(csvData[[1]],
                               rmc.dec=".", rmc.sf = 0, rmc.unit.time="POSIX",
                               rmc.firstrow.acc = 11, rmc.firstrow.header = 1,
                               rmc.col.acc = c(1,3,4), rmc.col.time=2,
                               rmc.unit.acc = "g", rmc.origin = "1970-01-01",
                               rmc.header.structure = csvData[[2]]),
                 regexp = "File header doesn't specify sample rate. Please provide a non-zero rmc.sf value to process")
    
    # check that for a file whose header doesn't specify sampling rate,
    # g.inspectfile() returns sf == rmc.sf if the latter was specified
    I = g.inspectfile(csvData[[1]],
                      rmc.dec=".", rmc.sf = 80, rmc.unit.time="POSIX",
                      rmc.firstrow.acc = 11, rmc.firstrow.header = 1,
                      rmc.col.acc = c(1,3,4), rmc.col.time=2,
                      rmc.unit.acc = "g", rmc.origin = "1970-01-01",
                      rmc.header.structure = csvData[[2]])
    expect_equal(I$sf, 80)
  }
  
  # Create test file: 2-column header, with temperature, with time,
  # and sample rate correctly specified in the header
  S1 = as.matrix(mydata)
  hd_NR = 10
  hd = matrix("", hd_NR + 1, ncol(S1))
  hd[1, 1:2] = c("ID","12345")
  hd[2, 1:2] = c("sample_freq","40")
  hd[3, 1:2] = c("serial_number","9876")
  hd[4, 1:2] = c("bit","8")
  hd[5, 1:2] = c("dynamic_range","6")
  S1 = rbind(hd, S1)
  S1[hd_NR + 1,] = colnames(S1)
  colnames(S1) = NULL
  
  testfile_two_col = "testcsv2col.csv"
  on.exit({if (file.exists(testfile_two_col)) file.remove(testfile_two_col)}, add = TRUE)
  write.table(S1, file = testfile_two_col, col.names = FALSE, row.names = FALSE)
  
  # Create test file: 1-column header, with time,
  # and sample rate not specified in the header
  S1 = as.matrix(mydata)
  hd = matrix("", hd_NR + 1, ncol(S1))
  hd[1, 1:2] = c("ID: 12345", "")
  hd[2, 1:2] = c("sample_freq: 40", "")
  hd[3, 1:2] = c("serial_number: 4321", "")
  hd[4, 1:2] = c("bit: 8", "")
  hd[5, 1:2] = c("dynamic_range: 6", "")
  S1 = rbind(hd, S1)
  S1[hd_NR + 1,] = colnames(S1)
  colnames(S1) = NULL
  
  testfile_one_col = "testcsv1col.csv"
  on.exit({if (file.exists(testfile_one_col)) file.remove(testfile_one_col)}, add = TRUE)
  write.table(S1, file = testfile_one_col, col.names = FALSE, row.names = FALSE)
  
  for (csvData in list(list(testfile_one_col, ": "),
                       list(testfile_two_col, c()))) {
    # check that g.inspectfile() returns sf value that was specified in the header, even if rmc.sf was also specified
    I = g.inspectfile(csvData[[1]],
                      rmc.dec=".", rmc.sf = 80, rmc.headername.sf = "sample_freq",
                      rmc.unit.time="POSIX",
                      rmc.firstrow.acc = 11, rmc.firstrow.header=1,
                      rmc.col.acc = c(1,3,4), rmc.col.time=2,
                      rmc.unit.acc = "g", rmc.origin = "1970-01-01",
                      rmc.headername.sn = "serial_number",
                      rmc.headername.recordingid = "ID",
                      rmc.bitrate = "bit", rmc.dynamic_range = "dynamic_range",
                      rmc.header.structure = csvData[[2]])
    
    expect_equal(I$sf, 40)
    
    # check that g.inspectfile() correctly reads the sf value from the header
    I = g.inspectfile(csvData[[1]],
                      rmc.dec=".", rmc.headername.sf = "sample_freq",
                      rmc.unit.time="POSIX",
                      rmc.firstrow.acc = 11, rmc.firstrow.header=1,
                      rmc.col.acc = c(1,3,4), rmc.col.time=2,
                      rmc.unit.acc = "g", rmc.origin = "1970-01-01",
                      rmc.header.structure = csvData[[2]])
    expect_equal(I$sf, 40)
  }
  cat("\nParmay Matrix .BIN")
  
  Imtx = g.inspectfile(mtxfile, params_rawdata = params_rawdata, params_general = params_general)
  expect_equal(Imtx$monc, MONITOR$PARMAY_MTX)
  expect_equal(Imtx$dformc, FORMAT$BIN)
  expect_equal(Imtx$sf, 25)
  EHV = g.extractheadervars(Imtx)
  expect_equal(EHV$deviceSerialNumber,"not extracted") # not stored in file, but leave this here to facilitate implementation in case that in the future it is available
  
  mtx_read = g.readaccfile(mtxfile, blocksize = 10, blocknumber = 1, filequality = filequality,
                           dayborder = dayborder, ws = 2, 
                           PreviousEndPage = 1, inspectfileobject = Imtx,
                           params_rawdata = params_rawdata, params_general = params_general)
  expect_equal(nrow(mtx_read$P$QClog), 4) # 4 blocks of data in this file

  expect_equal(sum(round(mtx_read$P$data[c("x")], digits = 4)), 8546.728, tolerance = .1, scale = 1)
  expect_equal(sum(round(mtx_read$P$data[c("y")], digits = 4)), 10913.9, tolerance = .1, scale = 1)
  expect_equal(sum(round(mtx_read$P$data[c("z")], digits = 4)), -13005.89, tolerance = .1, scale = 1)

  Mmtx = g.getmeta(mtxfile, desiredtz = desiredtz, windowsize = c(1,300,300),
                   inspectfileobject = Imtx)
  expect_true(Mmtx$filetooshort)
  expect_false(Mmtx$filecorrupt)
  
  # test decimal separator recognition extraction
  decn =  g.dotorcomma(Ax3CwaFile,dformat = FORMAT$CWA, mon = MONITOR$AXIVITY, desiredtz = desiredtz)
  expect_equal(decn,".")
  decn =  g.dotorcomma(GAfile, dformat = FORMAT$BIN, mon = MONITOR$GENEACTIV, desiredtz = desiredtz)
  expect_equal(decn,".")
  
  #also test one small other function:
  datadir  = system.file("testfiles", package = "GGIR")[1]
  fnames = datadir2fnames(datadir = datadir, filelist = FALSE)
  expect_equal(length(fnames$fnames), 8)
  expect_equal(length(fnames$fnamesfull), 8)
  
  if (dir.exists("unisensR-0.3.4/")) unlink("unisensR-0.3.4/", recursive = TRUE)
  if (file.exists(testfile_one_col)) file.remove(testfile_one_col)
  if (file.exists(testfile_two_col)) file.remove(testfile_two_col)
  if (file.exists(testfile)) file.remove(testfile)
  if (file.exists(filename)) file.remove(filename)
  if (file.exists(zip_file)) file.remove(zip_file)
})
