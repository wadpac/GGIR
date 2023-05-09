library(GGIR)
library(GGIRread)
context("g.readaccfile")
test_that("g.readaccfile and g.inspectfile can read genea, gt3x and cwa files correctly", {
  skip_on_cran()
  
  cwafile  = system.file("testfiles/ax3_testfile.cwa", package = "GGIRread")[1]
  binfile  = system.file("testfiles/genea_testfile.bin", package = "GGIRread")[1]
  wavfile  = system.file("testfiles/ax3test.wav", package = "GGIR")[1]
  GAfile  = system.file("testfiles/GENEActiv_testfile.bin", package = "GGIRread")[1]
  gt3xfile  = system.file("testfiles/actigraph_testfile.gt3x", package = "GGIR")[1]
  
  desiredtz = "Europe/London"
  
  cat("\nActigraph .gt3x")
  # actigraph .gt3x
  Igt3x = g.inspectfile(gt3xfile, desiredtz = desiredtz)
  expect_equal(Igt3x$monc, 3)
  expect_equal(Igt3x$dformc, 6)
  expect_equal(Igt3x$sf, 30)
  EHV = g.extractheadervars(Igt3x)
  expect_equal(EHV$deviceSerialNumber, "MOS2E39180594_firmware_1.9.2")
  Mgt3x = g.getmeta(datafile = gt3xfile, desiredtz = desiredtz, windowsize = c(1,300,300))
  expect_true(Mgt3x$filetooshort)
  expect_false(Mgt3x$filecorrupt)
  cat("\nAxivity .cwa")
  # axivity .cwa
  Icwa = g.inspectfile(cwafile, desiredtz = desiredtz)
  expect_equal(Icwa$monc, 4)
  expect_equal(Icwa$dformc, 4)
  expect_equal(Icwa$sf, 100)
  EHV = g.extractheadervars(Icwa)
  expect_equal(EHV$deviceSerialNumber,"39434")
  Mcwa = g.getmeta(cwafile, desiredtz = desiredtz, windowsize = c(1,300,300))
  expect_true(Mcwa$filetooshort)
  expect_false(Mcwa$filecorrupt)
  cat("\nGenea .bin")
  # genea .bin
  Igenea = g.inspectfile(binfile, desiredtz = desiredtz)
  expect_equal(Igenea$monc,1)
  expect_equal(Igenea$dformc,1)
  expect_equal(Igenea$sf,80)
  EHV = g.extractheadervars(Igenea)
  expect_equal(EHV$deviceSerialNumber,"01275")
  Mgenea = g.getmeta(binfile, desiredtz = desiredtz, windowsize = c(1,300,300))
  expect_true(Mgenea$filetooshort)
  expect_false(Mgenea$filecorrupt)
  
  cat("\nAxivity .wav")
  # axivity .wav
  Iwav = expect_warning(g.inspectfile(wavfile, desiredtz = desiredtz))
  expect_equal(Iwav$monc,4)
  expect_equal(Iwav$dformc,3)
  expect_equal(Iwav$sf,100)
  EHV = g.extractheadervars(Iwav)
  expect_equal(EHV$deviceSerialNumber,"37727")
  Mwav = expect_warning(g.getmeta(wavfile, desiredtz = desiredtz, windowsize = c(1,300,300)))
  expect_true(Mwav$filetooshort)
  expect_false(Mwav$filecorrupt)
  
  cat("\nGENEActiv .bin")
  # GENEActiv .bin
  IGA = g.inspectfile(GAfile, desiredtz = desiredtz)
  expect_equal(IGA$monc,2)
  expect_equal(IGA$dformc,1)
  expect_equal(IGA$sf,85.7)

  EHV = g.extractheadervars(IGA)
  expect_equal(EHV$deviceSerialNumber,"012967")
  MGA = g.getmeta(GAfile, desiredtz = desiredtz, windowsize = c(1,300,300), verbose = FALSE)
  expect_true(MGA$filetooshort)
  # expect_true(MGA$filecorrupt)
  
  # test decimal separator recognition extraction
  decn =  g.dotorcomma(cwafile,dformat = 4, mon = 4, desiredtz = desiredtz)
  expect_equal(decn,".")
  decn =  expect_warning(g.dotorcomma(wavfile, dformat = 3, mon = 4, desiredtz = desiredtz))
  expect_equal(decn,".")
  decn =  g.dotorcomma(GAfile, dformat = 1, mon = 2, desiredtz = desiredtz)
  expect_equal(decn,".")
  filequality = list(filecorrupt = FALSE, filetooshort = FALSE)
  dayborder = 0
  cwa_read = g.readaccfile(cwafile, blocksize = 10, blocknumber = 1, filequality = filequality,
                           decn = ".", dayborder,ws = 3, desiredtz = desiredtz, 
                           PreviousEndPage = 1, inspectfileobject = Icwa)
  genea_read = g.readaccfile(binfile, blocksize = 10, blocknumber = 1, filequality = filequality,
                           decn = ".", dayborder = dayborder, ws = 3, desiredtz = desiredtz, PreviousEndPage = 1,
                           inspectfileobject = Igenea)
  wav_read = expect_warning(g.readaccfile(wavfile, blocksize = 2, blocknumber = 1, 
                                          filequality = filequality, 
                                          decn = ".", dayborder = dayborder, ws = 3, desiredtz = desiredtz, 
                                          PreviousEndPage = 1, inspectfileobject = Iwav))
  GA_read = g.readaccfile(GAfile, blocksize = 2, blocknumber = 1, filequality = filequality,
                                         decn = ".", dayborder = dayborder, ws = 3,
                                         desiredtz = desiredtz, PreviousEndPage = 1, inspectfileobject = IGA)
  expect_equal(cwa_read$P$header$blocks, 145)
  expect_equal(round(cwa_read$P$data[200, 6], digits = 4), 0)
  
  expect_equal(nrow(genea_read$P$header), 18)
  expect_equal(sum(genea_read$P$rawxyz[20,]), 1000)
  
  expect_equal(round(sum(wav_read$P$rawxyz), digits = 1), -994.8)
  
  # As of R 4.0, an extra header row is extracted, which affects the positioning of the values.
  # expect_equal(as.numeric(as.character(wav_read$P$header$hvalues[7])),17) 
  expect_equal(round(sum(GA_read$P$data.out[, 2:4]), digits = 2), -467.59)
  # print(GA_read$P$header)
  # expect_equal(as.character(unlist(GA_read$P$header[3, 1])), "216 Hours")
  
  #also test one small other function:
  datadir  = system.file("testfiles", package = "GGIR")[1]
  fnames = datadir2fnames(datadir = datadir, filelist = FALSE)
  expect_equal(length(fnames$fnames), 4)
  expect_equal(length(fnames$fnamesfull), 4)
})