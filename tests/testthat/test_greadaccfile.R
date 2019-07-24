library(GGIR)
context("g.readaccfile")
test_that("g.readaccfile and g.inspectfile can read genea and cwa file correctly", {
  
  cwafile  = system.file("binfile/ax3_testfile.cwa", package = "GGIR")[1]
  binfile  = system.file("binfile/genea_testfile.bin", package = "GGIR")[1]
  wavfile  = system.file("binfile/ax3test.wav", package = "GGIR")[1]
  desiredtz = "Europe/London"
  Icwa = g.inspectfile(cwafile, desiredtz = desiredtz)
  expect_equal(Icwa$monc,4)
  expect_equal(Icwa$dformc,4)
  expect_equal(Icwa$sf,100)
  
  Igenea = g.inspectfile(binfile, desiredtz = desiredtz)
  expect_equal(Igenea$monc,1)
  expect_equal(Igenea$dformc,1)
  expect_equal(Igenea$sf,80)
  
  Iwav = expect_warning(g.inspectfile(wavfile, desiredtz = desiredtz))
  expect_equal(Iwav$monc,4)
  expect_equal(Iwav$dformc,3)
  expect_equal(Iwav$sf,100)

  filequality = list(filecorrupt = FALSE, filetooshort=FALSE)
  cwa_read = g.readaccfile(cwafile,blocksize=10,blocknumber=1,filequality=filequality,
                           decn=".",dayborder,ws=3, desiredtz = desiredtz, PreviousEndPage = 1,inspectfileobject=Icwa)
  genea_read = g.readaccfile(binfile,blocksize=10,blocknumber=1,filequality=filequality,
                           decn=".",dayborder,ws=3, desiredtz = desiredtz, PreviousEndPage = 1,inspectfileobject=Igenea)
  wav_read = expect_warning(g.readaccfile(wavfile,blocksize=2,blocknumber=1,filequality=filequality,
                             decn=".",dayborder,ws=3, desiredtz = desiredtz, PreviousEndPage = 1,inspectfileobject=Iwav))
  expect_equal(cwa_read$P$header$blocks,145)
  expect_equal(round(cwa_read$P$data[200,6], digits=4),4.1133)
  
  expect_equal(nrow(genea_read$P$header),18)
  expect_equal(sum(genea_read$P$rawxyz[20,]),1000)
  
  expect_equal(round(sum(wav_read$P$rawxyz),digits=1),-994.8)
  expect_equal(as.numeric(wav_read$P$header$hvalues[7]),3)
  
})