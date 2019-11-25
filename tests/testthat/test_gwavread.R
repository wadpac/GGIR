library(GGIR)
context("g.wavread")
test_that("g.wavread reads data in file correctly", {
  skip_on_cran()
  wavfile  = system.file("testfiles/ax3test.wav", package = "GGIR")[1]
  WAV = expect_warning(g.wavread(wavfile = wavfile,units="samples", start = 1, end = 100))

  expect_equal(nrow(WAV$rawxyz),100)
  expect_equal(round(sum(WAV$rawxyz),digits=2),-97.08)
  expect_equal(as.character(WAV$header$hnames[10]),"Channel-1")
  expect_equal(as.character(WAV$header$hvalues[11]),"8")
})