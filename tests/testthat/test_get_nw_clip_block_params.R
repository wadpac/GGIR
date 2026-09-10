library(GGIR)
context("non-wear and clipping block parameters")

test_that("rmc.noise is converted from mg to g for ad-hoc csv", {
  skip_on_cran()

  params_rawdata = load_params()$params_rawdata   # rmc.noise = 13, documented in mg

  out = get_nw_clip_block_params(monc = MONITOR$AD_HOC,
                                 dformat = FORMAT$AD_HOC_CSV,
                                 sf = 80,
                                 params_rawdata = params_rawdata)

  # 13 mg * 1.2 = 15.6 mg = 0.0156 g, the same order as the 0.013 g used for
  # every other monitor. Before the fix this was 15.6 g and the SD criterion in
  # detect_nonwear_clipping() and g.calibrate() never bound.
  expect_equal(out$sdcriter, 0.0156)
  expect_true(out$sdcriter < 0.1)

  # the sibling threshold is documented in mg too and was already converted
  expect_equal(out$racriter, 0.15)
})

test_that("other monitors keep the hard-coded sdcriter", {
  skip_on_cran()

  params_rawdata = load_params()$params_rawdata
  out = get_nw_clip_block_params(monc = MONITOR$GENEACTIV,
                                 dformat = FORMAT$BIN,
                                 sf = 80,
                                 params_rawdata = params_rawdata)
  expect_equal(out$sdcriter, 0.013)
})
