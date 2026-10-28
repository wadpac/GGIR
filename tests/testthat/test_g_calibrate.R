library(GGIR)

# Exercise calibration independently of file decoding, with one complete hour.
calibrate_test_samples = function(samples, sf) {
  reader = function(...) {
    list(P = list(data = samples), header = NULL, endpage = 1,
         isLastBlock = TRUE)
  }
  calibrate = g.calibrate
  environment(calibrate) = list2env(list(g.readaccfile = reader),
                                    parent = environment(calibrate))
  old_warn = getOption("warn")
  on.exit(options(warn = old_warn))
  calibrate("in-memory samples", verbose = FALSE, minloadcrit = 0,
            inspectfileobject = list(monc = MONITOR$AXIVITY,
                                     dformc = FORMAT$CWA, sf = sf))
}

test_that("calibration retains epoch SDs, threshold selection and fitted coefficients", {
  skip_on_cran()
  set.seed(42)
  epochs = 360
  orientations = matrix(rnorm(epochs * 3), ncol = 3)
  orientations = orientations / sqrt(rowSums(orientations^2))
  offsets = c(0.02, -0.015, 0.01)
  scales = c(0.997, 1.008, 0.991)
  temperature = 25 + sin(seq_len(epochs) / 13)

  for (sf in c(3, 30, 75, 100)) {
    epoch_samples = 10 * sf
    shape = sin(seq_len(epoch_samples) * 2 * pi / epoch_samples)
    shape = (shape - mean(shape)) / sd(shape)
    # Include constant epochs and epochs on both sides of the 0.013g cutoff.
    target_sd = rep(c(0, 0.001, 0.01299, 0.01301), length.out = epochs)
    noise = rep(target_sd, each = epoch_samples) * rep(shape, epochs)

    for (use_temperature in c(FALSE, TRUE)) {
      temp_offsets = if (use_temperature) c(0.001, -0.002, 0.0015) else rep(0, 3)
      means = orientations - (temperature - mean(temperature)) %o% temp_offsets
      means = sweep(sweep(means, 2, scales, "/"), 2, offsets, "-")
      samples = as.data.frame(means[rep(seq_len(epochs), each = epoch_samples), ] + noise)
      names(samples) = c("x", "y", "z")
      if (use_temperature) samples$temperature = rep(temperature, each = epoch_samples)
      # A missing final sample must exclude that epoch from the sphere.
      samples$x[nrow(samples)] = NA_real_

      expected_sd = vapply(c("x", "y", "z"), function(axis) {
        vapply(seq_len(epochs), function(epoch) {
          rows = seq.int((epoch - 1) * epoch_samples + 1, epoch * epoch_samples)
          sd(samples[[axis]][rows])
        }, numeric(1))
      }, numeric(epochs))
      keep = which(rowSums(expected_sd < 0.013) == 3)
      keep = setdiff(keep, 1) # Calibration discards the first epoch.

      result = calibrate_test_samples(samples, sf)
      expect_identical(result$npoints, length(keep))
      expect_identical(unname(as.matrix(result$spheredata[, c("sdx", "sdy", "sdz")])),
                       unname(expected_sd[keep, , drop = FALSE]))
      expect_equal(result$scale, scales, tolerance = 1e-4)
      expected_offsets = offsets + (mean(temperature[keep]) - mean(temperature)) *
        temp_offsets / scales
      # The iterative fit stops at its existing convergence tolerance.
      expect_lt(max(abs(result$offset - expected_offsets)), 0.0005)
      expect_lt(max(abs(result$tempoffset - temp_offsets)), 0.0005)
      expect_lt(result$cal.error.end, 0.0005)
    }
  }
})
