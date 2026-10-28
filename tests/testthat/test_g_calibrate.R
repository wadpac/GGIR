library(GGIR)

# Exercise calibration independently of file decoding, with one complete hour.
calibrate_test_samples = function(samples, sf, minloadcrit = 0, read_log = NULL,
                                 monitor = MONITOR$AXIVITY, format = FORMAT$CWA) {
  blocks = if (is.data.frame(samples) || is.matrix(samples)) list(samples) else samples
  reader = function(blocknumber, ...) {
    if (!is.null(read_log)) read_log$blocks = c(read_log$blocks, blocknumber)
    list(P = list(data = blocks[[blocknumber]]), header = NULL, endpage = blocknumber,
         isLastBlock = blocknumber == length(blocks))
  }
  calibrate = g.calibrate
  environment(calibrate) = list2env(list(g.readaccfile = reader),
                                    parent = environment(calibrate))
  old_warn = getOption("warn")
  on.exit(options(warn = old_warn))
  calibrate("in-memory samples", verbose = FALSE, minloadcrit = minloadcrit,
            inspectfileobject = list(monc = monitor, dformc = format, sf = sf))
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
      # Preserve the historical cumulative sums, including propagation of NA
      # to later epochs and exact comparisons used in feature selection.
      original_mean = function(x) {
        sums = cumsum(c(0, x))
        diff(sums[seq(1, length(sums), by = epoch_samples)]) / epoch_samples
      }
      expected_means = vapply(samples[c("x", "y", "z")], original_mean, numeric(epochs))
      expect_identical(unname(as.matrix(result$spheredata[, c("meanx", "meany", "meanz")])),
                       unname(expected_means[keep, , drop = FALSE]))
      expected_en = original_mean(sqrt(samples$x^2 + samples$y^2 + samples$z^2))
      expect_identical(result$spheredata[["Euclidean Norm"]], expected_en[keep])
      if (use_temperature) {
        expect_identical(result$spheredata$temperature,
                         original_mean(samples$temperature)[keep])
      }
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

calibration_block_samples = function(sf = 3, hours = 3) {
  set.seed(72)
  epochs = hours * 360
  axes = matrix(rnorm(epochs * 3), ncol = 3)
  axes = axes / sqrt(rowSums(axes^2))
  samples = as.data.frame(axes[rep(seq_len(epochs), each = sf * 10), ] * 1.02 +
                           matrix(rnorm(hours * 3600 * sf * 3, sd = 0.002), ncol = 3))
  names(samples) = c("x", "y", "z")
  samples$temperature = rep(25 + sin(seq_len(epochs) / 13), each = sf * 10)
  samples$time = seq_len(nrow(samples)) / sf
  samples
}

test_that("calibration carries partial hours across matrix and data-frame blocks", {
  skip_on_cran()
  sf = 3
  samples = calibration_block_samples(sf)
  hour = 3600 * sf
  blocks = list(samples[1:(hour + 5), ],
                as.matrix(samples[(hour + 6):(2 * hour + 3), ]),
                samples[(2 * hour + 4):nrow(samples), ])
  reads = new.env()
  result = calibrate_test_samples(blocks, sf, minloadcrit = 48, read_log = reads)
  complete = calibrate_test_samples(samples, sf, minloadcrit = 48)
  expect_equal(reads$blocks, 1:3)
  expect_equal(result$npoints, 3 * 360 - 1)
  expect_identical(result$nhoursused, complete$nhoursused)
  sd_columns = c("sdx", "sdy", "sdz")
  expect_identical(unname(as.matrix(result$spheredata[, sd_columns])),
                   unname(as.matrix(complete$spheredata[, sd_columns])))
  expect_equal(result$scale, complete$scale, tolerance = 1e-10)
  expect_equal(result$offset, complete$offset, tolerance = 1e-10)
  expect_equal(result$tempoffset, complete$tempoffset, tolerance = 1e-10)
  expect_identical(result$QCmessage, complete$QCmessage)
})

test_that("calibration expands the feature buffer beyond one day", {
  skip_on_cran()
  samples = calibration_block_samples(hours = 25)
  result = calibrate_test_samples(samples, sf = 3)
  expect_equal(result$npoints, 25 * 360 - 1)
  expect_equal(result$nhoursused, 25)
  expect_equal(result$scale, rep(1 / 1.02, 3), tolerance = 0.0001)
  expect_lt(result$cal.error.end, 0.0005)
})

test_that("calibration stops loading after an acceptable fit", {
  skip_on_cran()
  samples = calibration_block_samples(hours = 2)
  hour = 3600 * 3
  reads = new.env()
  result = calibrate_test_samples(list(samples[1:hour, ], samples[(hour + 1):nrow(samples), ]),
                                  sf = 3, read_log = reads)
  expect_equal(reads$blocks, 1)
  expect_match(result$QCmessage, "recalibration done, no problems detected")
  expect_lt(result$cal.error.end, result$cal.error.start)
})

test_that("calibration handles ignored temperature and insufficient stationary data", {
  skip_on_cran()
  samples = calibration_block_samples(hours = 1)
  reference = calibrate_test_samples(samples[, c("x", "y", "z", "time")], sf = 3)
  for (temperature in c(130, 25)) {
    samples$temperature = temperature
    expect_warning(result <- calibrate_test_samples(samples, sf = 3),
                   "temperature ignored")
    expect_false(result$use.temp)
    expect_identical(result$scale, reference$scale)
    expect_identical(result$offset, reference$offset)
  }
  # Exercise zero removal using both the CWA and ActiGraph CSV paths.
  samples = calibration_block_samples(hours = 2)
  samples[100:110, c("x", "y", "z")] = 0
  removed = samples[-(100:110), ]
  reference = calibrate_test_samples(removed, sf = 3)
  cwa = calibrate_test_samples(samples, sf = 3)
  csv = calibrate_test_samples(samples, sf = 3, monitor = MONITOR$ACTIGRAPH,
                               format = FORMAT$CSV)
  expect_identical(cwa$spheredata, reference$spheredata)
  expect_identical(csv$spheredata, reference$spheredata)
  expect_identical(cwa$scale, reference$scale)
  expect_identical(csv$scale, reference$scale)

  set.seed(91)
  moving = as.data.frame(matrix(rnorm(3600 * 3 * 3, sd = 0.1), ncol = 3))
  names(moving) = c("x", "y", "z")
  result = calibrate_test_samples(moving, sf = 3)
  expect_match(result$QCmessage, "no non-movement data available")
  expect_length(result$spheredata, 0)
})
