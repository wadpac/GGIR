library(GGIR)

# Use explicit window medians as a reference, independently of runmed().
reference_rollmedian_metrics = function(data, sf, ws3 = 5) {
  step = max(floor(sf / 10), 1)
  k = round(if (step > 1) 10 * 5 else sf * 5)
  if (k %% 2 == 0) k = k + 1
  smoothed = apply(data, 2, function(x) {
    down = x[seq(1, length(x), by = step)]
    half = k %/% 2
    centres = seq.int(half + 1, length(down) - half)
    medians = vapply(centres, function(i) median(down[(i - half):(i + half)]), numeric(1))
    padded = c(rep(medians[1], half), medians, rep(tail(medians, 1), half))
    rep(padded, each = step)[seq_along(x)]
  })
  average = function(x) {
    n = floor(length(x) / (sf * ws3)) * sf * ws3
    as.numeric(colMeans(matrix(x[seq_len(n)], nrow = sf * ws3)))
  }
  result = list()
  for (i in seq_len(3)) {
    axis = c('x', 'y', 'z')[i]
    result[[paste0('roll_med_acc_', axis)]] = average(smoothed[, i])
    result[[paste0('dev_roll_med_acc_', axis)]] = average(abs(data[, i] - smoothed[, i]))
    others = setdiff(seq_len(3), i)
    result[[paste0('angle_', axis)]] = average(atan(smoothed[, i] /
      sqrt(smoothed[, others[1]]^2 + smoothed[, others[2]]^2)) / (pi / 180))
  }
  result
}

test_that('rolling medians are retained for partial downsampling steps', {
  skip_on_cran()
  metrics = extract_params()$params_metrics
  metrics[] = lapply(metrics, function(x) FALSE)
  enabled = c('anglex', 'angley', 'anglez', 'roll_med_acc_x', 'roll_med_acc_y',
              'roll_med_acc_z', 'dev_roll_med_acc_x', 'dev_roll_med_acc_y', 'dev_roll_med_acc_z')
  metrics[paste0('do.', enabled)] = TRUE
  set.seed(1)
  for (sf in c(10, 30, 75, 100)) {
    for (extra in c(0, 1)) {
      n = if (sf == 75) 900 * sf + extra else 60 * sf + extra
      data = cbind(x = rnorm(n, 0, 0.3), y = rnorm(n, 0.2, 0.3), z = rnorm(n, 0.9, 0.3))
      actual = g.applymetrics(data, sf = sf, ws3 = 5, metrics2do = metrics)
      expected = reference_rollmedian_metrics(data, sf)
      for (name in names(expected)) {
        expect_equal(actual[[name]], expected[[name]], info = paste(sf, extra, name))
      }
      angles_only = metrics
      angles_only[paste0('do.', enabled[-seq_len(3)])] = FALSE
      angles = g.applymetrics(data, sf = sf, ws3 = 5, metrics2do = angles_only)
      expect_equal(angles, expected[c('angle_x', 'angle_y', 'angle_z')])
    }
  }
})

test_that('rolling median padding uses boundary medians and preserves real zeros', {
  metrics = extract_params()$params_metrics
  metrics[] = lapply(metrics, function(x) FALSE)
  metrics$do.roll_med_acc_x = TRUE
  sf = 30
  # A varying tail distinguishes the final median from an earlier tail value.
  # Zero plateaus distinguish real zero medians from edge padding.
  x = c(rep(0, 300), seq(-1, 1, length.out = 1200), rep(0, 300))
  data = cbind(x = x, y = rev(x), z = seq_along(x) / length(x))
  actual = g.applymetrics(data, sf = sf, ws3 = 5, metrics2do = metrics)
  expected = reference_rollmedian_metrics(data, sf)
  for (axis in c('x', 'y', 'z')) {
    name = paste0('roll_med_acc_', axis)
    expect_equal(actual[[name]], expected[[name]])
  }
  expect_equal(actual$roll_med_acc_x[c(1, 12)], c(0, 0))
})

test_that('rolling medians handle zero axes and retain the zoo fallback for missing data', {
  metrics = extract_params()$params_metrics
  metrics[] = lapply(metrics, function(x) FALSE)
  metrics$do.roll_med_acc_x = TRUE
  sf = 10
  x = seq_len(600) / 600
  x[300] = NA_real_
  data = cbind(x = x, y = rep(0, 600), z = rep(1, 600))
  actual = g.applymetrics(data, sf = sf, ws3 = 5, metrics2do = metrics)
  expected = zoo::rollmedian(x, k = 51, fill = list('extend', 0, 'extend'))
  expected[is.na(expected)] = tail(expected[!is.na(expected)], 1)
  expect_equal(actual$roll_med_acc_x, as.numeric(colMeans(matrix(expected, nrow = 50))))
  expect_equal(actual$roll_med_acc_y, rep(0, 12))
  expect_equal(actual$roll_med_acc_z, rep(1, 12))
})
