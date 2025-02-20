library(GGIR)
context("HASPT")
test_that("HASPT generate correct output", {
  sibs = c(rep(0, 180),
           rep(1, 60), rep(0, 15), rep(1, 120), rep(0, 30), rep(1, 180),
           rep(0, 240))
  marker = rep(0, length(sibs))
  marker[c(160, 250, length(sibs) - 260, length(sibs) - 20)] = 1
         
  # Marker data available and button pressed
  vanHees2025 = HASPT(angle = NULL, invalid = NULL,
                      ws3 = 60,
                      HASPT.algo = "vanHees2025",
                      marker = marker,
                      sibs = sibs)

  expect_equal(length(vanHees2025), 4)
  expect_equal(vanHees2025$SPTE_start, 160)
  expect_equal(vanHees2025$SPTE_end, 565)
  expect_equal(vanHees2025$part3_guider, "vanHees2025")

  # No marker data
  vanHees2025 = HASPT(angle = NULL, invalid = NULL,
                      ws3 = 60,
                      HASPT.algo = "vanHees2025",
                      marker = NULL,
                      sibs = sibs)

  expect_equal(length(vanHees2025), 4)
  expect_equal(vanHees2025$SPTE_start, 180)
  expect_equal(vanHees2025$SPTE_end, 585)
  expect_equal(vanHees2025$part3_guider, "vanHees2025")

  # Marker data but marker button not pressed
  vanHees2025 = HASPT(angle = NULL, invalid = NULL,
                      ws3 = 60,
                      HASPT.algo = "vanHees2025",
                      marker = rep(0, length(sibs)),
                      sibs = sibs)

  expect_equal(length(vanHees2025), 4)
  expect_equal(vanHees2025$SPTE_start, 180)
  expect_equal(vanHees2025$SPTE_end, 585)
  expect_equal(vanHees2025$part3_guider, "vanHees2025")
})