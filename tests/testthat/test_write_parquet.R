library(GGIR)
context("write_dashboard_parquet")

make_fake_metadatadir = function(include_optional = TRUE) {
  td = tempfile("ggir_parquet_test_")
  dir.create(file.path(td, "results", "QC"), recursive = TRUE)

  # --- Part 5 day summary (required) ---
  p5 = data.frame(
    ID            = c("P01", "P01", "P02"),
    filename      = c("P01.csv", "P01.csv", "P02.csv"),
    calendar_date = c("2024-01-01", "2024-01-02", "2024-01-01"),
    ENMO_mean     = c(0.032, 0.041, 0.028),
    stringsAsFactors = FALSE
  )
  write.csv(p5,
            file.path(td, "results", "QC",
                      "part5_daysummary_full_MM_L40M100V400_T5A5.csv"),
            row.names = FALSE)

  if (include_optional) {
    # --- Part 4 night summary (optional) ---
    p4 = data.frame(
      ID              = c("P01", "P02"),
      calendar_date   = c("2024-01-01", "2024-01-01"),
      sleeponset_ts   = c("2024-01-01 23:00:00", "2024-01-01 22:30:00"),
      stringsAsFactors = FALSE
    )
    write.csv(p4,
              file.path(td, "results",
                        "part4_nightsummary_sleep_cleaned.csv"),
              row.names = FALSE)

    # --- Part 2 day summary (optional) ---
    p2day = data.frame(
      ID            = c("P01", "P01", "P02"),
      calendar_date = c("2024-01-01", "2024-01-02", "2024-01-01"),
      L5             = c(0.010, 0.012, 0.009),
      stringsAsFactors = FALSE
    )
    write.csv(p2day,
              file.path(td, "results", "part2_daysummary.csv"),
              row.names = FALSE)

    # --- Part 2 person summary (optional) ---
    p2 = data.frame(
      ID       = c("P01", "P02"),
      filename = c("P01.csv", "P02.csv"),
      n_valid_days = c(2L, 1L),
      stringsAsFactors = FALSE
    )
    write.csv(p2,
              file.path(td, "results", "part2_summary.csv"),
              row.names = FALSE)

    # --- Data quality report (optional, joined by filename) ---
    qc = data.frame(
      filename     = c("P01.csv", "P02.csv"),
      qc_flag      = c(0L, 1L),
      stringsAsFactors = FALSE
    )
    write.csv(qc,
              file.path(td, "results", "QC", "data_quality_report.csv"),
              row.names = FALSE)
  }

  td
}

# Minimal params objects (only fields actually used by the function)
fake_params_output  = list(save_dashboard_parquet = TRUE)
fake_params_general = list(desiredtz = "UTC", acc.metric = "ENMO")
fake_params_phyact  = list(threshold.lig = 40, threshold.mod = 100,
                           threshold.vig = 400,
                           part6_threshold_combi = NULL)

# ---------------------------------------------------------------------------
# Test 1: happy path – Parquet file is created with the right shape
# ---------------------------------------------------------------------------
test_that("write_dashboard_parquet creates a Parquet file with correct rows and columns", {
  skip_on_cran()
  skip_if_not_installed("arrow")

  td = make_fake_metadatadir(include_optional = TRUE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  out = write_dashboard_parquet(
    metadatadir   = td,
    params_output  = fake_params_output,
    params_general = fake_params_general,
    params_phyact  = fake_params_phyact,
    verbose        = FALSE
  )

  parquet_dir = file.path(td, "results", "parquet")

  # File must exist and be returned by the function
  expect_true(file.exists(out))
  expect_equal(
    normalizePath(dirname(out), winslash = "/", mustWork = FALSE),
    normalizePath(parquet_dir, winslash = "/", mustWork = FALSE)
  )
  expect_equal(basename(out), "multiple_participants.parquet")

  # Read back and verify shape
  result = arrow::read_parquet(out)

  # Should have one row per Part 5 day-summary row (3 rows in our mock)
  expect_equal(nrow(result), 3)

  # Key columns from Part 5 must be present (after clean_colnames lowercasing)
  expect_true("id"            %in% names(result))
  expect_true("calendar_date" %in% names(result))
  expect_true("enmo_mean"     %in% names(result))

  # Optional Part 4 column should have been joined in
  expect_true("sleeponset_ts" %in% names(result))

  # Optional Part 2 day-summary column should have been joined in
  expect_true("l5" %in% names(result))
})

# ---------------------------------------------------------------------------
# Test 2: missing optional CSVs – function still succeeds (graceful degradation)
# ---------------------------------------------------------------------------
test_that("write_dashboard_parquet succeeds when optional CSVs are absent", {
  skip_on_cran()
  skip_if_not_installed("arrow")

  td = make_fake_metadatadir(include_optional = FALSE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  # Should produce no warnings about missing files crashing the export
  out = write_dashboard_parquet(
    metadatadir   = td,
    params_output  = fake_params_output,
    params_general = fake_params_general,
    params_phyact  = fake_params_phyact,
    verbose        = FALSE
  )

  parquet_dir = file.path(td, "results", "parquet")
  expect_true(file.exists(out))
  expect_equal(
    normalizePath(dirname(out), winslash = "/", mustWork = FALSE),
    normalizePath(parquet_dir, winslash = "/", mustWork = FALSE)
  )
  expect_equal(basename(out), "multiple_participants.parquet")

  result = arrow::read_parquet(out)
  expect_equal(nrow(result), 3)  # still 3 rows from Part 5
})

# ---------------------------------------------------------------------------
# Test 3: no results directory – function warns and returns NULL
# ---------------------------------------------------------------------------
test_that("write_dashboard_parquet returns NULL with a warning when results dir is missing", {
  skip_on_cran()
  skip_if_not_installed("arrow")

  td = tempfile("ggir_empty_")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  expect_warning(
    out <- write_dashboard_parquet(
      metadatadir   = td,
      params_output  = fake_params_output,
      params_general = fake_params_general,
      params_phyact  = fake_params_phyact,
      verbose        = FALSE
    ),
    regexp = "No results directory"
  )
  expect_null(out)
})

# ---------------------------------------------------------------------------
# Test 4: no Part 5 CSVs present – function warns and returns NULL
# ---------------------------------------------------------------------------
test_that("write_dashboard_parquet returns NULL with a warning when Part 5 CSVs are missing", {
  skip_on_cran()
  skip_if_not_installed("arrow")

  td = tempfile("ggir_nop5_")
  dir.create(file.path(td, "results", "QC"), recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  expect_warning(
    out <- write_dashboard_parquet(
      metadatadir   = td,
      params_output  = fake_params_output,
      params_general = fake_params_general,
      params_phyact  = fake_params_phyact,
      verbose        = FALSE
    ),
    regexp = "No Part 5 day summary"
  )
  expect_null(out)
})
