# Run GGIR from scratch with Parquet export enabled
# Run this script from the GGIR package root (e.g. Documents/GGIR).
# It loads GGIR from source and runs all parts (1-5) with Parquet export.

# Load GGIR from current source (development mode)
if (file.exists("DESCRIPTION") && read.dcf("DESCRIPTION")[1, "Package"] == "GGIR") {
  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".", quiet = TRUE)
  } else {
    library(GGIR)
  }
} else {
  library(GGIR)
}

# ============================================================================
# CONFIGURATION - Update these paths as needed
# ============================================================================

# Option A: Use built-in test data (creates 123A_testaccfile.csv in current dir)
# Option B: Set datadir to your folder of raw files (must be different from outputdir)
use_test_data <- TRUE  # Set FALSE and set datadir below to use your own data

if (use_test_data) {
  # Create 2-day test CSV in current directory (GGIR root)
  create_test_acc_csv(Nmin = 2 * 1440)  # 2 days of 1-min epochs
  datadir <- "123A_testaccfile.csv"  # file list
} else {
  datadir <- "testdata"  # UPDATE: folder with your raw .csv/.bin/.gt3x/.cwa files
}

# Path where GGIR will write output (cannot equal or be inside datadir)
outputdir <- "output_run"

# Study name (used to name the output folder)
studyname <- "Samuel_test"

# ============================================================================
# OPTIONAL: Clean existing output before running
# ============================================================================
# Uncomment the next 3 lines to delete existing output before rerunning:
# output_path <- file.path(outputdir, paste0("output_", studyname))
# if (dir.exists(output_path)) {
#   unlink(output_path, recursive = TRUE)
#   cat("Cleaned existing output directory\n")
# }

# ============================================================================
# Run GGIR
# ============================================================================
cat("Starting GGIR processing...\n")
cat(paste("Input directory:", datadir, "\n"))
cat(paste("Output directory:", outputdir, "\n"))
cat(paste("Study name:", studyname, "\n\n"))

GGIR(
  mode = 1:5,  # Run all parts (1=raw data loading, 2=calibration, 3=sleep, 4=sleep analysis, 5=activity)
  datadir = datadir,
  outputdir = outputdir,
  studyname = studyname,
  f0 = 1,  # Start from first file
  f1 = 0,  # Process all files (0 = all)
  do.report = c(2, 4, 5),  # Generate reports for parts 2, 4, 5
  verbose = TRUE,
  # Enable Parquet export (this will generate ggir_results.parquet)
  save_dashboard_parquet = TRUE
)

cat("\n=== GGIR run completed ===\n")
output_path <- file.path(outputdir, paste0("output_", studyname))
parquet_file <- file.path(output_path, "results", "ggir_results.parquet")
if (file.exists(parquet_file)) {
  cat(paste("Parquet file created:", parquet_file, "\n"))
  cat(paste("File size:", round(file.info(parquet_file)$size / 1024, 1), "KB\n"))
} else {
  cat("Warning: Parquet file not found. Check if save_dashboard_parquet worked.\n")
}

# Remove test CSV if we created it
# if (use_test_data && file.exists("123A_testaccfile.csv")) {
#   file.remove("123A_testaccfile.csv")
#   cat("Removed temporary test file 123A_testaccfile.csv\n")
# }
