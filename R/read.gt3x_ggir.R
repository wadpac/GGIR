## "Copyright (2021): Tuomo Nieminen, and John Muschelli; license: EUPL, see LICENSE.note file for details"

## Note on edits made to this version by Vincent van Hees:
##
## The code in this function is a modified version of the read.gt3x
## in that it aids batch-loading of modern gt3x files
## A pull request has been made to feed these enhancements back into the original code base
## https://github.com/THLfi/read.gt3x/pull/40
## If and when merged we intend to deprecate the GGIR version of the code and make a direct dependency.

read.gt3x_ggir <- function(path, verbose = FALSE, asDataFrame = FALSE,
                           imputeZeroes = FALSE,
                           flag_idle_sleep = FALSE,
                           cleanup = FALSE,
                           ...,
                           add_light = FALSE) {
  
  #=======================================================
  # Declare helper functions from ead.gt3x, but which it does not expose to the user:
  is_gt3x <- function(path) {
    if (length(path) == 0) return(FALSE)
    sapply(path, function(f) grepl("\\.gt3x$", f))
  }
  unzip_zipped_gt3x <- function(path, cleanup = TRUE) {
    if (length(path) == 0) return(path)
    stopifnot(length(path) == 1)
    exts <- sapply(path, tools::file_ext)
    reg_exts <- exts
    exts <- tolower(exts)
    unzip_these <- exts %in% c("gz", "bz", "bz2", "xz")
    # don't decompress if the file doesn't exist
    fe <- file.exists(path)
    fe_before <- file.exists(sub(paste0("[.]", reg_exts, "$"), "", path))
    if (any(unzip_these & fe)) {
      zipped_files <- path[unzip_these & fe]
      zip_exts <- exts[unzip_these & fe]
      zip_outfiles <- mapply(function(x, y) {
        FUN <- switch(y,
                      bz = bzfile,
                      bz2 = bzfile,
                      gz = gzfile,
                      xz = xzfile)
        R.utils::decompressFile(
          x,
          ext = y,
          FUN = FUN,
          remove = FALSE,
          overwrite = TRUE,
          temporary = TRUE)
      }, zipped_files, zip_exts)
      path[unzip_these & fe] <- zip_outfiles
    }
    
    attr(path, "remove") <- unzip_these & cleanup & !fe_before
    path
  }
  list_gt3x <- function(path) {
    files <- list.files(path = path, full.names = TRUE)
    gt3xfiles <- files[is_gt3x(files)]
    gt3xfiles
  }
  have_log_and_info <- function(path, verbose = TRUE) {
    haslog <- have_log(path, verbose)
    hasinfo <- have_info(path, verbose)
    if (!haslog & verbose) {
      message(path, " doesn't contain log.bin")
    }
    if (!hasinfo & verbose) {
      message(path, " doesn't contain info.txt")
    }
    return(haslog & hasinfo)
  }
  have_info <- function(path, verbose = TRUE) {
    if (is_gt3x(path)) {
      filenames <- unzip(path, list = TRUE)$Name
    } else {
      filenames <- list.files(path)
    }
    "info.txt" %in% filenames
  }
  have_log <- function(path, verbose = TRUE) {
    if (is_gt3x(path)) {
      filenames <- unzip(path, list = TRUE)$Name
    } else {
      filenames <- list.files(path)
    }
    "log.bin" %in% filenames
  }
  old_version <- function(info) {
    firmware_version <- info$Firmware
    firmware_version <- package_version(firmware_version)
    hdr <- info$`Serial Prefix`
    ret <- hdr %in% c("MRA", "NEO") &
      firmware_version <= package_version("2.5.0")
    return(ret)
  }
  get_features = function(features) {
    if (is.null(features)) {
      return(NULL)
    }
    feat <- c("heart rate monitor", "data summary", "sleep mode", "proximity tagging",
              "epoch data", "no raw data")
    features <- as.integer(intToBits(features))[1:5] > 0
    if (!any(features)) {
      features <- "none"
    } else {
      features <- paste(feat[features], collapse = ",")
    }
    features
  }
  #=========================================
  
  verbose_message <- function(..., verbose = verbose) {
    if (verbose) {
      message(...)
    }
  }
  fun_start_time <- Sys.time()
  
  path <- unzip_zipped_gt3x(path, cleanup = cleanup)
  remove_path <- path
  remove_file <- attr(path, "remove")
  if (is.null(remove_file)) {
    remove_file <- FALSE
  }
  
  has_info <- have_info(path)
  if (has_info) {
    info <- read.gt3x::parse_gt3x_info(path)
  }
  files <- c("info.txt", "log.bin")
  if (has_info) {
    is_old_version <- old_version(info)
    if (is_old_version) {
      files <- c("info.txt",
                 "activity.bin",
                 "lux.bin")
    }
  }
  
  if (is_gt3x(path)) {
    verbose_message(
      paste0("Input is a .gt3x file, unzipping to a ",
             "temporary location first..."),
      verbose = verbose)
    path <- read.gt3x::unzip.gt3x(path,
                                  verbose = verbose,
                                  files = files,
                                  check_structure = !is_old_version,
                                  location = tempdir())
    on.exit(unlink(path, recursive = TRUE))
  }
  
  tz  <- "GMT" # used for parsing, times are actually in local timezone
  info <- read.gt3x::parse_gt3x_info(path, tz = tz)
  
  if (verbose) {
    print(info)
  }
  
  samples <- read.gt3x::get_n_samples(info)
  bad_samples <- attr(samples, "bad")
  
  verbose_message(
    "Parsing GT3X data via CPP.. expected sample size: ", samples,
    verbose = verbose
  )
  xyz = c("X", "Y", "Z")
  if (!is_old_version) {
    logpath <- file.path(path, "log.bin")
    stopifnot(length(info$`Acceleration Scale`) > 0)
    stopifnot(length(info$`Sample Rate`) > 0)
    stopifnot(length(info$`Start Date`) > 0)
    accdata <- parseGT3Xggir(
      logpath, max_samples = samples,
      scale_factor = info[["Acceleration Scale"]],
      sample_rate = info[["Sample Rate"]],
      start_time = as.numeric(info[["Start Date"]]),
      verbose = as.logical(verbose),
      impute_zeroes = imputeZeroes, ...)
    # need reordering for Y X Z ACTIVITY PACKETS
    tmp_at <- attributes(accdata)
    accdata <- accdata[, xyz]
    tmp_at$dim <- dim(accdata)
    tmp_at$dimnames[[2]] <- xyz
    tmp_at$features <- get_features(tmp_at$features)
    attributes(accdata) <- tmp_at
    rm(tmp_at)
    
    if (verbose > 1) {
      message("Activity data now in R")
    }
  } else {
    warning(paste0("\nSorry, older gt3x data formats are currently not facilitated by GGIR.",
                    " For now, first convert your data to .csv format with Actilife or R package read.gt3x."))
  }
  gc()
  attr(accdata, "add_light") = add_light
  rdata = range(c(accdata[, xyz]))
  #!!! Need ISSUE/check
  if (any(abs(rdata) > 20)) {
    warning("Data seems too large (> 20) - checksum may be wrong - check data")
  }
  #!!! Need ISSUE/check
  check = anyDuplicated(attr(accdata, "time_index")) > 0
  if (check) {
    warning("Duplicated time indices - usually indicative of a problem!")
  }
  
  if (cleanup) {
    if (remove_file) {
      file.remove(remove_path)
    }
    rm_files <- file.path(path,
                          c("log.bin", "info.txt", "activity.bin",
                            "lux.bin"))
    suppressWarnings({
      file.remove(rm_files)
    })
  }
  
  verbose_message("Adding attributes", verbose = verbose > 1)
  
  attr(accdata, "start_time") <- info[["Start Date"]]
  attr(accdata, "stop_time") <- info[["Stop Date"]]
  attr(accdata, "last_sample_time") <- info[["Last Sample Time"]]
  attr(accdata, "subject_name") <- info[["Subject Name"]]
  attr(accdata, "time_zone") <- info[["TimeZone"]]
  attr(accdata, "firmware") <- info[["Firmware"]]
  attr(accdata, "serial_prefix") <- info[["Serial Prefix"]]
  attr(accdata, "acceleration_min") <- info[["Acceleration Min"]]
  attr(accdata, "acceleration_max") <- info[["Acceleration Max"]]
  
  attr(accdata, "bad_samples") <- bad_samples
  attr(accdata, "old_version") <- is_old_version
  
  attr(accdata, "header") <- info
  if (!is_old_version) {
    attr(accdata, "missingness") <- data.frame(
      time = as.POSIXct(as.numeric(names(attr(accdata, "missingness"))),
                        origin = "1970-01-01", tz = tz),
      n_missing = attr(accdata, "missingness"),
      stringsAsFactors = FALSE)
  }
  
  verbose_message(
    "Done", " (in ",
    as.numeric(
      difftime(Sys.time(),
               fun_start_time, units = "secs")),
    " seconds)",
    verbose = verbose)
  
  
  accdata <- structure(accdata,
                       class = c("activity", class(accdata)))
  
  if (asDataFrame) {
    accdata <- as.data.frame(accdata, verbose = verbose > 1)
  }
  if (flag_idle_sleep) {
    if (asDataFrame) {
      accdata$idle = rowSums(accdata[, c("X", "Y", "Z")] == 0) == 3
    } else {
      accdata = cbind(accdata,
                      idle = rowSums(accdata[, c("X", "Y", "Z")] == 0) == 3)
    }
  }
  return(accdata)
}
