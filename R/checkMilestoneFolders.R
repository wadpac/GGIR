checkMilestoneFolders = function(metadatadir, partNumber) {
  if (partNumber == 1) return()
  # This function checks whether expected output folder(s) exists
  # If not, it creates them
  # Further it check whether folders with expected content are empty.
  # If yesm it gives error.
  paths = c(paste0(metadatadir, "/meta/basic"),
            paste0(metadatadir, "/meta/ms2.out"),
            paste0(metadatadir, "/meta/ms3.out"),
            paste0(metadatadir, "/meta/ms4.out"),
            paste0(metadatadir, "/meta/ms5.out"))
  sleepqc = paste0(metadatadir, "/meta/sleep.qc")
  
  # Make sure folders exists
  for (i in 1:partNumber) {
    if (!dir.exists(paths[i])) {
      dir.create(file.path(paths[i]), recursive = TRUE)
    }
  }
  if (partNumber >= 3) {
    if (!dir.exists(sleepqc)) {
      dir.create(file.path(sleepqc), recursive = TRUE)
    }
  }
  
  # Make sure preceding parts have files
  if (partNumber == 2) {
    expectedParts = 1
  } else if (partNumber == 3) {
    expectedParts = 2
  } else if (partNumber == 4) {
    expectedParts = 3
  } else if (partNumber == 5) {
    expectedParts = c(2, 3, 4)
  }
   
  # Give error when no data was found in the expected parts
  warnAbout = NULL
  for (i in expectedParts) {
    N = length(dir(paths[i]))
    if (N == 0) {
      warnAbout = c(warnAbout, i)
    }
  }
  if (length(warnAbout) > 0) {
    stop(paste0("\nNo milestone data found for part(s) ", paste0(warnAbout, collapse = " and "),
                ". Run this/these first before running part ", partNumber, "."), call. = FALSE)
  }
}