datadir2fnames = function(datadir,filelist) {
  if (filelist == FALSE) {
    fnamesfull = c(dir(datadir, recursive = TRUE, ignore.case = TRUE, pattern = "[.]csv", full.names = TRUE),
                   dir(datadir, recursive = TRUE, ignore.case = TRUE, pattern = "[.]bin", full.names = TRUE),
                   dir(datadir, recursive = TRUE, ignore.case = TRUE, pattern = "[.]wav", full.names = TRUE),
                   dir(datadir, recursive = TRUE, ignore.case = TRUE, pattern = "[.]cwa", full.names = TRUE),
                   dir(datadir, recursive = TRUE, ignore.case = TRUE, pattern = "[.]gt3", full.names = TRUE))
    fnames = basename(fnamesfull)
    fnamesRD = dir(datadir,recursive = TRUE, pattern = "[.]RD", full.names = TRUE)
    if (length(fnames) == length(fnamesRD)) { #because filenames may have both .bin in the middle and .RData
      fnames = basename(fnamesRD)
      fnamesfull = fnamesRD
    }
  } else {
    if (ismovisens(datadir)) {
      fnamesfull = dir(datadir, recursive = TRUE, pattern = "acc.bin", full.names = TRUE)
      fnames = basename(dirname(fnamesfull))
      nfolders = length(dir(datadir))
      if (nfolders > length(fnamesfull)) { # meaning there are movisens data folder without acc.bin
        # folders without acc.bin
        allfolders = dir(datadir, full.names = TRUE)
        foldersWithAccBin = dirname(fnamesfull)
        noAccBin = allfolders[which(!allfolders %in% foldersWithAccBin)]
        warning(paste0("The following movisens data folders do not contain the ",
                       "acc.bin file with the accelerometer recording, and ",
                       "therefore cannot be processed in GGIR: ",
                       paste(noAccBin, collapse = ", ")), call. = FALSE)
      }
    } else {
      fnamesfull = datadir
      fnames = basename(fnamesfull)
    }
  }
  invisible(list(fnames = fnames, fnamesfull = fnamesfull))
}