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
      fnamesfull = dir(datadir, recursive = TRUE, pattern = "acc.bin$", full.names = TRUE)
      foldersWithAccBin = dirname(fnamesfull)
      fnames = basename(foldersWithAccBin)

      allfolders = list.dirs(datadir, recursive=FALSE, full.names = TRUE)

      # are there any folders that don't contain acc.bin (directly, or in any subfolders)?
      noAccBin = c()
      for (fld in allfolders) {
        if (fld %in% foldersWithAccBin) next # folder contains acc.bin

        # do any subfolders contain acc.bin?
        if (!any(grepl(paste(fld, '/', sep = ''), foldersWithAccBin))) {
          noAccBin = c(noAccBin, fld)
        }
      }
      
      if (length(noAccBin) > 0) {
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