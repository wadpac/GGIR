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
    fnamesfull = datadir
    fnames = basename(fnamesfull)
  }
  invisible(list(fnames = fnames, fnamesfull = fnamesfull))
}