datadir2fnames = function(datadir,filelist) {
  if (filelist == FALSE) {
    fnames = c(dir(datadir,recursive=TRUE,pattern="[.]csv"),
               dir(datadir,recursive=TRUE,pattern="[.]bin"),
               dir(datadir,recursive=TRUE,pattern="[.]wav"),
               dir(datadir,recursive=TRUE,pattern="[.]cwa"))
    fnamesRD = dir(datadir,recursive=TRUE,pattern="[.]RD")
    if (length(fnames) == length(fnamesRD)) { #because filenames may have both .bin in the middle and .RData
      fnames = c()
      fnames = fnamesRD
    }
  } else {
    fnames = datadir
  }
  return(fnames)
}