getfolderstructure = function(datadir=c(),referencefnames=c()) {
  filelist = isfilelist(datadir)
  if (filelist == FALSE) {
    fnamesfull = dir(datadir, recursive = TRUE, pattern = "[.](csv|bin|Rda|wav|cw|gt3)")
  } else {
    fnamesfull = datadir
  }
  fullfilenames = foldername = rep("",length(referencefnames))
  if (length(fnamesfull) > 0) {
    fnamesshort = apply(X=as.matrix(fnamesfull),MARGIN=1,FUN=function(X) basename(X))
    foldername_new = apply(X=as.matrix(fnamesfull),MARGIN=1,FUN=function(X) basename(dirname(X)))
    for (i in 1:length(referencefnames)) {
      index_match = which(fnamesshort == referencefnames[i] & referencefnames[i] %in% c("", " ",NA) == FALSE)
      if (length(index_match) > 0) {
        fullfilenames[i] = fnamesfull[index_match]
        foldername[i] = foldername_new[index_match]
      }
    }
  }
  invisible(list(fullfilenames=fullfilenames,foldername=foldername))
}