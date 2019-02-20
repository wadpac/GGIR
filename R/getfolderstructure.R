getfolderstructure = function(datadir=c(),referencefnames=c()) {
  filelist = isfilelist(datadir)
  if (filelist == FALSE) {
    fnamesfull = dir(datadir, recursive = TRUE, pattern = "[.](csv|bin|Rda|wav|cw)")
  } else {
    fnamesfull = datadir
  }
  f16 = function(X) {
    out = unlist(strsplit(X,"/"))
    f16 = out[length(out)]
  }
  f17 = function(X) {
    out = unlist(strsplit(X,"/"))
    f17 = out[(length(out)-1)]
  }
  fullfilenames = foldername = rep("",length(fnamesfull))

  if (length(fnamesfull) > 0) {
    fnamesshort = apply(X=as.matrix(fnamesfull),MARGIN=1,FUN=f16)
    foldername = apply(X=as.matrix(fnamesfull),MARGIN=1,FUN=f17)
    for (i in 1:length(referencefnames)) {
      if (length(which(fnamesshort == referencefnames[i])) > 0) {
        fullfilenames[i] = fnamesfull[which(fnamesshort == referencefnames[i])]
        foldername[i] = foldername[which(fnamesshort == referencefnames[i])]
      }
    }
  }
  invisible(list(fullfilenames=fullfilenames,foldername=foldername))
}