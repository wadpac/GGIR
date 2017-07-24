getfolderstructure = function(datadir=c()) {
  filelist = isfilelist(datadir)
  if (filelist == FALSE) {
    # fnamesfull = c(dir(datadir,recursive=TRUE,pattern="[.]csv"),dir(datadir,recursive=TRUE,pattern="[.]bin"))
    fnamesfull = dir(datadir, recursive = TRUE, pattern = "[.](csv|bin|Rda|wav)")
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
    for (i in 1:length(fnames.ms3)) {
      ff = as.character(unlist(strsplit(fnames.ms3[i],".RDa"))[1])
      if (length(which(fnamesshort == ff)) > 0) {
        fullfilenames[i] = fnamesfull[which(fnamesshort == ff)]
        foldername[i] = foldername[which(fnamesshort == ff)]
      }
    }
  }
  invisible(list(fullfilenames=fullfilenames,foldername=foldername))
}