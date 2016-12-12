isfilelist = function(datadir) {
  filelist = FALSE
  #   verify whether datadir is a directory or a list of files
  if (length(datadir) == 1) { #could be a directory or one file
    if (length(unlist(strsplit(datadir,"[.]bi")))>1) filelist = TRUE
    if (length(unlist(strsplit(datadir,"[.]cs")))>1) filelist = TRUE
    if (length(unlist(strsplit(datadir,"[.]wa")))>1) filelist = TRUE
  } else { #multiple files
    filelist = TRUE    
  }
  return(filelist)
}