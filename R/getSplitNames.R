getSplitNames = function(filename) {
  # extract file split names from filename
  segment_names = NULL
  if (length(grep(pattern = "_split", x =  filename)) > 0) {
    filename_tmp1 = unlist(strsplit(filename, "_split|[.]_split"))
    filename_tmp2 = unlist(strsplit(filename_tmp1[2], "[.]"))
    segment_names = unlist(strsplit(filename_tmp2[1], "_|TO"))
    filename = paste0(filename_tmp1[1], ".", filename_tmp2[2])
  }
  invisible(list(segment_names = segment_names, filename = filename))
}