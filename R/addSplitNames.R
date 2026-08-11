addSplitNames = function(x) {
  if (nrow(x) != 0) {
    x$filename = as.character(x$filename)
    for (spi in 1:nrow(x)) {
      splitnames = getSplitNames(as.character(x$filename[spi]))
      segment_names = splitnames$segment_names
      filename = splitnames$filename
      if (!is.null(segment_names)) {
        x$filename[spi] = filename
        if ("split1_name" %in% colnames(x) == FALSE) {
          x$split1_name = NA
          x$split2_name = NA
          col_index_filename = which(colnames(x) == "filename")
          
          x = x[,c(1:col_index_filename, 
                   which(colnames(x) %in% c("split1_name", "split2_name")),
                   (col_index_filename + 1):(ncol(x) - 2))] 
        }
        x$split1_name[spi] = segment_names[2]
        x$split2_name[spi] = segment_names[3]
      }
    }
    x$filename = as.factor(x$filename)
  }
  return(x)
}