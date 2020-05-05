ismovisens = function(fnames){
        if (TRUE %in% grepl("acc.bin", fnames) & 
        TRUE %in% grepl("temp.bin", fnames) & 
        TRUE %in% grepl("angularrate.bin", fnames)) {
                for (filei in 1:length(fnames)) {
                        fnames[[filei]] = strsplit(fnames[[filei]], "/")[[1]][1]
                }
                fnames = unique(fnames)
                is.mv = TRUE
        }
        else {
                is.mv = FALSE
        }
        return(is.mv)
        }
      
