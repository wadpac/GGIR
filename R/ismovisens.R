ismovisens = function(datadir){
        # ---------------------------
        # In some functions, the file, and not the datadir is introduced in the 
        # function. Now this allow to handle files or directories
        isFile = strsplit(datadir, ".", fixed = T)
        if(length(isFile[[1]]) >= 2) {  
                datadir_tmp = strsplit(datadir, "/")
                datadir_ln = length(datadir_tmp[[1]]) - 1
                datadir = paste(datadir_tmp[[1]][1:datadir_ln], collapse = "/")
        }
        # ---------------------------
        unisensXML = paste(datadir, "unisens.xml", sep = "/")
        is.mv = file.exists(unisensXML)
        return(is.mv)
}
      
