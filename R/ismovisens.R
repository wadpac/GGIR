ismovisens = function(data){
        # ---------------------------
        # Is data a directory? Then use the first file to test if recorded with movisens
        directory = dir(data, recursive = T, full.names = T)[1]
        isdir = !is.na(directory)
        if(isdir == TRUE) data = directory
        # ---------------------------
        # Now, I focus on the participant directory
        p_dir = strsplit(data, ".", fixed = T)
        data_tmp = strsplit(p_dir[[1]], "/")
        data_ln = length(data_tmp[[1]]) - 1
        data = paste(data_tmp[[1]][1:data_ln], collapse = "/")
        # ---------------------------
        unisensXML = paste(data, "unisens.xml", sep = "/")
        is.mv = file.exists(unisensXML)
        return(is.mv)
}
