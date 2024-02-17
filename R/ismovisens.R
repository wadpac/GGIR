ismovisens = function(data){
        # ---------------------------
        # Is data a directory? Then use the first file in this directory 
        # to test if recorded with movisens
        first_file = dir(data, recursive = T, full.names = T)[1]
        isdir = !is.na(first_file)
        if (isdir) data = first_file
        # ---------------------------
        # At this point, data is guaranteed to be a path to a file.
        # Now, focus on the directory containing this file.
        data_tmp = strsplit(data, "/")

        data_ln = length(data_tmp[[1]]) - 1
        data = paste(data_tmp[[1]][1:data_ln], collapse = "/")
        # ---------------------------
        unisensXML = paste(data, "unisens.xml", sep = "/")
        is.mv = file.exists(unisensXML)
        return(is.mv)
}
