ismovisens = function(datadir){
        unisensXML = paste(datadir, "unisens.xml", sep = "/")
        is.mv = file.exists(unisensXML)
        return(is.mv)
}
      
