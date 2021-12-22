g.part4_extractid = function(idloc, fname, dolog, sleeplogidnum, sleeplog) {
  #------------------------------------------------------
  # extract the identifier from accelerometer data
  if (idloc == 2 | idloc == 5) { #idloc is an argument to specify where the participant identifier can be found
    if (idloc == 2) {
      getCharBeforeUnderscore = function(x) {
        return(as.character(unlist(strsplit(x,"_")))[1])
      }
    } else {
      getCharBeforeUnderscore = function(x) {
        return(as.character(unlist(strsplit(x," ")))[1])
      }
    }
    accid = apply(as.matrix(as.character(fname)),MARGIN=c(1),FUN=getCharBeforeUnderscore)
    accid_bu = accid
    getLastCharacterValue = function(x) {
      tmp = as.character(unlist(strsplit(x,"")))
      return(tmp[length(tmp)])
    }
    letter = apply(as.matrix(accid),MARGIN=c(1),FUN=getLastCharacterValue)
    for (h in 1:length(accid)) {
      options(warn=-1)
      numletter = as.numeric(letter[h])
      options(warn=0)
      if (is.na(numletter) == TRUE) { # do not remove latest character if it is a number
        accid[h] = as.character(unlist(strsplit(accid[h],letter[h]))[1])
      }
    }
    accid = suppressWarnings(as.numeric(accid))
    #catch for files with only id in filename and for whom the above attempt to extract the id failed:
    if (is.na(accid) == TRUE) accid = accid_bu
  } else { # get id from filename
    newaccid = fname
    if (length(unlist(strsplit(newaccid,"_"))) > 1) newaccid = unlist(strsplit(newaccid,"_"))[1]
    if (length(unlist(strsplit(newaccid," "))) > 1) newaccid = unlist(strsplit(newaccid," "))[1]
    if (length(unlist(strsplit(newaccid,"[.]RDa"))) > 1) newaccid = unlist(strsplit(newaccid,"[.]RDa"))[1]
    if (length(unlist(strsplit(newaccid,"[.]cs"))) > 1) newaccid = unlist(strsplit(newaccid,"[.]cs"))[1]
    accid = newaccid[1]
  }
  # get matching identifier from sleeplog
  if (dolog == TRUE) {
    accid_num = suppressWarnings(as.numeric(accid))
    if (sleeplogidnum == FALSE) {
      matching_indices_sleeplog = which(as.character(sleeplog$ID) == as.character(accid))
      if (length(matching_indices_sleeplog) == 0) {
        matching_indices_sleeplog_alternative = which(sleeplog$ID == accid_num)
        if (length(matching_indices_sleeplog_alternative) > 0) {
          warning("\nArgument sleeplogidnum is set to FALSE, but it seems the identifiers are
                    stored as numeric values, you may want to consider changing sleeplogidnum to TRUE")
        }
      }
    } else {
      matching_indices_sleeplog = which(sleeplog$ID == accid_num)
      if (length(matching_indices_sleeplog) == 0) {
        matching_indices_sleeplog_alternative = which(as.character(sleeplog$ID) == as.character(accid))
        if (length(matching_indices_sleeplog_alternative) > 0) {
          warning("\nArgument sleeplogidnum is set to TRUE, but it seems the identifiers are
                    stored as character values, you may want to consider changing sleeplogidnum to TRUE")
        } else {
          
          if (is.na(accid_num) == TRUE) { # format probably incorrect
            warning(paste0("\nSleeplog id is stored as format: ", as.character(sleeplog$ID[1]),", while
                           code expects format: ",as.character(accid[1])))
          }
        }
      }
    }
    
  } else {
    matching_indices_sleeplog = 1
  }
  invisible(list(accid=accid, matching_indices_sleeplog=matching_indices_sleeplog))
}