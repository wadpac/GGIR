g.part4_extractid = function(idloc, fname, dolog,
                             sleeplog, accid = c()) {
  if (length(accid) == 0) {
    #------------------------------------------------------
    # extract the identifier from accelerometer data if it was not found in the GGIR part 3 milestone data
    if (idloc %in% c(2, 5, 6, 7) == TRUE) { #idloc is an argument to specify where the participant identifier can be found
      if (idloc == 2) {
        getIDfromChar = function(x) {
          return(as.character(unlist(strsplit(x, "_")))[1])
        }
      } else if (idloc == 5) {
        getIDfromChar = function(x) {
          return(as.character(unlist(strsplit(x, " ")))[1])
        }
      } else if (idloc == 6) {
        getIDfromChar = function(x) {
          return(as.character(unlist(strsplit(x, "[.]")))[1])
        }
      } else if (idloc == 7) {
        getIDfromChar = function(x) {
          return(as.character(unlist(strsplit(x, "-")))[1])
        }
      }
      accid = apply(as.matrix(as.character(fname)), MARGIN = c(1), FUN = getIDfromChar)
    } else { # get id from filename
      newaccid = fname
      if (length(unlist(strsplit(newaccid, "_"))) > 1) newaccid = unlist(strsplit(newaccid, "_"))[1]
      if (length(unlist(strsplit(newaccid, " "))) > 1) newaccid = unlist(strsplit(newaccid, " "))[1]
      if (length(unlist(strsplit(newaccid, "[.]RDa"))) > 1) newaccid = unlist(strsplit(newaccid, "[.]RDa"))[1]
      if (length(unlist(strsplit(newaccid, "[.]cs"))) > 1) newaccid = unlist(strsplit(newaccid, "[.]cs"))[1]
      accid = newaccid[1]
    }
  }
  # get matching identifier from sleeplog
  if (dolog == TRUE) {
    accid_num = suppressWarnings(as.numeric(accid))
    # if (sleeplogidnum == FALSE) {
    # remove spaces in ID, to ease matching, because some accelerometer brands at several spaces behind ID
    sleeplog$ID = as.character(sleeplog$ID)  
    sleeplog$ID = gsub(pattern = " ", replacement = "", x = as.character(sleeplog$ID))
    accid = gsub(pattern = " ", replacement = "", x = as.character(accid))
    # attempt to match
    matching_indices_sleeplog = which(as.character(sleeplog$ID) == as.character(accid))
    if (length(matching_indices_sleeplog) == 0) {
      matching_indices_sleeplog_alternative = which(sleeplog$ID == accid_num)
    }
  } else {
    matching_indices_sleeplog = 1
  }
  invisible(list(accid = accid, matching_indices_sleeplog = matching_indices_sleeplog))
}