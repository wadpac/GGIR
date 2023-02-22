g.part4_extractid = function(idloc, fname, dolog, sleeplog, accid = c()) {
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
    
    # convert to character
    logid = as.character(sleeplog$ID)
    accid2 = as.character(accid)
    
    # remove spaces in ID, to ease matching, because some accelerometer brands at several spaces behind ID
    logid = gsub(pattern = " ", replacement = "", x = as.character(logid))
    accid2 = gsub(pattern = " ", replacement = "", x = as.character(accid2))
    
    # attempt to match 1 - works if both IDs are identical
    matching_indices_sleeplog = which(logid == accid2)
    matched = length(matching_indices_sleeplog)
    matched_unique = unique(sleeplog$ID[matching_indices_sleeplog])
    
    # attempt to match 2 - ignore case
    if (matched == 0) {
      matching_indices_sleeplog = which(tolower(logid) == tolower(accid2))
      matched = length(matching_indices_sleeplog)
      matched_unique = unique(sleeplog$ID[matching_indices_sleeplog])
    } 
    
    # attempt to match 3 - get rid of letters
    if (matched == 0) {
      # remove all letters
      accid2 = gsub("[^0-9.-]", "", accid2)
      logid = gsub("[^0-9.-]", "", sleeplog$ID)
      matching_indices_sleeplog = which(logid == accid2)
      matched = length(matching_indices_sleeplog)
      matched_unique = unique(sleeplog$ID[matching_indices_sleeplog])
    }
    
    # attempt to match 4 - get rid of extra leading 0s
    if (matched == 0) {
      # remove leading zeros
      accid2 = gsub("^0+", "", accid2)
      logid = gsub("^0+", "", logid)
      matching_indices_sleeplog = which(logid == accid2)
      matched = length(matching_indices_sleeplog)
      matched_unique = unique(sleeplog$ID[matching_indices_sleeplog])
    }
    
    # if matched to more than one entrance in sleeplog, warn the user
    if (length(matched_unique) > 1) {
      warning(paste0("\n", as.character(accid), " matched to more than one entrance ",
                     "in the sleeplog (i.e., ", paste(as.character(matched_unique), collapse = ", "), 
                     ").\nPlease revise the IDs in your sleeplog. ", matched_unique[1], " used."))
      matching_indices_sleeplog = which(sleeplog$ID == matched_unique[1])
    }
  } else if (dolog == FALSE) { 
    matching_indices_sleeplog = 1
  }
  invisible(list(accid = accid, matching_indices_sleeplog = matching_indices_sleeplog))
}