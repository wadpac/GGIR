extractID = function(hvars, idloc, fname) {
  
  ID = hvars$ID
  iID = hvars$iID
  IDd = hvars$IDd
  
  # This part is specific to how data Pelotas cohort was stored.
  # It is legacy code from 2012, for later projects I tried to avoid
  # ending up with dataset specific solutions
  ID2 = ID
  iID2 = iID
  if (idloc == 3) { #remove hyphen in id-name for Pelotas id-numbers
    get_char_before_hyphen = function(x) {
      x2 = c()
      for (j in 1:length(x)) {
        temp = unlist(strsplit(x,"-"))
        if (length(temp) == 2) {
          x2[j] = as.character(temp[1])
        } else {
          x2[j] = as.character(x[j])
        }
      }
      return(x2)
    }
    ID2 = get_char_before_hyphen(ID)
    iID2 = get_char_before_hyphen(iID)
  }
  ID_NAs = which(ID == "NA")
  ID2_NAs = which(ID == "NA")
  if (length(ID_NAs) > 0) ID[ID_NAs] = iID[ID_NAs]
  if (length(ID2_NAs) > 0) ID2[ID2_NAs] = iID2[ID2_NAs]
  if (idloc == 2) { # default is idloc=1, where ID just stays ID
    ID = unlist(strsplit(fname,"_"))[1]
  } else if (idloc == 3) {
    ID = ID2
  } else if (idloc == 4) {
    ID = IDd
  } else if (idloc == 5) {
    ID = unlist(strsplit(fname," "))[1]
  } else if (idloc == 6) {
    ID = unlist(strsplit(fname,"[.]"))[1]
  } else if (idloc == 7) {
    ID = unlist(strsplit(fname,"-"))[1]
  }
  if (length(ID) == 0) { # If ID could not be extracted
    ID = basename(fname)
    warning(paste0("\nUnable to extract ID from, ", fname, ". Using filname instead. ",
                   " You may want to check argument idloc, which is currently set to ", idloc))
  } else {
    ID = gsub(pattern = " ", replacement = "", ID)
  }
  return(ID)
}
