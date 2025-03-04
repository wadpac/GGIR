splitRecords = function(metadatadir, desiredtz = "", idloc = 1, recordingSplitTimes = NULL) {
  
  # Declare local functions:
  getInfo = function(fn, idloc, tz) {
    load(fn)
    if (is.null(M$metashort)) return()
    hvars = g.extractheadervars(I)
    if (exists("Clist")) {
      ID = NA # If Clist exists then ignore this file as it was previously appended
    } else {
      ID = extractID(hvars, idloc, fname = I$filename)
    }
    start = as.POSIXct(x = M$metashort$timestamp[1], format = "%Y-%m-%dT%H:%M:%S%z", tz = tz)
    end = as.POSIXct(x = M$metashort$timestamp[nrow(M$metashort)], format = "%Y-%m-%dT%H:%M:%S%z", tz = tz)
    info = data.frame(ID = ID, start = start, end = end, filename = fn, brand = I$monn)
    return(info)
  }
  #==================================================
  # Main code:
  filefoldername = filename_dir = tail_expansion_log = NULL
  # Create overview of all recordings ID, start time, end time, and filename
  fns = dir(paste0(metadatadir, "/meta/basic"), full.names = TRUE)
  
  S = do.call("rbind", lapply(X = fns, FUN = getInfo, idloc = idloc, tz = desiredtz)) 
  
  
  splitTime = data.table::fread(recordingSplitTimes)
  if (length(S) > 0 & !is.null(recordingSplitTimes)) {
    S = S[!is.na(S$ID),]
    S = S[order(S$ID, S$start), ]
    # S$overlap = NA
    
    # Identify recordings that can be appended (ID and brand need to match)
    doubleID = unique(S$ID[duplicated(S[, c("ID", "brand")])])
    if (length(doubleID) > 0) {
      for (j in 1:length(doubleID)) {
        Mlist = Ilist = Clist = list()
        # Does time interval overlap?
        # Label the recordings that are groups, this can be more than 2 recordings
        rowsOfInterest = which(S$ID == doubleID[j])
        Nrec = length(which(S$ID == doubleID[j]))
        for (k in Nrec:2) {
          overlap = as.numeric(difftime(S$end[rowsOfInterest[k - 1]], S$start[rowsOfInterest[k]], units = "secs"))
          # positive overlap is true overlap, negative overlap is a gap
          # Add load number
          S$overlap[rowsOfInterest[k]] = overlap 
          load(S$filename[rowsOfInterest[k]])
          Mlist[[k]] = M
          Ilist[[k]] = I
          Clist[[k]] = C
        }
        # First recording
        load(S$filename[rowsOfInterest[1]])
        Mlist[[1]] = M
        Ilist[[1]] = I
        Clist[[1]] = C
        
        # Append all recordings if they are a sequence of nearby recordings
        cnt = 0
        for (k in (Nrec:1)) {
          if (!is.na(S$overlap[rowsOfInterest[k]])) {
            interval = S$overlap[rowsOfInterest[k]]
          } else {
            interval = -2e10
          }
          if (interval > -abs(maxRecordingInterval) * 3600 & k > 1) {
            # Merge this pair
            Mlist[[k - 1]] = mergePair(M1 = Mlist[[k - 1]],
                                       M2 =  Mlist[[k]],
                                       overlap = S$overlap[rowsOfInterest[k]],
                                       tz = desiredtz)
            interval = round(interval / 3600, digits = 3)
            if ("interval" %in% names(Ilist[[k]])) {
              Ilist[[k - 1]]$interval = c(Ilist[[k]]$interval, interval)
            } else {
              Ilist[[k - 1]]$interval = interval
            }
            # Remove duplicate files
            unlink(S$filename[rowsOfInterest[k]], recursive = TRUE)
            cnt = cnt + 1 # keep track of how many files were appended
          } else {
            if (cnt > 0) {
              # we need to have at least two recording appended to make it worth saving the data
              M = Mlist[[k]]
              C = Clist[[k]]
              I = Ilist[[k]]
              # save as new milestone data for part 1
              # Note that we also keep copy of I and C for each of the original recordings
              # this to retain this information for reporting in part 2
              Nappended = cnt
              C_list = Clist[c(k:(cnt + 1))]
              I_list = Ilist[c(k:(cnt + 1))]
              save(M, C, I, C_list, I_list,
                   filefoldername, filename_dir, tail_expansion_log, Nappended,
                   file = S$filename[rowsOfInterest[k]])
            }
            cnt = 0 # start again
          }
        }
      }
    }
  }
  # THIS FUNCTION DOES NOT PRODUCE OUTPUT IT ONLY APPENDS FILES
  return(NULL)
}
