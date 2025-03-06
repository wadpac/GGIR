splitRecords = function(metadatadir, params_general = NULL) {
  
  desiredtz = params_general[["desiredtz"]]
  idloc = params_general[["idloc"]]
  windowsizes = params_general[["windowsizes"]]
  
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
  
  # Ignore recordings that were previously split
  # The function will always split all files that do not have _split in their name
  if (length(S) > 0) {
    S[grep(pattern = "_split", x = basename(S$filename), invert = TRUE), ]
  }
  
  #------------------------------------
  # Load recordingsSplitTimes
  splitTime = data.table::fread(params_general[["recording_split_times"]], data.table = FALSE, stringsAsFactors = FALSE, colClasses = "character")
  
  # Identify ID columns
  IDcol = grep(pattern = "ID", x = colnames(splitTime))
  if (length(IDcol) == 0) {
    stop(paste0("\nFile specified by recording_split_times does not have a ",
                "column with ID in it, please fix."), call. = FALSE)
  }
  # Make sure timestamps have date and time
  countSpaces = function(x) {
    return(length(unlist(strsplit(x, " "))) - 1)
  }
  for (j in (IDcol + 1):ncol(splitTime)) {
    space_count = unlist(lapply(X = splitTime[, j], FUN = countSpaces))
    no_space = which(space_count == 0)
    if (length(no_space) > 0) {
      splitTime[no_space, j] = paste0(splitTime[no_space, j], " 00:00")
    }
  }
  if (length(S) > 0) S = S[!is.na(S$ID),]
  if (length(S) > 0 & length(splitTime) > 0) {
    # Identify recordings that need to be split
    buffer = params_general[["recording_split_overlap"]] * 3600
    for (j in 1:nrow(S)) {
      # matching ID(s) between milestone data and splitTime
      thisID = which(splitTime[, IDcol] == S$ID[j])
      if (length(thisID) > 0) {
        for (i in 1:length(thisID)) {
          splitTime_tmp = as.character(splitTime[thisID[i], (IDcol + 1):ncol(splitTime)])
          splitTime_tmp = as.POSIXct(splitTime_tmp, tz = desiredtz, format = params_general[["recording_split_timeformat"]])
          
          splitTime_tmp = as.POSIXct(round(as.numeric(splitTime_tmp) / windowsizes[2]) * windowsizes[2], tz = desiredtz)
          # Only consider timestamps that overlap with recording
          splitTime_tmp = splitTime_tmp[which(splitTime_tmp >= S$start[j] &
                                                splitTime_tmp <= S$end[j])]
          if (length(splitTime_tmp) == 0) next
          if (all(is.na(splitTime_tmp))) {
            stop(paste0("Timestamp format ", splitTime[thisID[i], (IDcol + 1):ncol(splitTime)],
                        " not recognised. You may want to check parameter ",
                        "recording_split_timeformat", call. = FALSE))
          }
          # If yes, use those splits to split the recording
          load(file = S$filename[j])
          timestamp_short = iso8601chartime2POSIX(x = M$metashort$timestamp, tz = desiredtz)
          timestamp_long = iso8601chartime2POSIX(x = M$metalong$timestamp, tz = desiredtz)
          Mbu = M
          segment_starts = segment_ends = NULL
          # Define segments
          if (splitTime_tmp[1] > timestamp_short[1]) {
            segment_starts = timestamp_short[1]
            segment_ends = splitTime_tmp[1]
          }
          for (segment_index in 1:length(splitTime_tmp)) {
            if (segment_index < length(splitTime_tmp)) {
              segment_starts = c(segment_starts, splitTime_tmp[segment_index])
              segment_ends = c(segment_ends, splitTime_tmp[segment_index + 1])
            } else {
              segment_starts = c(segment_starts, splitTime_tmp[segment_index])
              segment_ends = c(segment_ends, timestamp_short[length(timestamp_short)])
            }
          }
          # Store each part separately
          if (length(segment_starts) > 0) {
            file_was_split = FALSE
            # round to resolution that matches long epoch
            # segment_starts = as.POSIXct(round(as.numeric(segment_starts) / windowsizes[2]) * windowsizes[2], tz = desiredtz)
            buffer = round((buffer / 2) / windowsizes[2]) * windowsizes[2]
            for (g in 1:length(segment_starts)) {
              # Take subsection
              segment_short = which(timestamp_short >= segment_starts[g] - buffer &
                                      timestamp_short < segment_ends[g] + buffer)
              segment_long = which(timestamp_long >= segment_starts[g] - buffer &
                                     timestamp_long <= segment_ends[g] + buffer)
              # Make long a multitude of short
              Nshort = length(segment_short)
              Nlong = length(segment_long)
              segment_short = segment_short[1:pmin(Nshort, Nlong * (windowsizes[2] / windowsizes[1]))]
              Nshort = length(segment_short)
              segment_long = segment_long[1:pmin(Nlong, floor(Nshort / (windowsizes[2] / windowsizes[1])))]
              
              Nhours_long = length(segment_long) /  (3600 / windowsizes[2])
              # Only save if there are at least 12 hours of data
              if (Nhours_long > 2) {
                M$metashort = Mbu$metashort[segment_short, ]
                M$metalong = Mbu$metalong[segment_long, ]
                # Save the split
                newRDataFileName = unlist(strsplit(S$filename[j], "[.]RData"))
                newRDataFileName = paste0(newRDataFileName, "_split", g, ".RData")
                file_was_split = TRUE
                save(M, C, I,
                     filefoldername, filename_dir, tail_expansion_log,
                     file = newRDataFileName)
              }
            }
            if (file_was_split == TRUE) {
              # Delete original
              unlink(S$filename[j], recursive = TRUE)
            }
          }
        }
      }
    }
  }
  # THIS FUNCTION DOES NOT PRODUCE OUTPUT IT ONLY SPLITS FILES
  return()
}
