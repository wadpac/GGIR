splitRecords = function(metadatadir, params_general = NULL) {
  
  desiredtz = params_general[["desiredtz"]]
  idloc = params_general[["idloc"]]
  windowsizes = params_general[["windowsizes"]]
  
  filefoldername = filename_dir = tail_expansion_log = NULL
  
  # Create overview of all recordings ID, start time, end time, and filename
  fns = dir(paste0(metadatadir, "/meta/basic"), full.names = TRUE)
  fns = fns[grep(pattern = "_split", x = basename(fns), invert = TRUE)]
  if (length(fns) == 0) return()
  S = do.call("rbind", lapply(X = fns, FUN = getPart1BasicInfo, idloc = idloc, tz = desiredtz)) 
  #------------------------------------
  # Load recording split times file
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
  if (length(grep(pattern = "%H:%M:%S",
                  x = params_general[["recording_split_timeformat"]])) > 0) {
    defaultTime = "00:00:00"
  } else {
    defaultTime = "00:00"
  }
  for (j in (IDcol + 1):ncol(splitTime)) {
    space_count = unlist(lapply(X = splitTime[, j], FUN = countSpaces))
    no_space = which(space_count == 0)
    if (length(no_space) > 0) {
      splitTime[no_space, j] = paste0(splitTime[no_space, j], " ", defaultTime)
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
          split_names = colnames(splitTime)[(IDcol + 1):ncol(splitTime)]
          # Only consider timestamps that overlap with recording
          within_time_range = which(splitTime_tmp >= S$start[j] &
                                      splitTime_tmp <= S$end[j])
          splitTime_tmp = splitTime_tmp[within_time_range]
          
          if (length(splitTime_tmp) == 0) next
          split_names = split_names[within_time_range]
          # tidy up split_names
          split_names = tolower(gsub(pattern = " |[.]|-|TO", replacement = "", x = split_names))
          split_names = substr(split_names, start = 1, stop = 10) # consider max 8 characters
          split_names = make.unique(split_names, sep = "") # make names unique
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
          segment_names = segment_starts = segment_ends = NULL
          # Define segments
          if (splitTime_tmp[1] > timestamp_short[1]) {
            segment_starts = timestamp_short[1]
            segment_ends = splitTime_tmp[1]
            segment_names = paste0("startrecTO", split_names[1])
          }
          for (segment_index in 1:length(splitTime_tmp)) {
            if (segment_index < length(splitTime_tmp)) {
              segment_starts = c(segment_starts, splitTime_tmp[segment_index])
              segment_ends = c(segment_ends, splitTime_tmp[segment_index + 1])
              segment_names = c(segment_names, paste0(split_names[segment_index], "TO",
                                                      split_names[segment_index + 1]))
            } else {
              segment_starts = c(segment_starts, splitTime_tmp[segment_index])
              segment_ends = c(segment_ends, timestamp_short[length(timestamp_short)])
              segment_names = c(segment_names, paste0(split_names[segment_index],
                                                      "TOendrec"))
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
                
                # Take RData filename and split it based on dot
                fname_tmp = unlist(strsplit(basename(S$filename[j]), "[.]"))
                # Last two elements are the RData + Original file extension (e.g. .AWD, .csv)
                newfilebase = paste0(fname_tmp[1:pmax(1, length(fname_tmp) - 2)], collapse = ".")
                # Remember original file extension
                extension = fname_tmp[length(fname_tmp) - 1]
                # Reassamble file name but use character used for locating the ID
                # to ensure that the new part never becomes part of the extracted ID
                # In other words activity and sleep diary (part 2, 4 and 5) can
                # keep using the general participant ID and do not need to work with
                # segment specific IDs
                if (idloc == 6) {
                  id_separator = "._"
                } else {
                  id_separator = "_"
                }
                newFileName = paste0(newfilebase, id_separator, "split", g, "_", segment_names[g], ".", extension)
                # add back to I$filename because part 2 uses this in the csv report
                filename_dir = gsub(pattern = "meta_", replacement = "", x = newFileName)
                I$filename = filename_dir
                newRDataFileName = paste0(dirname(S$filename[j]), "/", newFileName, ".RData")
                file_was_split = TRUE
                
                # update weekday code and name
                wday = as.POSIXlt(x = timestamp_short[1], tz = desiredtz)$wday + 1
                weekdays = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
                M$wday = wday
                M$wdayname = weekdays[wday]
                # save
                save(M, C, I,
                     filefoldername, filename_dir, tail_expansion_log,
                     file = newRDataFileName)
              }
            }
            if (file_was_split == TRUE) {
              # Delete original RData file
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
