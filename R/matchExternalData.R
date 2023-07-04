matchExternalData = function(externalDatadir = NULL, metadatadir = NULL,
                             colname = NULL, idloc = NULL, desiredtz = "",
                             overwrite = FALSE, verbose = TRUE) {
  # load metadata part1
  # load RData with external sensor
  # merge data

  #==================================================
  # Declare local functions:
  loadRData = function(fileName){
    #loads an RData file, and returns it
    load(fileName)
    get(ls()[ls() != "fileName"])
  }
  getInfo = function(fn, idloc, tz) {
    load(fn)
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
  getExternalInfo = function(fn, tz) {
    ext_data = loadRData(fn)
    fname = basename(fn)
    # extract ID - for now it assumes RData including timestamp column and
    # named as ID.RData
    ID = gsub(".RData", "", fname)
    # start and end time
    start = as.POSIXct(x = ext_data$timestamp[1], tz = tz)
    end = as.POSIXct(x = ext_data$timestamp[nrow(ext_data)], tz = tz)
    info = data.frame(ID = ID, start = start, end = end, filename = fn)
    return(info)
  }

  #==================================================
  # Main code:
  filefoldername = filename_dir = tail_expansion_log = NULL
  S = ext_S = NULL

  # Create overview of all recordings ID, start time, end time, and filename
  fns = dir(paste0(metadatadir, "/meta/basic"), full.names = TRUE)
  ext_fns = dir(externalDatadir, full.names = TRUE)
  S = do.call("rbind", lapply(X = fns, FUN = getInfo, idloc = idloc, tz = desiredtz))
  ext_S = do.call("rbind", lapply(X = ext_fns, FUN = getExternalInfo, tz = desiredtz))

  # define new colname
  if (is.null(colname)) colname = "external_data"

  # match data
  if (length(S) > 0 & length(ext_S) > 0) {
    S = S[!is.na(S$ID),]
    S = S[order(S$ID, S$start), ]
    S$overlap = NA
    # Identify recordings that can be appended (ID needs to match)
    doubleID = intersect(S$ID, ext_S$ID)
    if (length(doubleID) > 0) {
      for (j in 1:nrow(S)) {
        if (verbose) cat(paste0(j, " "))
        Srow = j
        extSrow = which(ext_S$ID == S$ID[j])

        if (length(extSrow) > 0) {
          # is there an overlap?
          S_interval = lubridate::as.interval(S[Srow, "start"], S[Srow, "end"])
          extS_interval = lubridate::as.interval(ext_S[extSrow, "start"], ext_S[extSrow, "end"])
          overlap = !is.na(lubridate::intersect(S_interval, extS_interval))

          # if overlap, then match data
          if (overlap) {
            M = I = C = filefoldername = filename_dir = tail_expansion_log = NULL
            load(S$filename[Srow])
            external = loadRData(ext_S$filename[extSrow])
            external = external[, c("timestamp", colname)]

            # OVERWRITE?
            domatch = ifelse(overwrite == TRUE, TRUE, FALSE)
            if (colname %in% colnames(M$metashort)) {
              # remove column if overwrite = TRUE
              if (overwrite == TRUE) {
                col2rm = which(colnames(M$metashort) == colname)
                M$metashort = M$metashort[, -col2rm]
              }
            } else {
              domatch = TRUE
            }

            # merge data (external timestamp should be in iso8601 format)
            M$metashort = merge(M$metashort, external, by = "timestamp", all.x = TRUE)

          }
        } else {
          M$metashort[, colname] = NA
        }
        # store new metashort
        save(M, C, I, filefoldername, filename_dir, tail_expansion_log,
             file = S$filename[Srow])
      }
    }
  }
  # THIS FUNCTION DOES NOT PRODUCE OUTPUT IT ONLY MATCHES FILES
  return(NULL)
}
