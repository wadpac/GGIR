matchExternalData = function(externalDatadir = NULL, metadatadir = NULL,
                             f0 = 0, f1 = NULL,
                             colname = NULL, idloc = NULL, desiredtz = "",
                             overwrite = FALSE, verbose = TRUE) {

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
  if (f0 == 0 | is.null(f0) | f0 > length(fns)) f0 = 1
  if (f1 == 0 | is.null(f1) | f1 > length(fns)) f1 = length(fns)
  S = do.call("rbind", lapply(X = fns[f0:f1], FUN = getInfo, idloc = idloc, tz = desiredtz))
  ext_S = do.call("rbind", lapply(X = ext_fns, FUN = getExternalInfo, tz = desiredtz))

  # define new colname
  if (is.null(colname)) colname = "external_data"

  # match data
  S = S[!is.na(S$ID),]
  S = S[order(S$ID, S$start), ]
  S$overlap = NA
  # Identify recordings that can be appended (ID needs to match)
  doubleID = intersect(S$ID, ext_S$ID)
  for (j in 1:nrow(S)) {
    if (verbose) cat(paste0(j, " "))
    if (exists("extSrow")) rm(extSrow)
    Srow = j
    extSrow = which(ext_S$ID == S$ID[j])

    # load files
    M = I = C = filefoldername = filename_dir = tail_expansion_log = NULL
    load(S$filename[Srow])

    if (length(extSrow) > 0) {
      external = loadRData(ext_S$filename[extSrow])
      external = external[, c("timestamp", colname)]

      # is there an overlap?
      S_interval = lubridate::as.interval(S[Srow, "start"], S[Srow, "end"])
      extS_interval = lubridate::as.interval(ext_S[extSrow, "start"], ext_S[extSrow, "end"])
      overlap = !is.na(lubridate::intersect(S_interval, extS_interval))

      # if overlap, then match data
      if (overlap) {
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
      else { # if no overlap...
        M$metashort[, colname] = NA
      }
    } else { # if no external sensor data...
      M$metashort[, colname] = NA
    }
    # store new metashort
    save(M, C, I, filefoldername, filename_dir, tail_expansion_log,
         file = S$filename[Srow])
  }


  # THIS FUNCTION DOES NOT PRODUCE OUTPUT IT ONLY MATCHES FILES
  return(NULL)
}
