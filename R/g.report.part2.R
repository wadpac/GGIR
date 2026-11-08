g.report.part2 = function(metadatadir = c(), f0 = c(), f1 = c(), 
                          store.long = FALSE, params_output, myfun = c(), verbose = TRUE,
                          desiredtz = "") {
  ms2.out = "/meta/ms2.out"
  if (file.exists(paste0(metadatadir,ms2.out))) {
    if (length(dir(paste0(metadatadir,ms2.out))) == 0) {
      try.generate.report = FALSE
    } else {
      try.generate.report = TRUE
    }
  } else {
    try.generate.report = FALSE
  }
  if (try.generate.report == TRUE) {
    #---------------------------------
    # Specifying directories with meta-data and extracting filenames
    path = paste0(metadatadir, "/meta/basic/")  #values stored per long epoch, e.g. 15 minutes
    fnames = dir(path) # part 1
    fnames.ms2 = dir(paste0(metadatadir,ms2.out))  #part 2
    #---------------------------------
    SUMMARY = daySUMMARY = c()
    
    if (length(f0) ==  0) f0 = 1
    if (length(f1) ==  0) f1 = length(fnames)
    if (f0 > length(fnames)) f0 = 1
    if (f1 > length(fnames)) f1 = length(fnames)
    
    #-----------------------------
    # Loop through all the files
    for (i in f0:f1) {
      if (verbose == TRUE) cat(paste0(" ",i))
      # First load part 1 data
      M = c()
      part1_fname2read = paste0(path, fnames[i])
      try(expr = {load(part1_fname2read)}, silent = TRUE) #reading RData-file
      # Checks for availability of file and data
      if (length(M) == 0) {
        tmp = unlist(strsplit(part1_fname2read, "/"))
        part1_fname2read_display = paste0(tmp[length(tmp) - 1], "/", tmp[length(tmp)])
        warning(paste0("Struggling to read: ", part1_fname2read_display))
        next
      }
      fname = as.character(unlist(strsplit(fnames[i],"eta_"))[2])
      selp = which(fnames.ms2 == fname)
      if (length(selp) == 0 ) {
        if (verbose == TRUE) cat(paste0("File ", fname, " not available in part 2"))
      }
      # If part 1 milestone data indicates that file was useful
      # try loading part 2
      if (M$filecorrupt == FALSE & M$filetooshort == FALSE & length(selp) > 0) { 
        # Load part 2 data
        SUM = IMP = c()
        part2_fname2read = paste0(metadatadir, ms2.out, "/", fnames.ms2[selp])
        try(expr = {load(file = part2_fname2read)}, silent = TRUE)
        # Checks for availability of file and data
        if (length(IMP) == 0) {
          warning(paste0("Struggling to read: ", part2_fname2read))
        }
        if (M$filecorrupt == FALSE & M$filetooshort == FALSE) {
          if (i == 1 | i == f0) {
            SUMMARY = SUM$summary
            daySUMMARY = SUM$daysummary
          } else {
            bind_with_prev_data = function(df1, df2) {
              df1 = data.table::rbindlist(list(df1, df2), fill = TRUE)
              df1 = as.data.frame(df1)
              return(df1)
            }
            SUMMARY = bind_with_prev_data(SUMMARY, SUM$summary)
            daySUMMARY = bind_with_prev_data(daySUMMARY, SUM$daysummary)
          }
        }
      }
      if (length(SUMMARY) == 0 | length(daySUMMARY) == 0) {
        warning("No summary data available to be stored in csv-reports", call. = FALSE)
      }
      #-----------------
      # create data quality report
      if (length(C$cal.error.end) == 0) C$cal.error.end = " "
      if (M$filetooshort == TRUE | M$filecorrupt == TRUE) {
        C$cal.error.start = 0
        C$npoints = 0
      }
      tm = which(colnames(M$metalong) == "temperaturemean")
      if (length(tm) > 0) {
        tmean = as.character(mean(as.numeric(as.matrix(M$metalong[1:(nrow(M$metalong) - 1), tm]))))
      } else {
        tmean = ""
      }
      #=========
      if (is.null(I$sf) == FALSE) {
        header = I$header
        hnames = rownames(header)
        hvalues = as.character(as.matrix(header))
        pp = which(hvalues == "")
        hvalues[pp] = c("not stored in header")
      }
      mon = I$monn
      
      if (mon == "genea") {
        deviceSerialNumber = hvalues[which(hnames == "Serial_Number")] #serial number
      } else if (mon == "geneactive") {
        if ("Device_Unique_Serial_Code" %in% hnames) {
          deviceSerialNumber = hvalues[which(hnames == "Device_Unique_Serial_Code")] #serial number
          if (I$dformn == "csv") { #if it was stored in csv-format then underscores were replaced by spaces (by company)
            deviceSerialNumber = hvalues[which(hnames == "Device Unique Serial Code")] #serial number
          }
        } else {
          deviceSerialNumber = hvalues[which(hnames == "serial_number")] #serial number
        }
      } else if (mon == "actigraph" | mon == "axivity" | mon == "verisense") {
        deviceSerialNumber = "not extracted"
      } else if (I$monc %in% c(5, 7, 94, 96, 97, 98, 99)) { #movisense 5, parmay matrix 7, Actiwatch 98, Sensewear 99
        deviceSerialNumber = "not extracted"
      } else if (I$monc == 0) {
        if (header != "no header") {
          deviceSerialNumber = hvalues[which(hnames == "device_serial_number")]
          if (length(deviceSerialNumber) == 0) {
            deviceSerialNumber = "not extracted"
          }
        } else {
          deviceSerialNumber = "not extracted"
        }
      }
      if (length(C$cal.error.start) == 0) {
        C$cal.error.start = " "
      }
      if (length(C$npoints) == 0) {
        C$npoints = " "
      }
      if (length(C$tempoffset) == 0) {
        C$tempoffset = c(0, 0, 0)
      }
      if (length(M$NFilePagesSkipped) == 0) M$NFilePagesSkipped = 0 # to make the code work for historical part1 output.
      fname2store = unlist(strsplit(fnames[i], "eta_|[.]RDat"))[2]
      QC = data.frame(filename = fname2store,
                      file.corrupt = M$filecorrupt,
                      file.too.short = M$filetooshort,
                      use.temperature = C$use.temp,
                      scale.x = C$scale[1], scale.y = C$scale[2], scale.z = C$scale[3],
                      offset.x = C$offset[1], offset.y = C$offset[2], offset.z = C$offset[3],
                      temperature.offset.x = C$tempoffset[1],  temperature.offset.y = C$tempoffset[2],
                      temperature.offset.z = C$tempoffset[3],
                      cal.error.start = C$cal.error.start,
                      cal.error.end = C$cal.error.end,
                      n.10sec.windows = C$npoints,
                      n.hours.considered = C$nhoursused, QCmessage = C$QCmessage, mean.temp = tmean,
                      device.serial.number = deviceSerialNumber,
                      NFilePagesSkipped = M$NFilePagesSkipped, stringsAsFactors = FALSE)
      if (is.null(I$sf) == TRUE) {
        # When ActiGraph is corrupt
        # Note that QC shape is consistent here
        if (i == 1 | i == f0) {
          QCout = QC
        } else {
          # fill up missing columns with NA
          QC[setdiff(names(QCout), names(QC))] <- NA
          QCout = rbind(QCout, QC)
        }
        next
      }
      
      filehealth_cols = grep(pattern = "filehealth", x = names(SUMMARY), value = FALSE)
      if (length(filehealth_cols) > 0) {
        # migrate filehealth columns to QC report, only applicable to Axivity data
        QC = merge(x = QC, y = SUMMARY[, c(which(names(SUMMARY) == "filename"), filehealth_cols)], by = "filename")
        SUMMARY = SUMMARY[, -filehealth_cols]
      }
      if (i == 1 | i == f0) {
        QCout = QC
      } else {
        n1 = ncol(QCout)
        n2 = ncol(QC)
        # n1 and n2 are expected to be consistent, but in case of Axivity possibly not
        if (n1 > n2) {
          QC = cbind(QC, matrix(" ", 1, (n1 - n2)))
          colnames(QC) = colnames(QCout)
          QC = QC[, colnames(QCout)] # reorder to match order of QCout
        } else if (n1 < n2) {
          newcolnames = colnames(QC)[which(colnames(QC) %in% colnames(QCout) == FALSE)]
          newcols = (n1 + 1):(n1 +  length(newcolnames))
          QCout = cbind(QCout, matrix(" ", 1, n2 - n1))
          colnames(QCout)[newcols] = newcolnames
          QCout = QCout[, colnames(QC)] # reorder to match order of QC
        }
        QCout = rbind(QCout, QC)
      }
    }
    # tidy up memory
    if (exists("IMP")) rm(IMP)
    rm(M); rm(I)

    #--------------------------------------
    # Store Event/Type reports
    
    # Extract event name for legacy backwards compatibility (if user explicitly named their event)
    eventName = "event"
    if (length(myfun) > 0) {
      if ("name" %in% names(myfun)) {
        eventName =  myfun$name
      }
    }
    
    # Store Event and/or Type reports
    # split daySUMMARY into event, type and standard output
    ds_names = names(daySUMMARY)
    EventVars_day = grep(pattern = "ExtFunEvent_", x = ds_names, value = TRUE)
    TypeVars_day = grep(pattern = "ExtFunType_", x = ds_names, value = TRUE)
    
    if (length(EventVars_day) > 0) {
      dayEVENTSUMMARY = daySUMMARY[ , c("ID", "filename", "calendar_date",
                                        "N valid hours", "N hours", "weekday",
                                        sort(EventVars_day))]
      names(dayEVENTSUMMARY) = gsub(pattern = "ExtFunEvent_", replacement = "", x = names(dayEVENTSUMMARY))
      dayEVENTSUMMARY_clean = tidyup_df(dayEVENTSUMMARY)
      dayEVENTSUMMARY_clean = addSplitNames(dayEVENTSUMMARY_clean)  # If recording was split
      data.table::fwrite(x = dayEVENTSUMMARY_clean,
                         file = paste0(metadatadir, "/results/part2_day", eventName, "summary.csv"),
                         row.names = F, na = "", sep = params_output[["sep_reports"]],
                         dec = params_output[["dec_reports"]])
    }
    
    if (length(TypeVars_day) > 0) {
      dayTYPESUMMARY = daySUMMARY[ , c("ID", "filename", "calendar_date",
                                       "N valid hours", "N hours", "weekday",
                                       sort(TypeVars_day))]
      names(dayTYPESUMMARY) = gsub(pattern = "ExtFunType_", replacement = "", x = names(dayTYPESUMMARY))
      dayTYPESUMMARY_clean = tidyup_df(dayTYPESUMMARY)
      dayTYPESUMMARY_clean = addSplitNames(dayTYPESUMMARY_clean)  # If recording was split
      data.table::fwrite(x = dayTYPESUMMARY_clean,
                         file = paste0(metadatadir, "/results/part2_daytypesummary.csv"),
                         row.names = F, na = "", sep = params_output[["sep_reports"]],
                         dec = params_output[["dec_reports"]])
    }
    
    # Strip both variable families out of daySUMMARY
    VarsToRemove_day = c(EventVars_day, TypeVars_day)
    if (length(VarsToRemove_day) > 0) {
      daySUMMARY = daySUMMARY[, !names(daySUMMARY) %in% VarsToRemove_day]
    }
    
    # split SUMMARY into event, type and standard output
    s_names = names(SUMMARY)
    EventVars_S = grep(pattern = "ExtFunEvent_", x = s_names, value = TRUE)
    TypeVars_S = grep(pattern = "ExtFunType_", x = s_names, value = TRUE)
    
    if (length(EventVars_S) > 0) {
      EVENTSUMMARY = SUMMARY[ , c("ID", "filename", "start_time",
                                  "wear_dur_def_proto_day",
                                  sort(EventVars_S))]
      names(EVENTSUMMARY) = gsub(pattern = "ExtFunEvent_", replacement = "", x = names(EVENTSUMMARY))
      EVENTSUMMARY_clean = tidyup_df(EVENTSUMMARY)
      EVENTSUMMARY_clean = addSplitNames(EVENTSUMMARY_clean)  # If recording was split
      data.table::fwrite(x = EVENTSUMMARY_clean,
                         file =  paste0(metadatadir, "/results/part2_", eventName, "summary.csv"),
                         row.names = F, na = "", sep = params_output[["sep_reports"]],
                         dec = params_output[["dec_reports"]])
    }
    if (length(TypeVars_S) > 0) {
      TYPESUMMARY = SUMMARY[ , c("ID", "filename", "start_time",
                                 "wear_dur_def_proto_day",
                                 sort(TypeVars_S))]
      names(TYPESUMMARY) = gsub(pattern = "ExtFunType_", replacement = "", x = names(TYPESUMMARY))
      TYPESUMMARY_clean = tidyup_df(TYPESUMMARY)
      TYPESUMMARY_clean = addSplitNames(TYPESUMMARY_clean)  # If recording was split
      data.table::fwrite(x = TYPESUMMARY_clean,
                         file =  paste0(metadatadir, "/results/part2_typesummary.csv"),
                         row.names = F, na = "", sep = params_output[["sep_reports"]],
                         dec = params_output[["dec_reports"]])
    }
    
    # Strip both variable families out of the parent SUMMARY
    VarsToRemove_S = c(EventVars_S, TypeVars_S)
    if (length(VarsToRemove_S) > 0) {
      SUMMARY = SUMMARY[, !names(SUMMARY) %in% VarsToRemove_S]
    }
    #-----------------------------
    # tidy up data.frames
    if (length(SUMMARY) > 0 & length(daySUMMARY) > 0) {
      SUMMARY_clean = tidyup_df(SUMMARY)
      daySUMMARY_clean = tidyup_df(daySUMMARY)
      daySUMMARY_clean$start_time = daySUMMARY_clean$calendar_date
      # reorder to have starttime next to calendar_date
      old_vars = which(colnames(daySUMMARY_clean) != "start_time")
      new_var = which(colnames(daySUMMARY_clean) == "start_time")
      daySUMMARY_clean = daySUMMARY_clean[, c(old_vars[1:2], new_var, old_vars[3:length(old_vars)])]
      # format calendar dates
      dd = iso8601chartime2POSIX(daySUMMARY_clean$calendar_date, tz = desiredtz) 
      daySUMMARY_clean$calendar_date = format(dd, format = "%Y-%m-%d")
      #===============================================================================
      # store final matrices again
      SUMMARY_clean = addSplitNames(SUMMARY_clean)  # If recording was split
      daySUMMARY_clean = addSplitNames(daySUMMARY_clean)  # If recording was split
      data.table::fwrite(x = SUMMARY_clean, file = paste0(metadatadir, "/results/part2_summary.csv"),
                         row.names = F, na = "", sep = params_output[["sep_reports"]],
                         dec = params_output[["dec_reports"]])
      data.table::fwrite(x = daySUMMARY_clean, paste0(metadatadir, "/results/part2_daysummary.csv"),
                         row.names = F, na = "", sep = params_output[["sep_reports"]],
                         dec = params_output[["dec_reports"]])
      if (store.long == TRUE) { # Convert daySUMMARY to long format if there are multiple segments per day
        df = g.convert.part2.long(daySUMMARY)
        df_clean = tidyup_df(df)
        df_clean = addSplitNames(df_clean)  # If recording was split
        data.table::fwrite(x = df_clean, file = paste0(metadatadir, "/results/part2_daysummary_longformat.csv"),
                           row.names = F, na = "", sep = params_output[["sep_reports"]],
                           dec = params_output[["dec_reports"]])
      }
      QCout = addSplitNames(QCout)  # If recording was split
      data.table::fwrite(x = QCout, file = paste0(metadatadir, "/results/QC/data_quality_report.csv"),
                         row.names = F, na = "",
                         sep = params_output[["sep_reports"]],
                         dec = params_output[["dec_reports"]])
    }
  }
}
