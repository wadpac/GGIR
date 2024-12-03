g.analyse.perfile = function(I, C, metrics_nav,
                             AveAccAve24hr, doquan, doiglevels, tooshort, 
                             params_247, params_cleaning, params_general,
                             output_avday, output_perday,
                             dataqual_summary, file_summary) {
  
  # extract objects from lists in input:
  cosinor_coef = output_avday$cosinor_coef
  daysummary = output_perday$daysummary
  ds_names = output_perday$ds_names
  lookat = metrics_nav$lookat
  colnames_to_lookat = metrics_nav$colnames_to_lookat
  
  filesummary = matrix(" ", 1, 150) #matrix to be stored with summary per participant
  s_names = rep(" ", ncol(filesummary))
  vi = 1
  # tidy up daysummary
  cut = which(ds_names == " " | ds_names == "" | is.na(ds_names) == T)
  if (length(cut > 0)) {
    ds_names = ds_names[-cut]
    daysummary = daysummary[,-cut]
  }
  # for a very small file, there could be just one row in daysummary[-cut,], so it gets coerced to a vector.
  # But what we actually need is a 1-row matrix. So we need to transpose it. 
  if(is.vector(daysummary)) { 
    daysummary = t(daysummary)
  }

  # Person identification number
  filesummary[vi] = file_summary$ID
  # Identify which of the metrics are in g-units to aid deciding whether to multiply by 1000
  g_variables_lookat = lookat[grep(x = colnames_to_lookat, pattern = "BrondCount|ZCX|ZCY|NeishabouriCount|ExtAct", invert = TRUE)]
  
  # Serial number
  filesummary[(vi + 1)] = file_summary$deviceSerialNumber
  s_names[vi:(vi + 1)] = c("ID","device_sn")
  vi = vi + 2
  # starttime of measurement, body location, filename
  filesummary[vi] = file_summary$sensor.location
  filesummary[(vi + 1)] = file_summary$fname
  filesummary[(vi + 2)] = file_summary$startt # starttime of measurement
  s_names[vi:(vi + 2)] = c("bodylocation","filename","start_time")
  vi = vi + 3
  # weekday on which measurement started, sample frequency and device
  filesummary[vi] = file_summary$wdayname
  filesummary[(vi + 1)] = I$sf
  filesummary[(vi + 2)] = I$monn
  s_names[vi:(vi + 2)] = c("startday", "samplefreq", "device")
  vi = vi + 3
  # clipsing score and measurement duration in days
  filesummary[vi] = dataqual_summary$clipping_score
  filesummary[vi + 1] = dataqual_summary$meas_dur_dys #measurement duration in days
  # completeness core and measurement duration with different definitions
  filesummary[vi + 2] = dataqual_summary$dcomplscore #completeness of the day
  filesummary[vi + 3] = dataqual_summary$meas_dur_def_proto_day #measurement duration according to protocol
  filesummary[vi + 4] = dataqual_summary$wear_dur_def_proto_day #wear duration in days (out of measurement protocol)
  s_names[vi:(vi + 4)] = c("clipping_score", "meas_dur_dys", "complete_24hcycle", 
                           "meas_dur_def_proto_day", "wear_dur_def_proto_day")
  vi = vi + 5
  if (!is.null(params_cleaning[["nonwearFiltermaxHours"]])) {
    filesummary[vi] = dataqual_summary$nonwearHoursFiltered
    filesummary[vi + 1] = dataqual_summary$nonwearEventsFiltered
    s_names[vi:(vi + 1)] = c("nonwear_hours_filtered", "nonwear_events_filtered")
    vi = vi + 2
  }
  # calibration error after auto-calibration
  if (length(C$cal.error.end) == 0)   C$cal.error.end = c(" ")
  filesummary[vi] = C$cal.error.end
  filesummary[vi + 1] = C$QCmessage
  for (la in 1:length(lookat)) {
    AveAccAve24hr[la] = 	AveAccAve24hr[la] * ifelse(test = lookat[la] %in% g_variables_lookat, yes = 1000, no = 1)
  }
  q0 = length(AveAccAve24hr) + 1
  filesummary[(vi + 2):(vi + q0)] = AveAccAve24hr
  colnames_to_lookat = paste0(colnames_to_lookat, "_fullRecordingMean")
  s_names[vi:(vi + q0)] = c("calib_err",
                            "calib_status", colnames_to_lookat)
  vi = vi + q0 + 2
  # QClog summary
  if ("Dur_chsum_failed" %in% names(file_summary)) {
    # readAxivity QClog
    # These are summaries of the file health check by the GGIRread::readAxivity
    # the function handles data blocks (1-3 seconds) with faulty data by imputing 
    # them and logging the information.
    # Normally we do not expect issue with cwa files, but by logging the information
    # we will facilitate better insight into when this happens.
    filesummary[vi:(vi + 6)] = c(ifelse(is.null(file_summary$Dur_imputed), " ", file_summary$Dur_imputed), # total imputed
                                 ifelse(is.null(file_summary$Dur_chsum_failed), " ", file_summary$Dur_chsum_failed), # checksum
                                 ifelse(is.null(file_summary$Dur_nonincremental), " ", file_summary$Dur_nonincremental), # nonincremental id between blocks
                                 ifelse(is.null(file_summary$Dur_freqissue_5_10), " ", file_summary$Dur_freqissue_5_10), # bias 5-10%
                                 ifelse(is.null(file_summary$Dur_freqissue_10_20), " ", file_summary$Dur_freqissue_10_20), # bias 10-20%
                                 ifelse(is.null(file_summary$Dur_freqissue_20_30), " ", file_summary$Dur_freqissue_20_30), # bias 20-30%
                                 ifelse(is.null(file_summary$Dur_freqissue_30), " ", file_summary$Dur_freqissue_30)) # bias >30%
    s_names[vi:(vi + 6)] = c("filehealth_totimp_min",
                             "filehealth_checksumfail_min",
                             "filehealth_niblockid_min", # non incremental block id
                             "filehealth_fbias0510_min", # frequency bias
                             "filehealth_fbias1020_min", 
                             "filehealth_fbias2030_min",
                             "filehealth_fbias30_min")
    vi = vi + 7
    filesummary[vi:(vi + 6)] = c( ifelse(is.null(file_summary$Nblocks_imputed), " ", file_summary$Nblocks_imputed),
                                  ifelse(is.null(file_summary$Nblocks_chsum_failed), " ", file_summary$Nblocks_chsum_failed),
                                  ifelse(is.null(file_summary$Nblocks_nonincremental), " ", file_summary$Nblocks_nonincremental),
                                  ifelse(is.null(file_summary$Nblock_freqissue_5_10), " ", file_summary$Nblock_freqissue_5_10),
                                  ifelse(is.null(file_summary$Nblock_freqissue_10_20), " ", file_summary$Nblock_freqissue_10_20),
                                  ifelse(is.null(file_summary$Nblock_freqissue_20_30), " ", file_summary$Nblock_freqissue_20_30),
                                  ifelse(is.null(file_summary$Nblock_freqissue_30), " ", file_summary$Nblock_freqissue_30))
    s_names[vi:(vi + 6)] = c("filehealth_totimp_N",
                             "filehealth_checksumfail_N",
                             "filehealth_niblockid_N",
                             "filehealth_fbias0510_N",
                             "filehealth_fbias1020_N",
                             "filehealth_fbias2030_N",
                             "filehealth_fbias30_N")
    vi = vi + 7
  } else if ("Dur_imputed" %in% names(file_summary)) {
    # ActiGraph QClog
    # This also logs time gaps in ActiGraph files, which correspond with periods
    # in which the idle sleep mode has been activated
    filesummary[vi:(vi + 1)] = c(file_summary$Dur_imputed, # total imputed
                                 file_summary$Nblocks_imputed)
    s_names[vi:(vi + 1)] = c("filehealth_totimp_min",
                    "filehealth_totimp_N")
    vi = vi + 2
  }
  
  #quantile, ML5, and intensity gradient variables
  if (doquan == TRUE) {
    q1 = length(output_avday$QUAN)
    filesummary[vi:((vi - 1) + q1)] = output_avday$QUAN * ifelse(test = lookat[la] %in% g_variables_lookat, yes = 1000, no = 1)
    s_names[vi:((vi - 1) + q1)] = paste0(output_avday$qlevels_names, "_fullRecording")
    vi = vi + q1
    q1 = length(output_avday$ML5AD)
    filesummary[vi:((vi - 1) + q1)] = as.numeric(output_avday$ML5AD)
    s_names[vi:((vi - 1) + q1)] = paste0(output_avday$ML5AD_names, "_fullRecording")
    vi = vi + q1
  }
  if (doiglevels == TRUE) {
    # intensity gradient (as described by Alex Rowlands 2018)
    # applied to the averageday per metric (except from angle metrics)
    q1 = length(output_avday$igfullr)
    filesummary[vi:((vi - 1) + q1)] = output_avday$igfullr
    s_names[vi:((vi - 1) + q1)] = paste0(output_avday$output_avday$igfullr_names, "_fullRecording")
    vi = vi + q1
  }
  if (tooshort == 0) {
    #====================================================================
    # Summarise per recording (not per day) - additional variables if the recording is long enough
    #====================================================================
    # Recognise weekenddays with enough data
    wkend  = which(daysummary[, which(ds_names == "weekday")] == "Saturday" |
                     daysummary[, which(ds_names == "weekday")] == "Sunday")
    columnWithAlwaysData = which(ds_names == "N hours" | ds_names == "N_hours")
    NVHcolumn = which(ds_names == "N valid hours" | ds_names == "N_valid_hours" ) #only count in the days for which the inclusion criteria is met
    v1 = which(is.na(as.numeric(daysummary[wkend, columnWithAlwaysData])) == F &
                 as.numeric(daysummary[wkend, NVHcolumn]) >= params_cleaning[["includedaycrit"]][1])
    wkend = wkend[v1]
    wkday  = which(daysummary[,which(ds_names == "weekday")] != "Saturday" & daysummary[,which(ds_names == "weekday")] != "Sunday")
    v2 = which(is.na(as.numeric(daysummary[wkday, columnWithAlwaysData])) == F  &
                 as.numeric(daysummary[wkday, NVHcolumn]) >= params_cleaning[["includedaycrit"]][1])
    wkday = wkday[v2]
    # Add number of weekend and weekdays to filesummary
    filesummary[vi:(vi + 1)] = c(length(wkend),  length(wkday)) # number of weekend days & weekdays
    iNA = which(is.na(filesummary[vi:(vi + 1)]) == TRUE)
    if (length(iNA) > 0) filesummary[(vi:(vi + 1))[iNA]] = 0
    s_names[vi:(vi + 1)] = c("N valid WEdays","N valid WKdays")
    vi = vi + 2
    # Cosinor analysis + IV + IS + phi
    if (length(cosinor_coef) > 0) {
      filesummary[vi]  = c(cosinor_coef$timeOffsetHours)
      s_names[vi] = c("cosinor_timeOffsetHours")
      vi = vi + 1
      try(expr = {filesummary[vi:(vi + 5)]  = as.numeric(c(cosinor_coef$coef$params$mes,
                                                           cosinor_coef$coef$params$amp,
                                                           cosinor_coef$coef$params$acr,
                                                           cosinor_coef$coef$params$acrotime,
                                                           cosinor_coef$coef$params$ndays,
                                                           cosinor_coef$coef$params$R2))}, silent = TRUE)
      s_names[vi:(vi + 5)] = c("cosinor_mes", "cosinor_amp", "cosinor_acrophase",
                               "cosinor_acrotime", "cosinor_ndays", "cosinor_R2")
      vi = vi + 6
      try(expr = {filesummary[vi:(vi + 10)]  = c(cosinor_coef$coefext$params$minimum,
                                                 cosinor_coef$coefext$params$amp,
                                                 cosinor_coef$coefext$params$alpha,
                                                 cosinor_coef$coefext$params$beta,
                                                 cosinor_coef$coefext$params$acrotime,
                                                 cosinor_coef$coefext$params$UpMesor,
                                                 cosinor_coef$coefext$params$DownMesor,
                                                 cosinor_coef$coefext$params$MESOR,
                                                 cosinor_coef$coefext$params$ndays,
                                                 cosinor_coef$coefext$params$F_pseudo,
                                                 cosinor_coef$coefext$params$R2)}, silent = TRUE)
      s_names[vi:(vi + 10)] = c("cosinorExt_minimum", "cosinorExt_amp", "cosinorExt_alpha",
                                "cosinorExt_beta", "cosinorExt_acrotime", "cosinorExt_UpMesor",
                                "cosinorExt_DownMesor", "cosinorExt_MESOR",
                                "cosinorExt_ndays", "cosinorExt_F_pseudo", "cosinorExt_R2")
      vi = vi + 11
      filesummary[vi:(vi + 2)]  = c(cosinor_coef$IVIS$InterdailyStability,
                                    cosinor_coef$IVIS$IntradailyVariability,
                                    cosinor_coef$IVIS$phi)
      s_names[vi:(vi + 2)] = c("IS", "IV", "phi")
      vi = vi + 3
    } else {
      vi = vi + 21
    }
    
    # Variables per metric - summarise with stratification to weekdays and weekend days
    daytoweekvar = c(5:length(ds_names))
    md = which(ds_names[daytoweekvar] %in% c("measurementday", "weekday", "qwindow_timestamps", "qwindow_names"))
    if (length(md) > 0) daytoweekvar = daytoweekvar[-md]
    
    dtwtel = 0
    if (length(daytoweekvar) >= 1) {
      sp = length(daytoweekvar) + 1
      for (dtwi in daytoweekvar) {
        #check whether columns is empty:
        uncona = unique(daysummary[,dtwi])
        storevalue = !(length(uncona) == 1 & length(params_247[["qwindow"]]) > 2 & uncona[1] == "")
        if (is.na(storevalue) == TRUE) storevalue = FALSE
        # Only do next 15-isch lines of code if:
        # - there is more than 1 day of data
        # - there are multiple daysegments (qwindow)
        # - first value is not empty
        if (storevalue == TRUE) {
          # Plain average of available days
          v4 = mean(suppressWarnings(as.numeric(daysummary[, dtwi])), na.rm = TRUE)
          filesummary[(vi + 1 + (dtwtel*sp))] = v4 # #average all availabel days
          s_names[(vi + 1 + (dtwtel * sp))] = paste0("AD_", ds_names[dtwi])
          # Average of available days per weekenddays / weekdays:
          dtw_wkend = suppressWarnings(as.numeric(daysummary[wkend,dtwi]))
          dtw_wkday = suppressWarnings(as.numeric(daysummary[wkday,dtwi]))
          filesummary[(vi + 2 + (dtwtel * sp))] = suppressWarnings(mean(dtw_wkend, na.rm = TRUE))
          filesummary[(vi + 3 + (dtwtel * sp))] = suppressWarnings(mean(dtw_wkday, na.rm = TRUE))
          s_names[(vi + 2 + (dtwtel * sp))] = paste0("WE_", ds_names[dtwi]) # Weekend no weighting
          s_names[(vi + 3 + (dtwtel * sp))] = paste0("WD_", ds_names[dtwi]) # Weekdays no weighting
          # Weighted average of available days per weekenddays / weekdays:
          if (length(dtw_wkend) > 2) {
            dtw_wkend = c((dtw_wkend[1] + dtw_wkend[3]) / 2, dtw_wkend[2])
          }
          if (length(dtw_wkday) > 5) {
            dtw_wkday = c((dtw_wkday[1] + dtw_wkday[6]) / 2, dtw_wkday[2:5])
          }
          filesummary[(vi + 4 + (dtwtel * sp))] = suppressWarnings(mean(dtw_wkend, na.rm = TRUE))
          filesummary[(vi + 5 + (dtwtel * sp))] = suppressWarnings(mean(dtw_wkday, na.rm = TRUE))
          s_names[(vi + 4 + (dtwtel * sp))] = paste0("WWE_", ds_names[dtwi]) # Weekend with weighting
          s_names[(vi + 5 + (dtwtel * sp))] = paste0("WWD_", ds_names[dtwi]) # Weekdays with weighting
          dtwtel = dtwtel + 1
        }
        # Plain average of available days
        v4 = mean(suppressWarnings(as.numeric(daysummary[,dtwi])), na.rm = TRUE)
        filesummary[(vi + 1 + (dtwtel * sp))] = v4
        s_names[(vi + 1 + (dtwtel * sp))] = paste0("AD_", ds_names[dtwi])
        # Average of available days per weekenddays / weekdays:
        dtw_wkend = suppressWarnings(as.numeric(daysummary[wkend,dtwi]))
        dtw_wkday = suppressWarnings(as.numeric(daysummary[wkday,dtwi]))
        if (storevalue == TRUE) {
          filesummary[(vi + 2 + (dtwtel * sp))] = mean(dtw_wkend, na.rm = TRUE)
          filesummary[(vi + 3 + (dtwtel * sp))] = mean(dtw_wkday, na.rm = TRUE)
        }
        s_names[(vi + 2 + (dtwtel * sp))] = paste0("WE_", ds_names[dtwi]) # Weekend no weighting
        s_names[(vi + 3 + (dtwtel * sp))] = paste0("WD_", ds_names[dtwi]) # Weekdays no weighting
        # Weighted average of available days per weekenddays / weekdays:
        if (length(dtw_wkend) > 2) {
          dtw_wkend = c((dtw_wkend[1] + dtw_wkend[3]) / 2, dtw_wkend[2])
        }
        if (length(dtw_wkday) > 5) {
          dtw_wkday = c((dtw_wkday[1] + dtw_wkday[6]) / 2, dtw_wkday[2:5])
        }
        if (storevalue == TRUE) {
          filesummary[(vi + 4 + (dtwtel * sp))] = mean(dtw_wkend, na.rm = TRUE) # Weekend with weighting
          filesummary[(vi + 5 + (dtwtel * sp))] = mean(dtw_wkday, na.rm = TRUE) # Weekdays with weighting
        }
        s_names[(vi + 4 + (dtwtel * sp))] = paste0("WWE_", ds_names[dtwi])
        s_names[(vi + 5 + (dtwtel * sp))] = paste0("WWD_", ds_names[dtwi])
        dtwtel = dtwtel + 1
      }
      vi = vi + 6 + ((dtwtel * sp) - 1)
    }
    filesummary[vi] = params_cleaning[["data_masking_strategy"]]
    filesummary[(vi + 1)] = params_cleaning[["hrs.del.start"]]
    filesummary[(vi + 2)] = params_cleaning[["hrs.del.end"]]
    filesummary[(vi + 3)] = params_cleaning[["maxdur"]]
    filesummary[(vi + 4)] = params_general[["windowsizes"]][1]
    filesummary[(vi + 5)] = metrics_nav$longitudinal_axis_id
    #get GGIR version
    GGIRversion = "GGIR not used"
    if (is.element('GGIR', installed.packages()[,1])) {
      GGIRversion = as.character(utils::packageVersion("GGIR"))
      if (length(GGIRversion) != 1) GGIRversion = sessionInfo()$otherPkgs$GGIR$Version
    }
    filesummary[(vi + 6)] = GGIRversion #"2014-03-14 12:14:00 GMT"
    s_names[vi:(vi + 6)] = as.character(c(paste0("data exclusion stategy (value=1, ignore specific hours;",
                                                 " value=2, ignore all data before the first midnight and",
                                                 " after the last midnight)"),
                                          "n hours ignored at start of meas (if data_masking_strategy=1)",
                                          "n hours ignored at end of meas (if data_masking_strategy=1)",
                                          "n days of measurement after which all data is ignored (if data_masking_strategy=1)",
                                          "epoch size to which acceleration was averaged (seconds)",
                                          "if_hip_long_axis_id", "GGIR version"))
    vi = vi + 7
  }
  # tidy up daysummary object
  mw = which(is.na(daysummary) == T)
  mw = c(mw, grep(pattern = "NaN", x = daysummary))
  if (length(mw) > 0) {
    daysummary[mw] = " "
  }
  
  if (min(dim(as.matrix(daysummary))) == 1) {
    if (nrow(as.matrix(daysummary)) != 1) {
      daysummary = t(daysummary) #if there is only one day of data
    }
  }
  daysummary = data.frame(value = daysummary, stringsAsFactors = FALSE)
  names(daysummary) = ds_names
  # remove double columns with 1-6am variables
  columnswith16am = grep("1-6am", x = colnames(daysummary))
  if (length(columnswith16am) > 1) {
    daysummary = daysummary[,-columnswith16am[2:length(columnswith16am)]]
  }
  # tidy up filesummary object
  mw = which(is.na(filesummary) == T)
  mw = c(mw, grep(pattern = "NaN", x = filesummary))
  if (length(mw) > 0) {
    filesummary[mw] = " "
  }
  cut = which(as.character(s_names) == " " | as.character(s_names) == "" | is.na(s_names) == T | duplicated(s_names) |
                s_names %in% c("AD_", "WE_", "WD_", "WWD_", "WWE_",
                               "AD_N hours", "WE_N hours", "WD_N hours", "WWD_N hours", "WWE_N hours",
                               "AD_N valid hours", "WE_N valid hours", "WD_N valid hours", "WWD_N valid hours", "WWE_N valid hours"))
  if (length(cut) > 0) {
    s_names = s_names[-cut]
    filesummary = filesummary[-cut]
  }
  filesummary = data.frame(value = t(filesummary), stringsAsFactors = FALSE) #needs to be t() because it will be a column otherwise
  names(filesummary) = s_names
  
  columns2order = c()
  if (ncol(filesummary) > 37) {
    columns2order = grep(pattern = "AD_|WE_|WD_|WWD_|WWE_", x = names(filesummary))
  }
  options(encoding = "UTF-8")
  if (length(columns2order) > 0) {
    selectcolumns = c(names(filesummary)[1:(columns2order[1] - 1)],
                      grep(pattern = "^AD_", x = names(filesummary), value = T),
                      grep(pattern = "^WD_", x = names(filesummary), value = T),
                      grep(pattern = "^WE_", x = names(filesummary), value = T),
                      grep(pattern = "^WWD_", x = names(filesummary), value = T),
                      grep(pattern = "^WWE_", x = names(filesummary), value = T))
    selectcolumns = c(selectcolumns, names(filesummary)[which(names(filesummary) %in% selectcolumns == FALSE)])
  } else {
    selectcolumns = names(filesummary)
  }
  selectcolumns = selectcolumns[which(selectcolumns %in% colnames(filesummary) == TRUE)]
  filesummary = filesummary[,selectcolumns]
  filesummary = filesummary[,!duplicated(filesummary)]
  invisible(list(filesummary = filesummary, daysummary = daysummary))
}
