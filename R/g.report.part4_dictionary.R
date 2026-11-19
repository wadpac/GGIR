g.report.part4_dictionary = function(metadatadir, params_output) {
  # night level variables ---------------------------------------------------
  nightDictionary = c(
    ID = "File/participant identifier",
    night = "Night number in the recording",
    sleeponset = "Sleep onset time (hours since the midnight before the night started, e.g., 23.5 = 23:30 and 25 = 01:00)",
    wakeup = "Wake up time (hours since the midnight before the night started, e.g., 31 = 07:00 next morning)",
    SptDuration = "Duration of the sleep period time window, wakeup - sleeponset (hours)",
    sleepparam = "Definition of sustained inactivity bouts used (T = time threshold in minutes, A = angle threshold in degrees)",
    guider_onset = "Start of the sleep period time window according to the guider (hours since the midnight before the night started)",
    guider_wakeup = "End of the sleep period time window according to the guider (hours since the midnight before the night started)",
    guider_SptDuration = "Duration of the sleep period time window according to the guider (hours)",
    guider_inbedStart = "Time of going to bed according to the guider (hours since the midnight before the night started)",
    guider_inbedEnd = "Time of getting out of bed according to the guider (hours since the midnight before the night started)",
    guider_inbedDuration = "Time in bed according to the guider, guider_inbedEnd - guider_inbedStart (hours)",
    error_onset = "Difference between sleeponset and guider_onset (hours)",
    error_wake = "Difference between wakeup and guider_wakeup (hours)",
    error_dur = "Difference between SptDuration and guider_SptDuration (hours)",
    fraction_night_invalid = "Fraction of the night (noon-noon or 6pm-6pm) with invalid data",
    SleepDurationInSpt = "Total sleep duration, i.e., accumulated sustained inactivity bouts within the sleep period time window (hours)",
    WASO = "Wake after sleep onset, SptDuration - SleepDurationInSpt (hours)",
    duration_sib_wakinghours = "Accumulated duration of sustained inactivity bouts outside the sleep period time window (hours)",
    number_sib_sleepperiod = "Number of sustained inactivity bouts within the sleep period time window",
    number_of_awakenings = "Number of awakenings within the sleep period time window, i.e., number_sib_sleepperiod - 1",
    number_sib_wakinghours = "Number of sustained inactivity bouts outside the sleep period time window",
    duration_sib_wakinghours_atleast15min = "Accumulated duration of sustained inactivity bouts of at least 15 minutes outside the sleep period time window (hours)",
    sleeponset_ts = "Sleep onset time (hh:mm:ss)",
    wakeup_ts = "Wake up time (hh:mm:ss)",
    guider_onset_ts = "Start of the sleep period time window according to the guider (hh:mm:ss)",
    guider_wakeup_ts = "End of the sleep period time window according to the guider (hh:mm:ss)",
    guider_inbedStart_ts = "Time of going to bed according to the guider (hh:mm:ss)",
    guider_inbedEnd_ts = "Time of getting out of bed according to the guider (hh:mm:ss)",
    sleeplatency = "Sleep latency, sleeponset - guider_inbedStart (hours) (only calculated when sleepwindowType = 'TimeInBed' or when the marker button is the guider)",
    sleepefficiency = "Sleep efficiency as defined with sleepefficiency.metric: SleepDurationInSpt / guider_inbedDuration (sleepefficiency.metric = 1, default) or SleepDurationInSpt / (SptDuration + sleeplatency) (sleepefficiency.metric = 2) (only calculated when sleepwindowType = 'TimeInBed' or when the marker button is the guider)",
    page = "Page number of the night in visualisation_sleep.pdf",
    daysleeper = "Night classified as daysleeper, i.e., sleep period time window overlaps with noon (1) or not (0)",
    weekday = "Day of the week on which the night started (full name)",
    calendar_date = "Calendar date on which the night started",
    filename = "File name",
    cleaningcode = "Cleaning code for the sleep period time classification (0=no problem; 1=no sleeplog; 2=insufficient valid data; 3=no acc data available; 4=no nights; 5=guider-defined SPT; 6=SPT not found)",
    sleeplog_used = "Whether sleep log information was used for the identification of the sleep period time (TRUE/FALSE)",
    sleeplog_ID = "Participant identifier as found in the sleep log",
    acc_available = "Whether accelerometer data was available for the identification of the sleep period time (TRUE/FALSE)",
    guider = "Guider used for the sleep period time identification",
    SleepRegularityIndex1 = "Sleep Regularity Index for the day pair starting on this calendar date, based on sustained inactivity bouts",
    SriFractionValid = "Fraction of the 24 hours that was valid on both days used to calculate SleepRegularityIndex1",
    longitudinal_axis = "Longitudinal axis of the accelerometer used by the HorAngle guider (only hip-worn data)",
    guider_corrected = "Guider correction applied with guider_cor_do (0=not corrected; 1=window expanded with neighbouring windows; 2=main guider window changed; 3=both)",
    GGIRversion = "GGIR version used to process this recording in part 4 (stored in the milestone data), which may differ from the version used to generate the report",
    filename_dir = "Full path of the file (only if storefolderstructure = TRUE)",
    foldername = "Name of the folder containing the file (only if storefolderstructure = TRUE)",
    window = "Window number in part 5 (only when part 5 was run with timewindow WW)",
    nonwear_perc_spt = "Non-wear time during the sleep period time window as calculated in part 5 (%) (only when part 5 was run with timewindow WW)",
    ACC_spt_mg = "Mean acceleration during the sleep period time window as calculated in part 5 (mg) (only when part 5 was run with timewindow WW)"
  )
  # person level variables (not derived from a night level variable) ----------
  personDictionary = c(
    ID = "File/participant identifier",
    filename = "File name",
    calendar_date = "Calendar date of the first night in the recording",
    weekday = "Day of the week of the first night in the recording",
    sleeplog_used = "Whether sleep log information was used (TRUE/FALSE)",
    sleeplog_ID = "Participant identifier as found in the sleep log",
    n_nights_acc = "Number of nights with accelerometer data",
    n_nights_sleeplog = "Number of nights with sleep log data",
    n_nights_markerbutton = "Number of nights in which the marker button was used as guider",
    n_WE_nights_complete = "Number of complete weekend nights (Friday and Saturday nights)",
    n_WD_nights_complete = "Number of complete weekday nights (Sunday to Thursday nights)",
    n_WEnights_daysleeper = "Number of weekend nights classified as daysleeper",
    n_WDnights_daysleeper = "Number of weekday nights classified as daysleeper",
    N_nights_guider_corrected = "Number of nights in which the guider was corrected with guider_cor_do",
    N_nights_negative_latency = "Number of nights with a negative sleep latency, which are excluded from the sleep latency and sleep efficiency averages",
    GGIRversion = "GGIR version used to process this recording in part 4 (stored in the milestone data), which may differ from the version used to generate the report"
  )
  extraPersonDictionary = c(
    average_dur_sib_wakinghours = "Average duration of the sustained inactivity bouts outside the sleep period time window, duration_sib_wakinghours / number_sib_wakinghours (hours)",
    n_days_w_sib_wakinghours = "Number of days with at least one sustained inactivity bout outside the sleep period time window",
    sleep_efficiency = "Sleep efficiency as defined with sleepefficiency.metric"
  )
  aggDays = c(AD = "all nights", WD = "weekday nights (Sunday to Thursday)",
              WE = "weekend nights (Friday and Saturday)")
  # main script -------------------------------------------------------------
  resultsdir = file.path(metadatadir, "results")
  reports = file.path(resultsdir, c("part4_nightsummary_sleep_cleaned.csv",
                                    "part4_summary_sleep_cleaned.csv"))
  # if the cleaned reports are not available, use the full reports
  reports_full = file.path(resultsdir, "QC", c("part4_nightsummary_sleep_full.csv",
                                                "part4_summary_sleep_full.csv"))
  missing = which(!file.exists(reports))
  if (length(missing) > 0) reports[missing] = reports_full[missing]
  reports = reports[file.exists(reports)]
  if (length(reports) == 0) return()
  directory = file.path(resultsdir, "variableDictionary")
  if (!dir.exists(directory)) dir.create(directory)
  for (ri in 1:length(reports)) {
    R = data.table::fread(reports[ri], verbose = FALSE, nrows = 2, check.names = FALSE)
    cnames = colnames(R)
    dictionary = data.frame(Variable = cnames, Definition = NA)
    isPersonLevel = grepl("part4_summary", basename(reports[ri]))
    for (coli in 1:length(cnames)) {
      x = cnames[coli]
      def = NULL
      if (isPersonLevel == FALSE) {
        if (x %in% names(nightDictionary)) def = nightDictionary[[x]]
      } else {
        if (x %in% names(personDictionary)) {
          def = personDictionary[[x]]
        } else {
          # aggregated night level variable: <variable>_<AD|WD|WE>[_<sleepparam>][_mn|_sd]
          pattern = "^(.+)_(AD|WD|WE)(_T[0-9.]+A[0-9.]+)?(_mn|_sd)?$"
          if (grepl(pattern, x)) {
            nam = sub(pattern, "\\1", x)
            TW = sub(pattern, "\\2", x)
            sibdef = sub(pattern, "\\3", x)
            stat = sub(pattern, "\\4", x)
            what = NULL
            if (nam %in% names(extraPersonDictionary)) {
              what = extraPersonDictionary[[nam]]
            } else if (nam %in% names(nightDictionary)) {
              what = nightDictionary[[nam]]
            }
            if (!is.null(what)) {
              statname = switch(stat, "_mn" = "mean", "_sd" = "standard deviation", "total")
              if (stat == "") {
                def = paste0(what, ", across ", aggDays[[TW]])
              } else {
                def = paste0(what, " - ", statname, " across ", aggDays[[TW]])
              }
              if (sibdef != "") {
                def = paste0(def, " (sustained inactivity bout definition ",
                             gsub("^_", "", sibdef), ")")
              }
            }
          }
        }
      }
      if (!is.null(def)) dictionary[coli, "Definition"] = def
    }
    fn = gsub("^part4_", "part4_dictionary_", basename(reports[ri]))
    fn = gsub("_sleep_cleaned|_sleep_full", "", fn)
    data.table::fwrite(dictionary, file = file.path(directory, fn),
                       row.names = FALSE, na = "", sep = params_output[["sep_reports"]],
                       dec = params_output[["dec_reports"]])
  }
}
