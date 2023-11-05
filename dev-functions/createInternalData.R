
# baseDictionary ----------------------------------------------------------

# baseDictionary contains variable names that are exported always with the same
# name and, preferably, they can be used in reports from more than one GGIR part
# (e.g., ID definition can be reused in reports from part 2, 4, and 5)
# This would avoid unnecessary duplication of code 

baseDictionary = list(
  # general -----
  ID = "File/participant identifier",
  filename = "File name",
  
  # day/dates/windows -----
  weekday = "Day of the week (full name)",
  window_number = "Window number in the recording",
  window = "Window start and end names in segment analysis",
  night_number = "Night number in the recording",
  start_end_window = "Start and end time for the window (hh:mm:ss-hh:mm:ss)",
  daytype = "WD = weekday; WE = weekend day",
  calendar_date = "Calendar date",
  startday = "Calendar date for the first day in the recording",
  Nvaliddays = "Number of valid days based on the cleaning parameters for part 2, 4, and 5",
  Nvaliddays_WD = "Number of valid weekdays based on the cleaning parameters for part 2, 4, and 5",
  Nvaliddays_WE = "Number of valid weekend days based on the cleaning parameters for part 2, 4, and 5",
  Nvaliddays_AL10F_WD = "Number of valid weekdays with at least 10 fragments (fragmentation analysis)",
  Nvaliddays_AL10F_WE = "Number of valid weekend days with at least 10 fragments (fragmentation analysis)",
  Nvalidsegments = "Number of valid segments in the recording based on segmentWEARcrit.part5 and segmentDAYSPTcrit.part5",
  Nvalidsegments_WD = "Number of valid segments in the weekdays based on segmentWEARcrit.part5 and segmentDAYSPTcrit.part5 in the recording",
  Nvalidsegments_WE = "Number of valid segments in the weekend days based on segmentWEARcrit.part5 and segmentDAYSPTcrit.part5 in the recording",
  
  # sleep info in part 4 and part 5 -------
  daysleeper = "Night classified as daysleeper (i.e., wake-up time after noon)",
  cleaningcode = "Cleaning code for the sleep period time classification (0=no problem; 1=no sleeplog; 2=insufficient valid data; 3=no acc data available; 4=no nights; 5=guider-defined SPT; 6=SPT not found)",
  guider = "Guider used for the sleep period time identification",
  sleeplog_used = "Whether sleep log information was used for the identification of the sleep period time (TRUE/FALSE)",
  acc_available = "Whether accelerometer data was available for the identification of the sleep period time (TRUE/FALSE)",
  N_atleast5minwakenight = "Number of blocks awake after sleep onset with a duration of at least 5 minutes",
  Ndaysleeper = "Number of nights classified as daysleeper (i.e., wake-up time after noon)",
  Ncleaningcodezero = "Number of nights with cleaning code for the sleep period time classification = 0 (i.e., no problem)",
  Ncleaningcode1 = "Number of nights with cleaning code for the sleep period time classification = 1 (i.e., no sleeplog)",
  Ncleaningcode2 = "Number of nights with cleaning code for the sleep period time classification = 2 (i.e., insufficient valid data)",
  Ncleaningcode3 = "Number of nights with cleaning code for the sleep period time classification = 3 (i.e., no acc data available)",
  Ncleaningcode4 = "Number of nights with cleaning code for the sleep period time classification = 4 (i.e., no nights)",
  Ncleaningcode5 = "Number of nights with cleaning code for the sleep period time classification = 5 (i.e., guider-defined SPT)",
  Ncleaningcode6 = "Number of nights with cleaning code for the sleep period time classification = 6 (i.e., SPT not found)",
  Nsleeplog_used = "Number of nights in which the sleep log was used as guider for the sleep period time identification",
  Nacc_available = "Number of nights in which accelerometer data was available for the identification of the sleep period time",
  
  # Processing information -----------
  tail_expansion_minutes = "Time expanded at the end of the recording with expand_tail_max_hours to trigger the last sleep onset identification (min)",
  boutcriter.in = "Fraction of the bout that needs to be below the inactivity threshold",
  boutcriter.lig = "Fraction of the bout that needs to meet the light physical activity threshold",
  boutcriter.mvpa = "Fraction of the bout that needs to be above the moderate physical activity threshold",
  boutdur.in = "Duration/s of inactivity bouts (min)",
  boutdur.lig = "Duration/s of light physical activity bouts (min)",
  boutdur.mvpa = "Duration/s of moderate-to-vigorous physical activity bouts (min)")

# Future work - code to generate internal data below ----------------------






# Store internal data -----------------------------------------------------

# make sure to include the names of the objects to be stored in sysdata.rda
usethis::use_data(baseDictionary, 
                  internal = TRUE, overwrite = TRUE)

