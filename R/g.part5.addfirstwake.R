g.part5.addfirstwake = function(ts, summarysleep, nightsi, sleeplog, ID, 
                               Nepochsinhour, SPTE_end) {
  # Note related to if first and last night were ignored in part 4:
  # - diur lacks the first and last night at this point in the code.
  # - nightsi has all the midnights, so it is possible to check here
  # whether a wakeup time is missing on the first full day.
  # - if it is missing, then we will impute it in order for part5 to
  # the wake-to-wake analys on the second recording day.
  # Previously we accounted only for this later on in the code, which
  # did not benefit the exported timeseries.
  clock2numtime = function(x) { # function used for converting sleeplog times to hour times
    x2 = as.numeric(unlist(strsplit(x,":"))) / c(1,60,3600)
    return(sum(x2))
  }
  firstwake = which(diff(ts$diur) == -1)[1]
  firstonset = which(diff(ts$diur) == 1)[1]
  Nts = nrow(ts)
  epochSize = round(3600 / Nepochsinhour)
  if (is.na(SPTE_end[1]) == TRUE) {
    SPTE_end = SPTE_end[which(is.na(SPTE_end) == FALSE)]
  }
  # test whether wake for second day is missing
  # if the full sleep period happens before midnights
  if (length(nightsi) < 2) {
    return(ts)
  }
  guider = "unknown" # initialise guider name as unknown
  if (!is.na(firstwake) && firstwake > nightsi[2] ||
      (summarysleep$sleeponset[1] < 18 &&
       summarysleep$wakeup[1] < 18 &&
       firstwake < nightsi[2])) {
    wake_night1_index = c()
    if (length(sleeplog) > 0) {
      # use sleeplog for waking up after first night
      if (is.na(match(ID, sleeplog$ID))) ID = gsub('\\D', '', ID) # ignore characters if match cannot be made
      wake_night1 = sleeplog[which(sleeplog$ID == ID & sleeplog$night == 1),
                                       grep(pattern = "bedend|wake", x = colnames(sleeplog))]
      onset_night1 = sleeplog[which(sleeplog$ID == ID & sleeplog$night == 1),
                                         grep(pattern = "bedstart|sleeponset", x = colnames(sleeplog))]
      if (length(wake_night1) != 0 & length(onset_night1) != 0) {
        if (wake_night1 != "" & onset_night1 != "") {
          # express hour relative to midnight within the noon-noon:
          wake_night1_hour = clock2numtime(wake_night1)
          onset_night1_hour = clock2numtime(onset_night1)
          # If wake is in the afternoon or evening and onset hour is at noon
          # then this cannot be a daysleeper and wakie time need to be correct
          if (wake_night1_hour > 12 & (onset_night1_hour >= 12 & onset_night1_hour < 18)) {
            wake_night1_hour = wake_night1_hour - 24 # express hour relative to midnight
          }
          # onset_night1_hour = clock2numtime(onset_night1)
          wake_night1_index = nightsi[1] + round(wake_night1_hour * Nepochsinhour)
          # express hour relative to midnight within the noon-noon:
          if (wake_night1_index > Nts) wake_night1_index = Nts
          if (wake_night1_hour > 18) wake_night1_hour = wake_night1_hour - 24 # express hour relative to midnight
          if (wake_night1_index < 1) wake_night1_index = 1
          wake_night1_index = nightsi[1] + round(wake_night1_hour * Nepochsinhour)
          if (wake_night1_index > Nts) wake_night1_index = Nts
          if (wake_night1_index < 1) wake_night1_index = 1
          guider = "sleeplog"
        } else { # use SPTE algorithm as plan B
          wake_night1_index = nightsi[1] + round((SPTE_end[1] - 24) * Nepochsinhour)
          guider = "part3_estimate"
        }
      } else { # use SPTE algorithm as plan B
        wake_night1_index = nightsi[1] + round((SPTE_end[1] - 24) * Nepochsinhour)
        guider = "part3_estimate"
      }
    } else if (length(SPTE_end) > 0 & length(sleeplog) == 0) {
      # use SPTE algortihm for waking up after first night
      # if there was no sleep log
      if (is.na(SPTE_end[1]) == FALSE) {
        if (SPTE_end[1] != 0) {
          wake_night1_index = nightsi[1] + round((SPTE_end[1] - 24) * Nepochsinhour) 
          guider = "part3_estimate"
        }
      }
    }
    if (length(wake_night1_index) == 0) {
      # use waking up from next day and subtract 24 hours,
      # the final option if neither of the above routes works
      wake_night1_index = (firstwake - (24 * ((60 / epochSize) * 60))) + 1
    }
    if (is.na(wake_night1_index)) wake_night1_index = 0
    if (wake_night1_index < firstwake & wake_night1_index > 1 &
        (wake_night1_index - 1) > nightsi[1]) {
      newWakeIndex = c()
      firstSIBs = which(ts$sibdetection[1:(wake_night1_index - 1)] == 1)
      if (length(firstSIBs) > 0) newWakeIndex = max(firstSIBs)
      if (length(newWakeIndex) == 0) {
        newWakeIndex = wake_night1_index - 1
      }
      ts$diur[1:newWakeIndex] = 1
      ts$guider[1:newWakeIndex] = guider
    } else {
      # Person slept only during the afternoon on day 2
      # And there is no sleep data available for the first night
      # We will now add 5 minutes of dummy waking time before it this now, 
      # and label it as non-wear.
      # We do this to make sure that the day numbering
      # and merging of the sleep variables is still consistent with
      # the other recording.
      if (!is.na(firstonset)) {
        dummywake = max(firstonset - round(Nepochsinhour/12), nightsi[1] + round(Nepochsinhour * 6))
        ts$diur[1:dummywake] = 1 
        ts$nonwear[1:firstonset] = 1
        ts$guider[1:dummywake] = guider
      }
    }
  }
  return(ts)
}
