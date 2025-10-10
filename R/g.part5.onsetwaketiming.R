g.part5.onsetwaketiming = function(qqq, ts, min, sec, hour, timewindowi) {
  onset = wake = 0
  skiponset = TRUE; skipwake = TRUE
  # Onset index
  if (timewindowi == "OO") {
    # For OO onset is by definition the start and end of the window
    onseti = qqq[1] + 1
  } else {
    # For WW and MM onset needs to be search in the window
    onseti = c(qqq[1]:qqq[2])[which(diff(ts$diur[qqq[1]:(qqq[2] - 1)]) == 1)] + 1
    if (length(onseti) > 1) {
      onseti = onseti[length(onseti)] # in the case if MM use last onset
    }
  }
  # Wake index
  if (timewindowi == "WW") {
    # For WW wake is by definition the start and end of the window
    wakei = qqq[2] + 1
    if (wakei > length(hour)) wakei = length(hour) # in the case if MM use first wake-up time
  } else {
    # For OO and MM wake needs to be search in the window
    wakei = c(qqq[1]:qqq[2])[which(diff(ts$diur[qqq[1]:(qqq[2] - 1)]) == -1) + 1]
    if (length(wakei) > 1) wakei = wakei[1] # in the case if MM use first wake-up time
  }
  # Onset time
  if (length(onseti) == 1) { # in MM window it is possible to not have an onset
    if (is.na(onseti) == FALSE) {
      onset = hour[onseti] + (min[onseti]/60) + (sec[onseti]/3600)
      skiponset = FALSE
    }
  }
  # Wake time
  if (length(wakei) == 1) { # in MM window it is possible to not have a wake
    if (is.na(wakei) == FALSE) {
      wake = hour[wakei] + (min[wakei]/60) + (sec[wakei]/3600)
      skipwake = FALSE
    }
  }
  if (wake >= 12 & wake <= 18) { # daysleeper and onset in the morning or afternoon
    if (onset <= 18 & skiponset == FALSE) onset = onset + 24
    if (wake <= 18 & skipwake == FALSE) {
      wake = wake + 24
    }
  } else if (wake <= 12) { # no daysleeper, but onset before noon
    if (wake <= 12 & skipwake == FALSE) wake = wake + 24
    if (onset <= 12 & skiponset == FALSE) onset = onset + 24
  }
  if (wake >= 12 & onset <= 12 & skiponset == FALSE) onset = onset + 24
  if (wake > 36 & onset > 36) {
    # both on the next afternoon is not possible,
    # so this was an overcorrection and reverse this:
    onset = onset - 24
    wake = wake - 24
  }
  return(invisible(
    list(
      wake = wake,
      onset = onset,
      wakei = wakei,
      onseti = onseti,
      skiponset = skiponset,
      skipwake = skipwake
    )
  ))
}