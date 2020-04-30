g.part5.onsetwaketiming = function(qqq,ts, min, sec, hour, timewindowi, skiponset, skipwake) {
  onset = wake = 0
  # Onset index
  if (timewindowi == "WW") {
    onseti = c(qqq[1]:qqq[2])[which(diff(ts$diur[qqq[1]:(qqq[2]-1)]) == 1)+1]
    if (length(onseti) > 1) {
      onseti = onseti[length(onseti)] # in the case if MM use last onset
    }
  } else {
    onseti = c(qqq[1]:qqq[2])[which(diff(ts$diur[qqq[1]:(qqq[2]-1)]) == 1)+1]
    if (length(onseti) > 1) {
      onseti = onseti[length(onseti)] # in the case if MM use last onset
    }
  }
  # Wake index
  if (timewindowi == "WW") {
    wakei = qqq[2]+1
  } else {
    wakei = c(qqq[1]:qqq[2])[which(diff(ts$diur[qqq[1]:(qqq[2]-1)]) == -1)+1]
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
  return(invisible(list(wake=wake, onset=onset, wakei=wakei, onseti=onseti, skiponset=skiponset, skipwake=skipwake)))
}