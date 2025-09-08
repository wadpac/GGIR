g.part5.lux_persegment = function(ts, sse, LUX_day_segments, epochSize, desiredtz = "") {
  # only look at waking hours of this day
  sse = sse[which(ts$diur[sse] == 0)]
  # create equal length vector representing per epoch the first hour of the corresponding day segment
  if ("POSIXt" %in% class(ts$time[sse[1]])) {
    posixTime = ts$time[sse]
  } else {
    posixTime = iso8601chartime2POSIX(ts$time[sse], tz = desiredtz)
  }
  first_hour_seg = as.numeric(format(posixTime,"%H"))
  for (ldi in 1:(length(LUX_day_segments) - 1)) {
    # round all hours to bottom of its class
    tmpl = which(first_hour_seg >= LUX_day_segments[ldi] &
                   first_hour_seg <  LUX_day_segments[ldi + 1])
    if (length(tmpl) > 0) {
      first_hour_seg[tmpl] = LUX_day_segments[ldi]
    }
  }
  Nepochperday = 24 * (3600 / epochSize)
  if (length(first_hour_seg) > Nepochperday) {
    first_hour_seg = first_hour_seg[1:Nepochperday]
  }
  if (length(sse) > Nepochperday) {
    sse = sse[1:Nepochperday]
  }
  # expanding first time segment
  expected_N_seg1 = (rep(LUX_day_segments,2)[which(LUX_day_segments == first_hour_seg[1])+1] - LUX_day_segments[which(LUX_day_segments == first_hour_seg[1])]) * 60 * (60/epochSize)
  actual_N_seg1 = length(which(first_hour_seg[1:round(Nepochperday*0.66)] == first_hour_seg[1]))
  missingN_seg1 = expected_N_seg1 - actual_N_seg1
  if (length(sse) > 0) {
    if (sse[1] - missingN_seg1 < 0) missingN_seg1 = missingN_seg1 + (sse[1] - missingN_seg1 - 1)
    if (missingN_seg1 > 0) {
      extension = (sse[1]-missingN_seg1):(sse[1]-1)
      ts$lightpeak[extension] = 0 # ensure that light during SPT is treated as zeros
      sse = c(extension,sse)
      first_hour_seg = c(rep(first_hour_seg[1], missingN_seg1), first_hour_seg)
    }
  }
  # removing again the data after 24 hours to avoid overlap  Nepochperday = 24 * (3600 / ws3)
  if (length(first_hour_seg) > Nepochperday) {
    first_hour_seg = first_hour_seg[1:Nepochperday]
  }
  if (length(sse) > Nepochperday) {
    sse = sse[1:Nepochperday]
  }
  # if less than 24 hours makes sure that last incomplete segment is removed
  if (length(sse) < Nepochperday) {
    last_seg = first_hour_seg[length(first_hour_seg)]
    N_last_seg = length(which(first_hour_seg[round(length(first_hour_seg)*0.5):length(first_hour_seg)] == last_seg))
    sse = sse[1:(length(sse) - N_last_seg)]
    first_hour_seg = first_hour_seg[1:(length(first_hour_seg) - N_last_seg)]
  }
  # Aggregation per segment of the window:
  # Function to help standardise metric per seg output across days and recordings
  standardise_luxperseg = function(x, LUX_day_segments, LUXmetricname = "") {
    colnames(x) = c("seg", "light")
    if (24 %in% LUX_day_segments) LUX_day_segments = LUX_day_segments[which(LUX_day_segments != 24)] # remove end of day
    Nsegs = length(LUX_day_segments)
    x = base::merge(x, data.frame(seg= LUX_day_segments, 
                                  light = rep(NA, Nsegs)),
                    by =c("seg"), all.y=TRUE)
    x = x[,c("seg","light.x")]
    colnames(x) = c("seg", "light")
    LUX_day_segments = c(LUX_day_segments, 24) # end of day back in
    end_of_segment = LUX_day_segments[which(x$seg %in% LUX_day_segments) + 1]
    invisible(list(values = x$light,
                   names = paste0("LUX_",LUXmetricname,"_",x$seg,"_",end_of_segment, "hr_day")))
  }
  # Fraction above 1000 LUX
  fraction_above_thousand = function(x, epochSize) {
    timeabove1000 = length(which(x > 1000)) / (60/epochSize)
    return(timeabove1000)
  }
  LUXabove1000 = aggregate(ts$lightpeak[sse], by =  list(first_hour_seg), fraction_above_thousand, epochSize)
  # Time awake
  countvalue = function(x, epochSize, value) {
    return(length(which(x == value)) / (60/epochSize))
  }
  LUXwaketime = aggregate(ts$diur[sse], by = list(first_hour_seg), FUN = countvalue, value = 0, epochSize = epochSize)
  # Mean light
  mymean = function(x) {
    return(round(mean(x, na.rm = T), digits = 1))
  }
  LUXmean = aggregate(ts$lightpeak[sse], by =  list(first_hour_seg), mymean)
  # Time light imputed
  LUXlightimputed = aggregate(ts$lightpeak_imputationcode[sse], by = list(first_hour_seg), FUN = countvalue, value = 1, epochSize = epochSize)
  LUXlightignored = aggregate(ts$lightpeak_imputationcode[sse], by = list(first_hour_seg), FUN = countvalue, value = 2, epochSize = epochSize)
  
  standardLPS1 = standardise_luxperseg(x = LUXabove1000, LUX_day_segments, LUXmetricname = "above1000")
  standardLPS2 = standardise_luxperseg(x = LUXwaketime, LUX_day_segments, LUXmetricname = "timeawake")
  standardLPS3 = standardise_luxperseg(x = LUXmean, LUX_day_segments, LUXmetricname = "mean")
  standardLPS4 = standardise_luxperseg(x = LUXlightimputed, LUX_day_segments, LUXmetricname = "imputed")
  standardLPS5 = standardise_luxperseg(x = LUXlightignored, LUX_day_segments, LUXmetricname = "ignored")
  
  values = c(standardLPS1$values, standardLPS2$values,
             standardLPS3$values, standardLPS4$values,
             standardLPS5$values)
  names = c(standardLPS1$names, standardLPS2$names,
            standardLPS3$names, standardLPS4$names,
            standardLPS5$names)
  invisible(list(values=values, names=names))
}
