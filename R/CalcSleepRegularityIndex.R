CalcSleepRegularityIndex = function(data = c(), epochsize = c(), desiredtz= c(),
                                    SRI1_smoothing_wsize_hrs = NULL,
                                    SRI1_smoothing_frac = NULL) {
  if (inherits(data$time[1], "character") || inherits(data$time[1], "factor")) {
    data$time = iso8601chartime2POSIX(data$time, tz = desiredtz)
  }
  # ignore columns that this function does not need
  data = data[, grep("temperature|selfreported|angle|invalid_|guider|ACC|step_count", colnames(data), invert = TRUE)]
  sleepcol = grep("sleepnap", colnames(data))
  if (length(sleepcol) != 1) {
    # Only run when used in GGIR part 3
    # because there we use sustained inactivity bouts which optionally need
    # to be smoothed
    sleepcol = which(colnames(data) %in% c("time", "invalid", "night") == FALSE)[1]
    # Optionally smooth sleep/wake classification
    if (!is.null(SRI1_smoothing_wsize_hrs) && !is.null(SRI1_smoothing_frac)) {
      data[,sleepcol] = zoo::rollmean(x = data[,sleepcol], k = (SRI1_smoothing_wsize_hrs * 3600) / epochsize, fill = 0)
      data[, sleepcol] = ifelse(data[,sleepcol] >= SRI1_smoothing_frac, yes = 1, no = 0)
    }
    invalid_epochs = which(data$invalid == 1)
  } else {
    # Only run when used in GGIR part 6
    invalid_epochs = which(data$invalidepoch == 1)
  }
  # Ignore invalid epochs and set them to NA
  if (length(invalid_epochs) > 0) { 
    is.na(data[invalid_epochs, sleepcol]) = TRUE
  }
  # Aggregate to 30-second epoch if epochsize is less than 30 seconds to be in line
  # with original paper by Phillips et al. 2017
  if (epochsize < 30 & (30/epochsize) == round(30/epochsize)) {
    data$time_num = floor(as.numeric(data$time) / 30) * 30
    # aggregate by mean and round off value in the next step, to make sure we still
    # have a simple 1 (sleep) or 0 (wakefullness) for the resulting epoch.
    data = aggregate(data[,sleepcol],
                     by = list(data$time_num), FUN = mean)
    colnames(data) = c("time_num",  "sleepstate")
    data$sleepstate = round(data$sleepstate)
    M = 24 * 60 * 2
  } else { # if larger epoch size, keep as it is
    data$time_num = as.numeric(data$time)
    colnames(data)[sleepcol] = "sleepstate"
    M = (24*3600) / epochsize
  }
  epochsize = max(c(epochsize, 30))
  # reinsert timestamps
  data$time = as.POSIXlt(data$time_num, tz = desiredtz, origin = "1970-01-01")
  data$date = as.Date(data$time)
  # add sec(ond) in the day
  convert2SecInDay = function(x) {
    tmp1 = format(x, format = "%H %M %S")
    tmp2 = unlist(strsplit(tmp1," "))
    SecInDay = sum(as.numeric(tmp2) * c(3600, 60, 1))
  }
  data$SecInDay = sapply(data$time, FUN = convert2SecInDay)
  # Expand data frame with empty values such that data.frame
  # has full days
  uniqueDates = unique(data$date)
  Ndays = length(uniqueDates)
  MaxSecInDay = max(data$SecInDay) # maximum number of seconds in a day
  SecSequence = seq(0, MaxSecInDay, by = epochsize) # Typical sequences of seconds in the day
  SecInDayTmp = rep(SecSequence, times = Ndays)
  DatesTmp = rep(uniqueDates, each = length(SecSequence))
  temp_df = data.frame(SecInDay = SecInDayTmp, date = DatesTmp, stringsAsFactors = FALSE)
  data = merge(x = data, y = temp_df, by.all = c("SecInDay", "date"), all = TRUE)
  
  # Calculations of Sleep Regularity Index per day pair
  NR = Ndays - 1
  if (NR > 1) {
    SleepRegularityIndex = data.frame(day = 1:NR, SleepRegularityIndex = numeric(NR),
                                      weekday = character(NR), frac_valid = numeric(NR), 
                                      date = character(NR), stringsAsFactors = FALSE)
    for (i in 1:NR) { # Loop over all days
      thisday = which(data$date == uniqueDates[i] & data$SecInDay < (24*3600))
      nextday = which(data$date == uniqueDates[i + 1] & data$SecInDay < (24*3600))
      ne25 = ((3600*25)/epochsize)
      ne24 = ((3600*24)/epochsize)
      ne23 = ((3600*23)/epochsize)
      if (length(thisday) == ne25) {
        thisday = thisday[1:ne24]
      }
      if (length(nextday) == ne25) {
        nextday = nextday[1:ne24]
      }
      if (length(thisday) == ne23) {
        nextday = nextday[1:ne23]
      }
      if (length(nextday) == ne23) {
        thisday = thisday[1:ne23]
      }
      EqualState = c(data$sleepstate[thisday] == data$sleepstate[nextday])
      testNA = c(is.na(data$sleepstate[thisday]) | is.na(data$sleepstate[nextday]))
      SummedValue = sum(ifelse(test = EqualState[which(testNA == FALSE)] == TRUE, yes = 1, no = 0))
      NValuesSkipped = length(which(testNA == TRUE))
      SleepRegularityIndex$frac_valid[i] = round((M - NValuesSkipped) / M, digits = 4)
      if (M != NValuesSkipped) {
        SleepRegularityIndex$SleepRegularityIndex[i] = round(-100 + (200/(M - NValuesSkipped)) * SummedValue, digits = 3)
      }
      SleepRegularityIndex$weekday[i] = weekdays(abbreviate = FALSE, x =  uniqueDates[i])
      SleepRegularityIndex$date[i] = format(as.Date(uniqueDates[i], 
                                                          origin = "1970-01-01"), format = "%d/%m/%Y")
    }
  } else {
    SleepRegularityIndex = NA
  }
  # SleepRegularityIndex is now a data.frame with SleepRegularityIndex per calendar date
  return(SleepRegularityIndex)
}