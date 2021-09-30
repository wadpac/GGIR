CalcSleepRegularityIndex = function(data = c(), epochsize = c(), desiredtz= c()) {
  data$time = iso8601chartime2POSIX(data$time,tz=desiredtz)
  sleepcol = which(colnames(data) %in% c("time", "invalid", "night") == FALSE)[1]
  invalid_epochs = which(data$invalid == 1)
  if (length(invalid_epochs) > 0) { #for Sleep Regularity Index we ignore invalid
    is.na(data[invalid_epochs, sleepcol]) = TRUE
  }
  #aggregate to 30-second epoch if epochsize is less than 30 seconds
  if (epochsize < 30 & (30/epochsize) == round(30/epochsize)) {
    data$time_num = round(as.numeric(data$time) / 30) * 30
    # aggregate by mean and round of value in the next step, to make sure we still
    # have a simple 1 (sleep) or 0 (wakefullness) for the reslting epoch.
    sleepstate = aggregate(data[,sleepcol], by = list(data$time_num), FUN=mean)
    sleepstate = round(sleepstate$x)
    M = 24 * 60 * 2
  } else { # if other epochsize
    sleepstate = data[,sleepcol]
    M = (24*3600) / epochsize
  }
  # truncate to integer number of complete days and put in matrix
  sleepstate = sleepstate[1:(floor(length(sleepstate)/M)*M)]
  sleepstate_byday <- matrix(sleepstate, nrow = M, byrow = FALSE)
  # calculate SRI
  SummedValue = 0
  NValuesSkipped = 0
  N = ncol(sleepstate_byday)
  for (j in 1:M) { # epoch
    for (i in 1:(N-1)) { # day
      # Note I am ignoring invalid epochs (usually non-wear related)
      if (!any(is.na(sleepstate_byday[j,c(i, (i+1))]))) {
        SummedValue = SummedValue + ifelse(test = (sleepstate_byday[j,i] == sleepstate_byday[j,i+1]),
                                           yes = 1, no = 0)
      } else {
        NValuesSkipped = NValuesSkipped + 1
      }
    }
  }
  SleepRegularityIndex = round(-100 + (200/((M*(N-1))-NValuesSkipped)) * SummedValue, digits = 3)
  return(SleepRegularityIndex)
}