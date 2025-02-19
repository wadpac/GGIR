apply_cosinor_IS_IV_Analyses = function(ts, qcheck, midnightsi, epochsizes, threshold = NULL) {
  # qcheck - vector of length ts to indicate invalid values
  ws2 = epochsizes[2]
  ws3 = epochsizes[1]
  # Re-derive Xi but this time include entire time series
  # Here, we ignore duplicated values (when clock moves backward due to DST)
  handleDST = !duplicated(ts$time)
  qcheck = qcheck[handleDST]
  Xi = ts[handleDST, grep(pattern = "time", x = colnames(ts), invert = TRUE)]
  Nlong_epochs_day =  (1440 * 60) / ws2 # this is 96 by default
  dstgap = which(diff(midnightsi) != Nlong_epochs_day)
  if (length(dstgap) > 0) {
    # Time moved forward due to DST
    gaplocation = ((midnightsi[dstgap[1]] * ws2) / ws3) + (2 * (3600/ws3))
    # Insert NA values
    Xi = c(Xi[1:gaplocation], rep(NA, 3600/ws3), Xi[(gaplocation + 1):length(Xi)])
    qcheck = c(qcheck[1:gaplocation], rep(NA, 3600/ws3), qcheck[(gaplocation + 1):length(qcheck)])
  }
  
  # Xi = log((Xi * 1000) + 1)  # log transformed to be more robust against peaks in the data
  # set non-wear to missing values, because for Cosinor fit
  # it seems more logical to only fit with real data
  # this comes at the price of not being able to extract F_pseudo
  firstvalid = 1
  if (length(which(qcheck == 1)) > 0) {
    is.na(Xi[which(qcheck == 1)]) = TRUE
    # ignore invalid start of recording (if applicable)
    # such that 24 hour blocks start from first valid value
    firstvalid = which(qcheck == 0)[1]
    if (is.na(firstvalid) == FALSE) {
      if (firstvalid != 1) {
        Xi = Xi[firstvalid:length(Xi)]
      }
    }
  }
  if (length(which(is.na(Xi) == FALSE)) > (1440 * (60/ws3))) { # Only attempt cosinor analyses if there is more than 24 hours of data
    midnightsi_ws3 = (midnightsi - 1) * (ws2 / ws3)
    timeOffsetHours = (midnightsi_ws3[which(midnightsi_ws3 >= firstvalid - 1)[1]] - (firstvalid - 1)) / (3600 / ws3)
    if (ws3 < 60) {
      # If epochsize < 1 minute then aggregate to 1 minute by taking average value
      # but keep NA values
      XTtime = rep(1:length(Xi), each = 60 / ws3)
      XT = data.frame(Xi = Xi, time = XTtime[1:length(Xi)])
      custommean = function(x) {
        y = NA
        if (length(x) > 0) {
          if (length(which(is.na(x) == FALSE) ) > 0) {
            y = mean(x, na.rm = TRUE)
          }
        }
        return(y)
      }
      XT = aggregate(x = XT, by = list(XT$time), FUN = custommean)
      if (length(which(is.nan(XT$Xi) == TRUE)) > 0) {
        is.na(XT$Xi[which(is.nan(XT$Xi) == TRUE)]) = TRUE
      }
      Xi = XT$Xi
      epochsize = 60
    } else {
      epochsize = ws3
    }
    cosinor_coef = cosinor_IS_IV_Analyses(Xi = Xi, epochsize = epochsize, 
                                   timeOffsetHours = timeOffsetHours, threshold = threshold)
    cosinor_coef$timeOffsetHours = timeOffsetHours
  } else {
    cosinor_coef = c()
  }
  return(cosinor_coef)
}