g.IVIS = function(Xi, epochSize = 60, threshold = NULL) {
  if (!is.null(threshold)) {
    if (max(Xi, na.rm = TRUE) < 8) Xi = Xi * 1000
    Xi = ifelse(Xi < threshold, 0, 1)  
  }
  if (3600 > epochSize) {
    # Always Xi aggregate to 1 hour resolution
    df = data.frame(Xi = Xi, time = numeric(length(Xi)))
    time = seq(0, length(Xi) * epochSize, by = epochSize)
    df$time = time[1:nrow(df)]
    df = aggregate(x = df, by = list(floor(df$time / 3600) * 3600), 
                   FUN = mean, na.action = na.pass)
    Xi = df$Xi
  }
  # Hourly time series
  N = length(Xi)
  hour = (1:ceiling(N)) - 1
  ts = data.frame(Xi = Xi, hour = hour, stringsAsFactors = TRUE)
  IS = IV = NA
  if (nrow(ts) > 1) {
    ts$day = floor(ts$hour/24) + 1
    ts$hour = ts$hour - (floor(ts$hour / 24) * 24) # 24 hour in a day
    if (nrow(ts) > 1) {
      # real average day
      aveDay = aggregate(. ~ hour, data = ts, mean, na.action = na.omit)
      Xh = aveDay$Xi
      Xm = suppressWarnings(mean(Xh,na.rm = TRUE)) # Average acceleration per day
      N = length(Xi)
      # difference between successive values
      deltaXi = aggregate(ts$Xi, by = list(ts$day), FUN = function(x) sum(diff(c(x))^2, na.rm = TRUE))
      # Keep track of how many differences between successive values are valid
      deltaXi_valid = aggregate(ts$Xi, by = list(ts$day), FUN = function(x) length(which(is.na(diff(c(x))) == FALSE)))
      # Count valid values
      Nvalid = sum(deltaXi_valid$x + 1, na.rm = TRUE) # + 1 because n still refers to number of epochs
      IS = (sum((Xh - Xm)^2, na.rm = TRUE) * Nvalid) / (24 * sum((Xi - Xm)^2, na.rm = TRUE)) # IS: lower is less synchronized with the 24 hour zeitgeber
      IV = (sum(deltaXi$x, na.rm = TRUE) * Nvalid) / ((Nvalid - 1) * sum((Xi - Xm)^2, na.rm = TRUE)) #IV: higher is more variability within days (fragmentation)
    }
  }
  invisible(list(InterdailyStability = IS, IntradailyVariability = IV))
}
