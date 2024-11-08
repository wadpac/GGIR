g.IVIS = function(Xi, epochSize = 60, threshold = NULL) {
  if (!is.null(threshold)) {
    Xi = ifelse(Xi > threshold, 0, 1)  
  }
  if (epochSize < 3600) {
    # Always aggregate Xi to 1 hour resolution if it is not provided at 1 hour resolution
    df = data.frame(Xi = Xi, time = numeric(length(Xi)))
    time = seq(0, length(Xi) * epochSize, by = epochSize)
    df$time = time[1:nrow(df)]
    df = aggregate(x = df, by = list(floor(df$time / 3600) * 3600), 
                   FUN = mean, na.action = na.pass, na.rm = TRUE)
    Xi = df$Xi
  }
  # ts hourly time series, including NA values
  hour = (1:ceiling(length(Xi))) - 1
  ts = data.frame(Xi = Xi, hour = hour, stringsAsFactors = TRUE)
  IS = IV = phi = NA
  if (nrow(ts) > 1) {
    ts$day = floor(ts$hour/24) + 1
    ts$hour = ts$hour - (floor(ts$hour / 24) * 24) # 24 hour in a day
    if (nrow(ts) > 1) {
      # Average day
      aveDay = aggregate(. ~ hour, data = ts, mean, na.action = na.omit)
      Xh = aveDay$Xi
      Xm = suppressWarnings(mean(Xh,na.rm = TRUE)) # Average acceleration per day
      deltaXi = diff(Xi)^2
      N = length(Xi[!is.na(Xi)])
      
      # phi
      model = arima(Xi[!is.na(Xi)], order = c(1, 0, 0))
      phi = model$coef[[1]]

      # IS: lower is less synchronized with the 24 hour zeitgeber
      ISnum = sum((Xh - Xm)^2, na.rm = TRUE) * N
      ISdenom = 24 * sum((Xi - Xm)^2, na.rm = TRUE)
      IS = ISnum / ISdenom
      
      #IV: higher is more variability within days (fragmentation)
      IVnum = sum(deltaXi, na.rm = TRUE) * N
      IVdenom = (N - 1) * sum((Xi - Xm)^2, na.rm = TRUE)
      IV = IVnum / IVdenom 
    }
  }
  invisible(list(InterdailyStability = IS, IntradailyVariability = IV, phi = phi))
}
