aggregate_to_epoch = function(ts, to, lightpeak_available, params_general) {
  
  # Aggregate time
  ts$time_num = floor(as.numeric(iso8601chartime2POSIX(ts$time,tz = params_general[["desiredtz"]])) / to) * to
  
  # Columns to aggregate
  angleColName = lightColName = NULL
  if ("angle" %in% names(ts)) angleColName = "angle"
  if (lightpeak_available) lightColName = c("lightpeak", "lightpeak_imputationcode")
  cols2agg = c("ACC","sibdetection", "diur", "nonwear", angleColName, lightColName)
  
  # aggregate
  ts = aggregate(ts[, cols2agg], by = list(ts$time_num), FUN = function(x) mean(x))
  ts$sibdetection = round(ts$sibdetection)
  ts$diur = round(ts$diur)
  ts$nonwear = round(ts$nonwear)
  names(ts)[1] = "time"
  # # convert back to iso8601 format
  ts$time = as.POSIXct(ts$time, origin = "1970-1-1", tz = params_general[["desiredtz"]])
  return(ts)
}