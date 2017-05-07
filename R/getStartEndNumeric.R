getStartEndNumeric <- function(d, hhr, startHour = 4) {
  # gets page numbers for the start and end of day specified in arg d
  # Args: d (character) date string using format of 
  #       hhr: GENEActiv::header.info(f) output
  #       startHour 
  hcal <- attr(hhr, "calibration")
  freq = as.numeric(hhr$Value[[2]])
  tmzone <- ifelse(hcal$tz == 1, "Europe/London", "UTC")
  nobs <- 300
  timespan <- nobs/freq
  timestamps <- seq(hcal$t1, by = timespan, length = hcal$npages)
  d1 <- as.POSIXct(paste0(d, " 03:55:00"), format = "%d/%m/%Y %H:%M:%S", tz = "Europe/London")
  dend <- d1 + (60*60*24) + (60*5) # (24 hours + five minutes)
  start <- findInterval(d1 - 0.5, timestamps, all.inside = T)
  end <- findInterval(dend, timestamps, all.inside = T)
  # Value: data frame - one row, start page and end page
  data.frame(start = start, end = end, stringsAsFactors = FALSE)
}