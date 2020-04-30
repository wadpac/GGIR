is_this_a_dst_night = function(calendar_date=c(),tz="Europe/London") {
  # calendardata in dd/mm/yyyy format
  # this function investigates whether it is a dst night
  splitdate = unlist(strsplit(calendar_date,"/"))
  t0 = as.POSIXlt(paste0(splitdate[3],"-",splitdate[2],"-",splitdate[1]," 21:00:00"),tz=tz)
  t1 = as.POSIXlt(as.numeric(t0) + 3600*9,origin="1970-01-01",tz=tz)
  hoursinbetween = as.numeric(format(seq(t0,t1,by=3600),"%H"))
  t1 = as.numeric(format(t1,"%H")) + 24
  t0 = as.numeric(format(t0,"%H"))
  dsthour = c()
  if (t1 - t0 < 9) { 
    # if time has moved backward (clock gives the impression that 8 hours 
    # elapsed, while physically 9 hours elased
    dst_night_or_not = -1
    dsthour = hoursinbetween[duplicated(hoursinbetween)] #the double hour
  } else if (t1 - t0 > 9) {
    # if time has moved forwared (clock gives the impression that 10 hours
    # elapsed, while physically 9 hours elapsed
    dst_night_or_not = 1
    expectedhours = c(21:23,0:7)
    dsthour = expectedhours[which(expectedhours %in% hoursinbetween == FALSE)[1]] #themissinghour
    
  } else if (t1 - t0 == 9) {
    dst_night_or_not = 0
  }
  return(invisible(list(
    dst_night_or_not = dst_night_or_not,
    dsthour = dsthour)))
}