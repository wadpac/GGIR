getStartEnd <- function(d, startHour, outputFormat = "%d/%m/%Y %H:%M:%S",
                        tz = "Europe/London") {
  # Args: d - date (without time) in correct format of inputFormat 
  #       startHour - Hour that analysis starts at
  startTimeString <- paste0(d, " ", startHour, ":00:00")
  # create a POSIXlt object on date d and at hour startHour
  startTimeObj <- as.POSIXlt(startTimeString, format = "%d/%m/%Y %H:%M:%S", tz = tz)
  # calculate number of seconds for five minutes and one day.
  fiveMinutes <- 60 * 5
  oneDay <- 60*60*24
  # Return value: data.frame with two columns: a start time five minutes before startHour on day d
  #               and an endtime 24 hours after startHour 
  # For example: d = "01/09/2016", startHour = 4 returns start: 01/09/2016 03:55:00, end: 02/09/2016 04:00:00 
  data.frame(start = as.character(startTimeObj - fiveMinutes, format = outputFormat),
             end = as.character(startTimeObj + oneDay, format = outputFormat), stringsAsFactors = FALSE)
}