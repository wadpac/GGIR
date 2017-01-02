getStartEnd <- function(d, startHour, inputFormat = "%d/%m/%Y", outputFormat = "%d/%m/%Y %H:%M:%S",
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

getStartEndNumeric <- function(d, hhr, startHour = 4, inputFormat = "%d/%m/%Y") {
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

getFirstTimestamp <- function(f, p1) {
    hhr <- GENEAread::header.info(f)
    hcal <- attr(hhr, "calibration")
    t1 <- hcal$t1
    rate <- 1 / hcal$freq
    nrows <- (p1 - t1) / rate
    newRate <- 1 / as.numeric(hhr$Value[[2]])
    as.POSIXct(t1 + (nrows * newRate), origin = "1970-1-1")
}

