g.getstarttime = function(datafile, P, mon, dformat, desiredtz, configtz = NULL) {
  starttime = NULL
  if (is.null(configtz)) {
    configtz = desiredtz
  }
  if ("time" %in% colnames(P$data)) {
    starttime = as.POSIXlt(P$data$time[1], tz = desiredtz, origin = "1970-01-01")
  } else if (mon == MONITOR$MOVISENS) {
      starttime = unisensR::readUnisensStartTime(dirname(datafile))

      # movisens timestamp is stored in unisens.xml file as a string "%Y-%m-%dT%H:%M:%S",
      # without a timezone.
      # unisensR::readUnisensStartTime() converts this string into a POSIXct object,
      # but since no timezone information was stored, this conversion happens using
      # the system timezone.
      # If configtz is different from the system timezone (""), we need to force the
      # correct timezone (force, while keeping the same hh:mm:ss time, not just convert to congigtz)
      # (Converting to POSIXlt then back to POSIXct with the correct timezone
      #  seems to work fine as an equivalent of lubridate::force_tz()).
      if (configtz != "") {
        starttime = as.POSIXlt(starttime)
        starttime = as.POSIXct(starttime, tz=configtz)
      }
      starttime = as.POSIXlt(starttime, tz = desiredtz)

  } else if (dformat == FORMAT$CSV && (mon == MONITOR$ACTIGRAPH || mon == MONITOR$VERISENSE)) {
    starttime = startdate = NULL
    header_rows = 8
    tmph = read.csv(datafile, nrow = header_rows, skip = 1)

    tmphi = 1
    while (tmphi < header_rows) {
      tmp = unlist(strsplit(format(tmph[tmphi,1]),"Start Time"))
      if (length(tmp) > 1) {
        starttime = tmp[2]
        break
      }
      tmphi = tmphi + 1
    }
    if (is.null(starttime)) {
      stop(paste0("Start Time not found in the header of ", datafile))
    }
    #-------------------------------
    tmphi = 1
    while (tmphi < header_rows) {
      tmp = unlist(strsplit(format(tmph[tmphi,1]),"Start Date"))
      if (length(tmp) > 1) {
        startdate = tmp[2]
        break
      }
      tmphi = tmphi + 1
    }
    if (is.null(startdate)) {
      stop(paste0("Start Date not found in the header of ", datafile))
    }

    #-----------------------------------------
    # trim any spaces
    startdate = gsub(" ", "", startdate, fixed = TRUE)
    starttime = gsub(" ", "", starttime, fixed = TRUE)

    #-----------------------------------------
    # flexible four date/time formats
    starttime = paste0(startdate," ",starttime)
    getOption("digits.secs")
    options(digits.secs = 3)

    options(warn = -1)
    topline = as.matrix(colnames(as.matrix(read.csv(datafile, nrow = 1, skip = 0))))
    topline = topline[1]  #To avoid dots
    options(warn = 0)
    # Extraction of date format.
    # all formats to consider following R date formatting symbols:
    # Y year including century, y year excluding century, b month as character, m month as a number
    # Note: year in the middle of a date is assumed to not occur.
    allformats = c("mdY", "mdy","bdY", "bdy",
                   "dmY", "dmy", "dbY", "dby",
                   "Ymd", "ymd", "Ybd", "ybd",
                   "Ydm", "ydm", "Ydb", "ydb")
    fc = data.frame(matrix(0,1,length(allformats))) # data.frame to keep track of formats that have been checked
    names(fc) = allformats
    # Now systematically go through all possible formats to identify which one it is
    fc$mdY = length(grep("MM[.]dd[.]yyyy|M[.]d[.]yyyy|M[.]dd[.]yyyy|MM[.]d[.]yyyy",topline))
    fc$mdy = length(grep("MM[.]dd[.]yy|M[.]d[.]yy|M[.]dd[.]yy|MM[.]d[.]yy",topline))
    fc$bdY = length(grep("MMM[.]dd[.]yyyy|MMM[.]d[.]yyyy",topline))
    fc$bdy = length(grep("MMM[.]dd[.]yy|MMM[.]d[.]yy",topline))
    if (fc$mdY == 1 & fc$mdy == 1) fc$mdy = 0 # not yy when yyyy is also detected
    if (fc$bdY == 1 & fc$bdy == 1) fc$bdy = 0 # not yy when yyyy is also detected
    if (fc$mdY == 1 & fc$bdY == 1) fc$mdY = 0 # not M(M) when MMM is also detected for yyyy
    if (fc$mdy == 1 & fc$bdy == 1) fc$mdy = 0 # not M(M) when MMM is also detected for yy
    fc$dmY = length(grep("d[.]M[.]yyyy|d[.]MM[.]yyyy",topline))
    fc$dmy = length(grep("d[.]M[.]yy|d[.]MM[.]yy",topline))
    fc$dbY = length(grep("d[.]MMM[.]yyyy",topline))
    fc$dby = length(grep("d[.]MMM[.]yy",topline))
    if (fc$dmY == 1 & fc$dmy == 1) fc$dmy = 0 # not yy when yyyy is also detected
    if (fc$dbY == 1 & fc$dby == 1) fc$dby = 0 # not yy when yyyy is also detected
    if (fc$dmY == 1 & fc$dbY == 1) fc$dmY = 0 # not M(M) when MMM is also detected for yyyy
    if (fc$dmy == 1 & fc$dby == 1) fc$dmy = 0 # not M(M) when MMM is also detected for yy
    fc$Ymd = length(grep("yyyy[.]M[.]d|yyyy[.]MM[.]d",topline))
    fc$ymd = length(grep("yy[.]M[.]d|yy[.]MM[.]d",topline))
    fc$Ybd = length(grep("yyyy[.]MMM[.]d",topline))
    fc$ybd = length(grep("yy[.]MMM[.]d",topline))
    if (fc$Ymd == 1 & fc$ymd == 1) fc$ymd = 0 # not yy when yyyy is also detected
    if (fc$Ybd == 1 & fc$ybd == 1) fc$ybd = 0 # not yy when yyyy is also detected
    if (fc$Ymd == 1 & fc$Ybd == 1) fc$Ymd = 0 # not M(M) when MMM is also detected for yyyy
    if (fc$ymd == 1 & fc$ybd == 1) fc$ymd = 0 # not M(M) when MMM is also detected for yy
    fc$Ydm = length(grep("yyyy[.]dd[.]MM|yyyy[.]d[.]M|yyyy[.]d[.]MM|yyyy[.]dd[.]M",topline))
    fc$ydm = length(grep("yy[.]dd[.]MM|yy[.]d[.]M|yy[.]d[.]MM|yy[.]dd[.]M",topline))
    fc$Ydb = length(grep("yyyy[.]dd[.]MMM|yyyy[.]d[.]MMM",topline))
    fc$ydb = length(grep("yy[.]dd[.]MMM|yy[.]d[.]MMM",topline))
    if (fc$Ydm == 1 & fc$ydm == 1) fc$ydm = 0 # not yy when yyyy is also detected
    if (fc$Ydb == 1 & fc$ydb == 1) fc$ydb = 0 # not yy when yyyy is also detected
    if (fc$Ydm == 1 & fc$Ydb == 1) fc$Ydm = 0 # not M(M) when MMM is also detected for yyyy
    if (fc$ydm == 1 & fc$ydb == 1) fc$ydm = 0 # not M(M) when MMM is also detected for yy
    # Now we can identify which format it is:
    theformat = names(fc)[which(fc == 1)[1]]
    if (is.na(theformat)) warning("date format not recognised")
    splitformat = unlist(strsplit(theformat,""))
    # identify separater:
    if (length(grep("/",starttime)) > 0) {
      sepa = "/"
    } else {
      if (length(grep("-",starttime)) > 0) {
        sepa = "-"
      } else {
        if (length(grep(".",starttime)) > 0) {
          sepa = "."
        } else {
          warning("separator character for dates not identified")
        }
      }
    }
    expectedformat = paste0('%',splitformat[1],sepa,'%',splitformat[2],sepa,'%',splitformat[3],' %H:%M:%S')
    Sys.setlocale("LC_TIME", "C")  # set language to English because that is what we use elsewhere in GGIR
    starttime = as.POSIXct(starttime, format = expectedformat, tz = configtz)
    starttime = as.POSIXlt(starttime, tz = desiredtz)
  } else {
    stop(paste0("Timestamps not found for monitor type ", mon, " and file format type ", dformat,
                "\nThis should not happen."))
  }

  return(starttime)
}
