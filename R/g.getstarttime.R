g.getstarttime = function(datafile, data, header, mon, dformat, desiredtz, configtz = NULL) {
  if (is.null(configtz)) {
    configtz = desiredtz
  }
  
  if (mon == MONITOR$MOVISENS) {
    starttime = unisensR::readUnisensStartTime(dirname(datafile))
    if (attr(starttime, "tzone") != desiredtz) {
      attr(starttime, "tzone") <- desiredtz
    }
  } else if ((mon == MONITOR$ACTIGRAPH && dformat == FORMAT$CSV) || mon == MONITOR$VERISENSE) {
    tmph = read.csv(datafile, nrow = 8, skip = 1)
    tmphi = 1
    while (tmphi < 10) {
      if (length(unlist(strsplit(format(tmph[tmphi,1]),"Start Time"))) > 1) {
        starttime = unlist(strsplit(format(tmph[tmphi,1]),"Start Time"))[2]
        break
      }
      tmphi = tmphi + 1
    }
    #-------------------------------
    tmphi = 1
    while (tmphi < 10) {
      if (length(unlist(strsplit(format(tmph[tmphi,1]),"Start Date"))) > 1) {
        startdate = unlist(strsplit(format(tmph[tmphi,1]), "Start Date"))[2]
        break
      }
      tmphi = tmphi + 1
    }

    #remove possible spaces in date or time
    startdate = gsub(" ", "", startdate)
    starttime = gsub(" ", "", starttime)

    starttime = paste0(startdate, " ", starttime)
    
    Sys.setlocale("LC_TIME", "C")  # set language to English because that is what we use elsewhere in GGIR

    starttime = as.POSIXlt(starttime, tz = configtz,
                           tryFormats = c("%m-%d-%Y %H:%M:%OS", "%m/%d/%Y %H:%M:%OS",
                                          "%m-%d-%y %H:%M:%OS", "%m/%d/%y %H:%M:%OS",
                                          "%b-%d-%Y %H:%M:%OS", "%b/%d/%Y %H:%M:%OS",
                                          "%b-%d-%y %H:%M:%OS", "%b/%d/%y %H:%M:%OS",
                                          "%B-%d-%Y %H:%M:%OS", "%B/%d/%Y %H:%M:%OS",
                                          "%B-%d-%y %H:%M:%OS", "%B/%d/%y %H:%M:%OS",
                                          "%d-%m-%Y %H:%M:%OS", "%d/%m/%Y %H:%M:%OS",
                                          "%d-%m-%y %H:%M:%OS", "%d/%m/%y %H:%M:%OS",
                                          "%d-%b-%Y %H:%M:%OS", "%d/%b/%Y %H:%M:%OS",
                                          "%d-%b-%y %H:%M:%OS", "%d/%b/%y %H:%M:%OS",
                                          "%d-%B-%Y %H:%M:%OS", "%d/%B/%Y %H:%M:%OS",
                                          "%d-%B-%y %H:%M:%OS", "%d/%B/%y %H:%M:%OS",
                                          "%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS",
                                          "%y-%m-%d %H:%M:%OS", "%y/%m/%d %H:%M:%OS",
                                          "%Y-%b-%d %H:%M:%OS", "%Y/%b/%d %H:%M:%OS",
                                          "%y-%b-%d %H:%M:%OS", "%y/%b/%d %H:%M:%OS",
                                          "%Y-%B-%d %H:%M:%OS", "%Y/%B/%d %H:%M:%OS",
                                          "%y-%B-%d %H:%M:%OS", "%y/%B/%d %H:%M:%OS",
                                          "%Y-%d-%m %H:%M:%OS", "%Y/%d/%m %H:%M:%OS",
                                          "%y-%d-%m %H:%M:%OS", "%y/%d/%m %H:%M:%OS",
                                          "%Y-%d-%b %H:%M:%OS", "%Y/%d/%b %H:%M:%OS",
                                          "%y-%d-%b %H:%M:%OS", "%y/%d/%b %H:%M:%OS",
                                          "%Y-%d-%B %H:%M:%OS", "%Y/%d/%B %H:%M:%OS",
                                          "%y-%d-%B %H:%M:%OS", "%y/%d/%B %H:%M:%OS"
                                          ))
    if (configtz != desiredtz) {
      attr(starttime, "tzone") <- desiredtz
    }

   } else {
    starttime = data$time[1] # this could be a numeric or a POSIXct
    starttime = as.POSIXlt(starttime, tz = desiredtz, origin = "1970-01-01")
  }
  return(starttime)
}
