g.loadlog = function(loglocation = c(), coln1 = c(), colid = c(), 
                     sleeplogsep = ",", meta.sleep.folder = c(), desiredtz="") {
  # declare local functions:
  getIDstartdate = function(x) {
    rec_starttime = ID = c()
    load(x)
    invisible(list(ID = ID, rec_starttime = rec_starttime))
  }
  remove_empty_rows_cols = function(logmatrix, name) {
    logmatrix = as.data.frame(logmatrix[which((rowSums(logmatrix != "") != 0) == TRUE),
                                        which((colSums(logmatrix != "") != 0) == TRUE)])
    logmatrix = as.data.frame(logmatrix)
    if (length(name) > 0 & nrow(logmatrix) > 0) {
      newnames = c("ID", "date", rep(paste0(name, 1:ncol(logmatrix)), each = 2))
      colnames(logmatrix) = newnames[1:ncol(logmatrix)]
    }
    return(logmatrix)
  }
  removeEmptyCells = function(x) {
    if (nrow(x) == 1) {
      if (sum(x[, 2:ncol(x)] == "") == ncol(x) - 1) {
        emptyrows = 1
      } else {
        emptyrows = NULL
      }
    } else {
      emptyrows = which(rowSums(x[, 2:ncol(x)] == "") == ncol(x) - 1)
    }
    if (length(emptyrows)) {
      x = as.matrix(x[-emptyrows,])
    }
    if (length(x) != 0) {
      if (ncol(x) == 1 & nrow(x) > 1) {
        x = t(x)
      }
    }
    emptycols = which(colSums(x == "") == nrow(x))
    colp = ncol(x)
    twocols = c(colp - 1, colp)
    while (min(twocols) > 0) {
      if (all(twocols %in% emptycols)) {
        x = as.matrix(x[, -twocols])
        if (ncol(x) == 1) x = t(x)
        twocols = twocols - 2
      } else {
        break
      }
    }
    if (ncol(x) == 1) x = NULL
    return(x)
  }
  adjustLogFormat = function(S, nnights, mode = "sleeplog") {
    # function to adjust the sleeplog or bedlog format
    log = matrix(0,(nrow(S)*nnights),3)
    log_times = matrix(" ", (nrow(S) * nnights), 2)
    cnt = 1
    sli = coln1
    wki = sli + 1
    night = 1
    while (wki <= ncol(S)) { #loop through nights
      SL = as.character(S[,sli])
      WK = as.character(S[,wki])
      # Check whether any correction need to be made to the sleep log:
      for (j in 1:length(SL)) { #loop through participant
        # idtmp = S[j,colid]
        if (is.na(WK[j]) == FALSE & is.na(SL[j]) == FALSE & WK[j] != "" & SL[j] != "") {
          SLN = as.numeric(unlist(strsplit(SL[j],":")))
          WKN = as.numeric(unlist(strsplit(WK[j],":")))
          if (length(SLN) == 2) SLN = c(SLN,0) #add seconds when they are not stored
          if (length(WKN) == 2) WKN = c(WKN,0) #add seconds when they are not stored
          SL[j] = paste0(SLN[1], ":", SLN[2], ":", SLN[3])
          WK[j] = paste0(WKN[1], ":", WKN[2], ":", WKN[3])
          SLN2 = SLN[1] * 3600 + SLN[2] * 60 + SLN[3]
          WKN2 = WKN[1] * 3600 + WKN[2] * 60  + WKN[3]
          if (is.na(WKN2)) stop(paste0(WKN[1]," as found in the ", mode, " is not a valid timestamp"), call. = FALSE)
          if (is.na(SLN2)) stop(paste0(SLN[1]," as found in the ", mode, " is not a valid timestamp"), call. = FALSE)
          if (WKN2 > SLN2) { #e.g. 01:00 - 07:00
            dur = WKN2 - SLN2
          } else if (WKN2 < SLN2) { #e.g. 22:00 - 07:00
            dur = ((24*3600) - SLN2) + WKN2
          }
          dur = dur / 3600
        } else {
          cnt_time_notrecognise = cnt_time_notrecognise + 1
          dur = 0
          is.na(dur) =  TRUE
        }
        # add extra row if needed
        if (nrow(log) < cnt) {
          log = rbind(log, matrix(0, 1, 3))
          log_times = rbind(log_times, matrix(0, 1, 2))
        }
        # store information in log
        log[cnt,1] = as.character(S[j,colid])
        log[cnt,2] = night #ifelse(deltadate > 0, yes = i, no = i + abs(deltadate))
        log[cnt,3] = dur
        log_times[cnt,1] = SL[j]
        log_times[cnt,2] = WK[j]
        cnt = cnt + 1
      }
      sli = sli + 2
      wki = wki + 2
      night = night + 1
    }
    # delete id-numbers that are unrecognisable
    empty_rows = which(as.character(log[,1]) == "0")
    if (length(empty_rows) > 0) {
      log = log[-empty_rows, , drop = FALSE]
      log_times = log_times[-empty_rows, , drop = FALSE]
    }
    log = as.data.frame(log, stringsAsFactors = FALSE)
    names(log) = c("ID","night","duration")
    if (mode == "sleeplog") {
      log$sleeponset = log_times[,1]
      log$sleepwake = log_times[,2]
    } else if (mode == "bedlog") {
      log$bedstart = log_times[,1]
      log$bedend = log_times[,2]
    }
    # keep only the non-empty rows as they can only lead to confusion later on
    log = log[which(is.na(log$duration) == FALSE),]
    return(log)
  }
  
  #==========================================================================
  # Main code starts here
  #==========================================================================
  dateformat_correct = "%Y-%m-%d" # set default value
  deltadate = 0
  #===============================
  # Load sleep log data...
  S = data.table::fread(file = loglocation, stringsAsFactors = FALSE, data.table = FALSE,
                        check.names = TRUE, colClasses = "character")
  if (colnames(S)[1] == "V1" && any(S[1, ] == "")) {
    stop(paste0("Sleeplog column found with empty header, please fix. This can also happen if ",
                "there are empty columns at the end, delete those columns if applicable."), call. = FALSE)
  }
  cnt_time_notrecognise = 0
  advanced_sleeplog = length(grep(pattern = "date", x = colnames(S), ignore.case = TRUE)) > 0
  if (advanced_sleeplog ==  TRUE) {
    if (length(meta.sleep.folder) > 0) {
      
      startdates = lapply(X = dir(meta.sleep.folder, full.names = T), FUN = getIDstartdate)
      startdates = as.data.frame(data.table::rbindlist(startdates, fill = TRUE))
      
      startdates$startAtMidnight = FALSE
      # If recording starts at midnight or before 4am that first half night
      # is still counted in part 3, which means that the start date of the recording
      # does not equal the start date of the nights. Via startdates$startAtMidnight
      # we correct for this
      starthour = as.numeric(format(as.POSIXct(x = startdates$rec_starttime, format = "%Y-%m-%dT%H:%M:%S%z", tz = desiredtz), format = "%H"))
      startdates$startAtMidnight[which(starthour <= 4)] = TRUE
      colnames(startdates)[1:2] = c("ID", "startdate")
      startdates$startdate = as.Date(iso8601chartime2POSIX(startdates$startdate, tz = desiredtz), tz = desiredtz)
    } else {
      warning("\nArgument meta.sleep.folder has not been specified")
    }
  }
  if (length(S) == 0) {
    warning(paste0("Could not read sleeplog file, check that file path is correct.",
                   "Tip: Try to aply function g.loadlog to your sleeplog file first",
                   " to verify that sleeplog is correctly processed."), call. = FALSE)
  } else {
    if (nrow(S) == 0 | ncol(S) <= 2) {
      warning(paste0("Could not read sleeplog file. Does it have at least 3 columns",
                     " and comma seperated values?",
                     " Tip: Try to aply function g.loadlog to your sleeplog file ",
                     "first to verify that sleeplog is correctly processed."), call. = FALSE)
    }
  }
  count = 1 # to keep track of row in new sleeplog matrix
  naplog = nonwearlog = newsleeplog = imputecodelog = c()
  if (advanced_sleeplog ==  TRUE) {
    # assumptions:
    # if date occurs in column names we assume it is an advanced sleeplogreport
    # columnames with onset|inbed|tobed|lightsout represent start of the main sleep/inbed window in a day
    # columnames with wakeup represent end of the main sleep/inbed window in a day
    # columnames with nap represent nap start or end-times
    # columnames with nonwear represent nonwear start or end-times
    # dates are expressed as YYYY-mm-dd
    datecols = grep(pattern = "date", x = colnames(S), value = FALSE, ignore.case = TRUE)
    nnights = length(datecols)
    # if date occurs in column names at least twice we assume it is an advanced sleeplogreport
    if (length(datecols) > 1) { # if yes, do:
      bedstartcols = grep(pattern = "lightsout|inbed|tobed|bedstart",x = colnames(S), value = FALSE, ignore.case = TRUE)
      bedendcols = grep(pattern = "lightson|outbed|bedend",x = colnames(S), value = FALSE, ignore.case = TRUE)
      wakecols = grep(pattern = "wakeup",x = colnames(S), value = FALSE, ignore.case = TRUE)
      onsetcols = grep(pattern = "onset",x = colnames(S), value = FALSE, ignore.case = TRUE)
      napcols = grep(pattern = "nap",x = colnames(S), value = FALSE, ignore.case = TRUE)
      nonwearcols = grep(pattern = "nonwear",x = colnames(S), value = FALSE, ignore.case = TRUE)
      imputecols = grep(pattern = "impute",x = colnames(S), value = FALSE, ignore.case = TRUE)
      # Create new sleeplog consisting of:
      # - original ID column
      # - empty columns if relevant to make sleeplog match accelerometer recording, make sure coln1 argument is used
      # - onset and wakup times of sleeplog, for this extract dates from sleeplog to check for missing days
      newsleeplog = matrix("", nrow(S), max(c(nnights*2, 100)) + 1)
      newbedlog = matrix("", nrow(S), max(c(nnights*2, 100)) + 1)
      naplog = matrix("", nrow(S)*nnights * 5, 50) #ID date start end
      nonwearlog = matrix("", nrow(S)*nnights * 5, 50) #ID date start end
      if (length(imputecols) > 0) {
        imputecodelog = matrix("", nrow(S)*nnights * 2, 50) #ID date start end
      } else {
        imputecodelog = NULL
      }
      napcnt = nwcnt = iccnt = 1
      IDcouldNotBeMatched = TRUE
      dateformat_found = FALSE
      for (i in 1:nrow(S)) { # loop through rows in sleeplog
        ID = S[i,colid]
        if (ID %in% startdates$ID == TRUE) { # matching ID in acc data, if not ignore ID
          IDcouldNotBeMatched = FALSE
          startdate_acc = as.Date(startdates$startdate[which(startdates$ID == ID)], tz = desiredtz)
          startdate_sleeplog = as.character(S[i, datecols[1:pmin(length(datecols), 5)]])
          Sdates_correct = c()
          dateformats_to_consider = c("%Y-%m-%d", "%d-%m-%Y", "%m-%d-%Y", "%Y-%d-%m",
                                      "%y-%m-%d", "%d-%m-%y", "%m-%d-%y", "%y-%d-%m",
                                      "%Y/%m/%d", "%d/%m/%Y", "%m/%d/%Y", "%Y/%d/%m",
                                      "%y/%m/%d", "%d/%m/%y", "%m/%d/%y", "%y/%d/%m")
          if (dateformat_found == TRUE && dateformats_to_consider[1] != dateformat_correct) {
            # If found then first try that before trying anything else
            dateformats_to_consider = c(dateformat_correct, dateformats_to_consider)
          }
          # Detect data format in sleeplog:
          for (dateformat in dateformats_to_consider) {
            startdate_sleeplog_tmp = as.Date(startdate_sleeplog, format = dateformat, tz = desiredtz)
            Sdates = as.Date(as.character(S[i,datecols]), format = dateformat, tz = desiredtz)
            if (length(which(diff(which(is.na(Sdates))) > 1)) > 0) {
              stop(paste0("\nSleeplog for ID: ", ID, " has missing date(s)"), call. = FALSE)
            }
            if (all(is.na(startdate_sleeplog_tmp) == FALSE)) {
              deltadate = as.numeric(startdate_sleeplog_tmp - startdate_acc)
              if (all(is.na(deltadate) == FALSE)) {
                if (all(abs(deltadate) < 30)) {
                  startdate_sleeplog = startdate_sleeplog_tmp[1]
                  Sdates_correct = Sdates
                  dateformat_correct = dateformat
                  deltadate = deltadate[1]
                  dateformat_found = TRUE
                  break
                }
              }
            }
          }
          if (startdates$startAtMidnight[which(startdates$ID == ID)] == TRUE) {
            # If the first day in the advanced sleeplog is 28/11 
            # and the recording starts at midnight 27/11 00:00:00
            # then that means that we miss the first 2 nights.
            # However, the code above only sees a
            # difference of 1 day (deltadate) between 27/11 and 28/11.
            # If the recording starts at 27/11 00:00:05 this is correct 
            # because 27/11 is not counted as a night in g.part3 and g.part4.
            # This is why we need to do + 1 if the recording starts at midnight.
            deltadate = deltadate + 1
          }
          
          if (length(Sdates_correct) == 0 | is.na(startdate_sleeplog) == TRUE) {
            warning(paste0("\nSleeplog for ID: ",ID," not used because first date",
                           " not within 30 days of first date in accerometer recording"), call. = FALSE)
          } else {
            # handle missing dates
            ndates = as.numeric(diff(range(Sdates_correct[!is.na(Sdates_correct)]))) + 1
            if (ndates > nnights) {
              extraColumns = matrix("", nrow(newsleeplog), max(c((ndates - nnights)*2, 100)) + 1)
              newsleeplog = cbind(newsleeplog, extraColumns)
              newbedlog = cbind(newbedlog, extraColumns)
              extraColumns = matrix("", nrow(naplog), max(c((ndates - nnights)*2, 100)) + 1)
              naplog = cbind(naplog, extraColumns)
              nonwearlog = cbind(nonwearlog, extraColumns)
              nnights = ndates
            }
            # only attempt to use sleeplog if start date could be recognised
            # Add row to newsleeplog if somehow there are not enough rows
            if (count > nrow(newsleeplog)) {
              newsleeplog = rbind(newsleeplog, matrix(NA, 1, ncol(newsleeplog)))
            }
            if (count > nrow(newbedlog)) {
              newbedlog = rbind(newbedlog, matrix(NA, 1, ncol(newbedlog)))
            }
            newsleeplog[count ,1] = ID
            newbedlog[count ,1] = ID
            newsleeplog_times = newbedlog_times = c()
            expected_dates = seq(startdate_sleeplog - deltadate, startdate_sleeplog + nnights, by = 1)
            # loop over expect dates giving start date of sleeplog
            for (ni in 1:(length(expected_dates) - 1)) {
              # checking whether date exists in sleeplog
              ind = which(Sdates_correct == as.Date(expected_dates[ni], tz = desiredtz))
              if (length(ind) > 0) {
                if (length(ind) > 1) {
                  duplicatedDate = unique(as.character(S[i, datecols[ind]]))
                  stop(paste0("\n", ID, " has duplicate dates in the diary, please fix ", duplicatedDate), call. = FALSE)
                }
                curdatecol = datecols[ind]
                nextdatecol =  datecols[which(datecols > curdatecol)[1]]
                if (is.na(nextdatecol)) nextdatecol = ncol(S) + 1
                # Handle mixed reporting of time in bed and SPT"
                if (length(bedendcols) == 0 & length(bedstartcols) != 0 &
                    length(onsetcols) == 0 & length(wakecols) != 0) {
                  # bedstart and wakeup are present, but bedend and sleeponset not,
                  # treat wake as bedend such that this can be treated as time in bed period
                  bedendcols = wakecols
                  wakecols = NULL
                }
                if (length(bedendcols) != 0 & length(bedstartcols) == 0 &
                    length(onsetcols) != 0 & length(wakecols) == 0) {
                  # bedend and onsetpresent, but bedstart and wakeup not,
                  # treat bedend as wakeup such that this can be treat as SPT
                  wakecols = bedendcols
                  bedendcols = NULL
                }
                # Sleeplog:
                onseti = onsetcols[which(onsetcols > curdatecol & onsetcols < nextdatecol)]
                wakeupi = wakecols[which(wakecols > nextdatecol)[1]]
                if (length(onseti) == 1 & length(wakeupi) == 1) {
                  newsleeplog_times = c(newsleeplog_times, S[i,onseti], S[i,wakeupi])
                } else {
                  newsleeplog_times = c(newsleeplog_times, "", "")
                }
                # time in bed
                bedstarti = bedstartcols[which(bedstartcols > curdatecol & bedstartcols < nextdatecol)]
                bedendi = bedendcols[which(bedendcols > nextdatecol)[1]]
                if (length(bedstarti) == 1 & length(bedendi) == 1) {
                  newbedlog_times = c(newbedlog_times, S[i,bedstarti], S[i,bedendi])
                } else {
                  newbedlog_times = c(newbedlog_times, "", "")
                }
                # Also grap nap, non-wear, and imputation code info and put those in separate matrices:
                naps = napcols[which(napcols  > curdatecol & napcols < nextdatecol)]
                nonwears = nonwearcols[which(nonwearcols  > curdatecol & nonwearcols < nextdatecol)]
                imputecodes = imputecols[which(imputecols  > curdatecol & imputecols < nextdatecol)]
                if (length(naps) > 0) {
                  naplog[napcnt, 1] = ID
                  naplog[napcnt, 2] = S[i, curdatecol]
                  naplog[napcnt, 3:(2 + length(naps))] = as.character(S[i, naps])
                  napcnt = napcnt + 1
                }
                if (length(nonwears) > 0) {
                  nonwearlog[nwcnt, 1] = ID
                  nonwearlog[nwcnt, 2] = S[i, curdatecol]
                  nonwearlog[nwcnt, 3:(2 + length(nonwears))] = as.character(S[i, nonwears ])
                  nwcnt = nwcnt + 1
                }
                if (length(imputecodes) > 0) {
                  imputecodelog[iccnt, 1] = ID
                  imputecodelog[iccnt, 2] = S[i, curdatecol]
                  imputecodelog[iccnt, 3:(2 + length(imputecodes))] = gsub("[^0-9.-]", "", as.character(S[i, imputecodes ]))
                  iccnt = iccnt + 1
                }
              } else {
                newsleeplog_times = c(newsleeplog_times, "", "")
                newbedlog_times = c(newbedlog_times, "", "")
              }
            }
            # add columns to sleeplog
            extracols = (length(newsleeplog_times) + 2) - ncol(newsleeplog)
            if (extracols > 0) {
              newsleeplog = cbind(newsleeplog, matrix(NA, nrow(newsleeplog), extracols))
            }
            newsleeplog[count, 2:(length(newsleeplog_times) + 1)] = newsleeplog_times
            
            # add columns to bedlog
            extracols = (length(newbedlog_times) + 2) - ncol(newbedlog)
            if (extracols > 0) {
              newbedlog = cbind(newbedlog, matrix(NA, nrow(newbedlog), extracols))
            }
            newbedlog[count, 2:(length(newbedlog_times) + 1)] = newbedlog_times
            
            count  = count + 1
          }
        }
      }
      if (IDcouldNotBeMatched == TRUE) {
        warning(paste0("\nNone of the IDs in the accelerometer data could be matched with",
                       " the ID numbers in the sleeplog. You may want to check that the ID",
                       " format in your sleeplog is consistent with the ID column in the GGIR part2 csv-report,",
                       " and that argument coldid is correctly set."), call. = FALSE)
      }
      # remove empty rows and columns:
      
      if (length(naplog) > 0) {
        naplog = remove_empty_rows_cols(naplog, name = "nap")
      }
      if (length(nonwearlog) > 0) {
        nonwearlog = remove_empty_rows_cols(nonwearlog, name = "nonwear")
      }
      if (length(imputecodelog) > 0) {
        imputecodelog = remove_empty_rows_cols(imputecodelog, name = "imputecode")
        colnames(imputecodelog)[3] = "imputecode"
        imputecodelog$date = as.Date(imputecodelog$date, dateformat_correct)
      }
      
      if (length(newsleeplog) > 0) {
        newsleeplog = removeEmptyCells(newsleeplog)
        if (!is.null(newsleeplog)) {
          S = as.data.frame(newsleeplog)
        }
        coln1 = 2
        colid = 1
      }
      if (length(newbedlog) > 0) {
        newbedlog = removeEmptyCells(newbedlog)
        if (!is.null(newbedlog)) {
          B = as.data.frame(newbedlog)
        }
        coln1 = 2
        colid = 1
      }
    }
  } else {
    nnights = (ncol(S) - coln1 + 1) / 2
  }
  # test whether number of columns with night information in sleeplog is odd
  # this would provide nnights %% 2 == 0.5
  if (nnights %% 2 == 0.5) { 
    warning(paste0("\nWe see an odd number of timestamp columns",
                   " in the sleeplog. The last column will be ignored. If this is incorrect,",
                   " please check that argument coln1 is correctly specified if you use a basic sleeplog format and",
                   " that all days have a date column if you use an advanced sleeplog format."), call. = FALSE)
    nnights = floor(nnights)
  }
  nnights = nnights + deltadate + 1 # to account for the possibility of extra night at the beginning of recording
  # # From here we continue with original code focused on sleeplog only
  if (exists("S") && ncol(S) > 0) {
    sleeplog = adjustLogFormat(S, nnights, mode = "sleeplog")
  } else {
    sleeplog = NULL
  }
  if (exists("B")) {
    bedlog = adjustLogFormat(B, nnights, mode = "bedlog")
  } else {
    bedlog = NULL
  }
  invisible(list(sleeplog = sleeplog, nonwearlog = nonwearlog, naplog = naplog, bedlog = bedlog,
                 imputecodelog = imputecodelog,
                 dateformat = dateformat_correct))
}
