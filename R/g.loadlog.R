g.loadlog = function(loglocation = c(), coln1 = c(), colid = c(), nnights = c(),
                     sleeplogidnum=TRUE,  sleeplogsep = ",", meta.sleep.folder = c(), desiredtz="") {
  
  dateformat_correct = "%Y-%m-%d" # set default value
  deltadate = 0
  #===============================
  # Load sleep log data...
  S = read.csv(loglocation, sep = sleeplogsep, stringsAsFactors = FALSE)
  cnt_time_notrecognise = 0
  advanced_sleeplog = length(grep(pattern = "date", x = colnames(S))) > 0
  if (advanced_sleeplog ==  TRUE) {
    if (length(meta.sleep.folder) > 0) {
      getIDstartdate = function(x) {
        rec_starttime = ID = c()
        load(x)
        invisible(list(ID = ID, rec_starttime = rec_starttime))
      }
      startdates = lapply(X = dir(meta.sleep.folder, full.names = T), FUN = getIDstartdate)
  
      startdates = data.table::rbindlist(startdates, fill = TRUE)
      colnames(startdates) = c("ID", "startdate")
      startdates$startdate = as.Date(iso8601chartime2POSIX(startdates$startdate, tz = desiredtz))
      if (sleeplogidnum == TRUE) {
        startdates$ID = as.numeric(startdates$ID)
      }
    } else {
      warning("\nArgument meta.sleep.folder has not been specified")
    }
  }
  if (length(S) == 0) {
    cat("\nCould not read sleeplog file, check that file path is correct.")
    cat("\nTip: Try to aply function g.loadlog to your sleeplog file first to verify that sleeplog is correctly processed.")
  } else {
    if (nrow(S) == 0 | ncol(S) <= 2) {
      cat("\nCould not read sleeplog file. Does it have at least 3 columns and comma seperated values?")
      cat("\nTip: Try to aply function g.loadlog to your sleeplog file first to verify that sleeplog is correctly processed.")
    }
  }
  count = 1 # to keep track of row in new sleeplog matrix
  naplog = nonwearlog = newsleeplog = c()
  if (advanced_sleeplog ==  TRUE) {
    # assumptions:
    # if date occurs in column names we assume it is an advanced sleeplogreport
    # columnames with onset|inbed|tobed|lightsout represent start of the main sleep/inbed window in a day
    # columnames with wakeup represent end of the main sleep/inbed window in a day
    # columnames with nap represent nap start or end-times
    # columnames with nonwear represent nonwear start or end-times
    # dates are expressed as YYYY-mm-dd
    datecols = grep(pattern = "date", x = colnames(S), value = FALSE)
    # if date occurs in column names at least twice we assume it is an advanced sleeplogreport
    if (length(datecols) > 1) { # if yes, do:
      wakecols = grep(pattern = "wakeup",x = colnames(S), value = FALSE)
      onsetcols = grep(pattern = "onset|inbed|tobed|lightsout",x = colnames(S), value = FALSE)
      napcols = grep(pattern = "nap",x = colnames(S), value = FALSE)
      nonwearcols = grep(pattern = "nonwear",x = colnames(S), value = FALSE)
      # Create new sleeplog consisting of:
      # - original ID column
      # - empty columns if relevant to make sleeplog match accelerometer recording, make sure coln1 argument is used
      # - onset and wakup times of sleeplog, for this extract dates from sleeplog to check for missing days
      newsleeplog = matrix("", nrow(S), max(c(nnights*2, 100)) + 1)
      naplog = matrix("", nrow(S)*nnights * 5, 50) #ID date start end
      nonwearlog = matrix("", nrow(S)*nnights * 5, 50) #ID date start end
      napcnt = 1
      nwcnt = 1
      IDcouldNotBeMatched = TRUE
      for (i in 1:nrow(S)) { # loop through rows in sleeplog
        ID = S[i,colid]
        if (ID %in% startdates$ID == TRUE) { # matching ID in acc data, if not ignore ID
          IDcouldNotBeMatched = FALSE
          startdate_acc = as.Date(startdates$startdate[which(startdates$ID == ID)])
          startdate_sleeplog = S[i, datecols[1]]
          Sdates_correct = c()
          # Detect data format in sleeplog:
          for (dateformat in c("%Y-%m-%d", "%d-%m-%Y", "%m-%d-%Y", "%Y-%d-%m", 
                               "%y-%m-%d", "%d-%m-%y", "%m-%d-%y", "%y-%d-%m", 
                               "%Y/%m/%d", "%d/%m/%Y", "%m/%d/%Y", "%Y/%d/%m",
                               "%y/%m/%d", "%d/%m/%y", "%m/%d/%y", "%y/%d/%m")) {
            startdate_sleeplog_tmp = as.Date(startdate_sleeplog, format = dateformat) 
            Sdates = as.Date(as.character(S[i,datecols]), format = dateformat)
            if (is.na(startdate_sleeplog_tmp) == FALSE) {
              deltadate = as.numeric(startdate_sleeplog_tmp - startdate_acc)
              if (is.na(deltadate) == FALSE) {
                if (abs(deltadate) < 30) {
                  startdate_sleeplog = startdate_sleeplog_tmp
                  Sdates_correct = Sdates
                  dateformat_correct = dateformat
                  break
                }
              }
            }
          }
          if (length(Sdates_correct) == 0 | is.na(startdate_sleeplog) == TRUE) {
            warning(paste0("\nSleeplog for ID: ",ID," not used because first date",
                           " not within 30 days of first date in accerometer recording"), call. = FALSE)
          } else {
            # only attempt to use sleeplog if start date could be recognisedd
            # Add row to newsleeplog if somehow there are not enough rows
            if (count > nrow(newsleeplog)) {
              newsleeplog = rbind(newsleeplog, matrix(NA, 1, ncol(newsleeplog)))
            }
            newsleeplog[count ,1] = ID
            newsleeplog_times = c()
            expected_dates = seq(startdate_sleeplog - deltadate, startdate_sleeplog + nnights, by = 1)
            # loop over expect dates giving start date of sleeplog
            for (ni in 1:(length(expected_dates) - 1)) { 
              # checking whether date exists in sleeplog
              ind = which(Sdates_correct == as.Date(expected_dates[ni]))
              if (length(ind) > 0) {
                curdatecol = datecols[ind]
                nextdatecol =  datecols[which(datecols > curdatecol)[1]]
                onseti = onsetcols[which(onsetcols > curdatecol & onsetcols < nextdatecol)]
                # if (ni < (length(expected_dates) - 1)) {
                wakeupi = wakecols[which(wakecols > nextdatecol)[1]]
                # } else if (ni == length(expected_dates)-1) {
                #   wakeupi = wakecols[which(wakecols > curdatecol)[1]]
                # }
                if (length(onseti) == 1 & length(wakeupi) == 1) {
                  newsleeplog_times = c(newsleeplog_times, S[i,onseti], S[i,wakeupi])
                } else {
                  newsleeplog_times = c(newsleeplog_times, "", "")
                }
                # Also grap nap and non-wear info and put those in separate matrix:
                naps = napcols[which(napcols  > curdatecol & napcols < nextdatecol)]
                nonwears = nonwearcols[which(nonwearcols  > curdatecol & nonwearcols < nextdatecol)]
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
              } else {
                newsleeplog_times = c(newsleeplog_times, "", "")
              }
            }
            extracols = (length(newsleeplog_times) + 2) - ncol(newsleeplog)
            if (extracols > 0) {
              newsleeplog = cbind(newsleeplog, matrix(NA, nrow(newsleeplog), extracols))
            }
            newsleeplog[count, 2:(length(newsleeplog_times) + 1)] = newsleeplog_times
            count  = count + 1  
          }
        }
      }
      if (IDcouldNotBeMatched == TRUE) {
        warning(paste0("\nNone of the IDs in the accelerometer data could be matched with",
                       " the ID numbers in the sleeplog. You may want to check that the ID",
                       " format in your sleeplog is consistent with the ID column in the GGIR part2 csv-report,", 
                       " and that arguments coldid and sleeplogidnum are correctly set."), call. = FALSE)
      }
      # remove empty rows and columns:
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
      if (length(naplog) > 0) {
        naplog = remove_empty_rows_cols(naplog, name = "nap")
      }
      if (length(nonwearlog) > 0) {
        nonwearlog = remove_empty_rows_cols(nonwearlog, name = "nonwear")
      }
      if (length(newsleeplog) > 0) {
        emptyrows = which(rowSums(newsleeplog == "") == ncol(newsleeplog)) 
        if (length(emptyrows)) {
          newsleeplog = newsleeplog[-emptyrows,]
        }
        emptycols = which(colSums(newsleeplog == "") == nrow(newsleeplog)) 
        colp = ncol(newsleeplog)
        twocols = c(colp - 1, colp)
        while (min(twocols) > 0) {
          if (all(twocols %in% emptycols)) {
            newsleeplog = as.matrix(newsleeplog[, -twocols])
            if (ncol(newsleeplog) == 1) newsleeplog = t(newsleeplog)
            twocols = twocols - 2
          } else {
            break
          }
        }
        S = as.data.frame(newsleeplog)
        coln1 = 2
        colid = 1
        if (sleeplogidnum == TRUE) {
          S[,1] = as.numeric(as.character(S[,1]))
        }
      }
    }
  }
  nnights = nnights + deltadate
  # # From here we continue with original code focused on sleeplog only
  sleeplog = matrix(0,(nrow(S)*nnights),3)
  sleeplog_times = matrix(" ", (nrow(S) * nnights), 2)
  cnt = 1
  nnights = min(floor((ncol(S) - 1) / 2), nnights)
  for (i in 1:nnights) { #loop through nights
    SL = as.character(S[,coln1 + ((i - 1) * 2)])
    WK = as.character(S[,coln1 + ((i - 1) * 2) + 1])
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
      if (sleeplogidnum == TRUE) {
        sleeplog[cnt,1] = round(S[j,colid])
      } else {
        sleeplog[cnt,1] = as.character(S[j,colid])
      }
      sleeplog[cnt,2] = i #ifelse(deltadate > 0, yes = i, no = i + abs(deltadate))
      sleeplog[cnt,3] = dur
      sleeplog_times[cnt,1] = SL[j]
      sleeplog_times[cnt,2] = WK[j]
      cnt = cnt + 1
    }
  }
  # delete id-numbers that are unrecognisable
  empty_rows = which(as.character(sleeplog[,1]) == "0")
  if (length(empty_rows) > 0) {
    sleeplog = sleeplog[-empty_rows,]
    sleeplog_times = sleeplog_times[-empty_rows,]
  }
  sleeplog = as.data.frame(sleeplog, stringsAsFactors = FALSE)
  names(sleeplog) = c("ID","night","duration")
  sleeplog$sleeponset = sleeplog_times[,1]
  sleeplog$sleepwake = sleeplog_times[,2]
  # keep only the non-empty rows as they can only lead to confusion later on
  sleeplog = sleeplog[which(is.na(sleeplog$duration) == FALSE),]
  invisible(list(sleeplog = sleeplog, nonwearlog = nonwearlog, naplog = naplog, 
                 dateformat = dateformat_correct))
}
