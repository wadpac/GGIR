g.convert.part2.long = function(daySUMMARY) {
  # local functions:
  extractlastpart = function(x) { 
    # function extract windowsegment specification which 
    # is at the end of the variable name
    tmp = unlist(strsplit(x,"_"))
    if (length(tmp) > 1) {
      tmp = tmp[length(tmp)]
      tmp2 = unlist(strsplit(tmp,"-"))
      if (length(tmp2) < 2) tmp = ""
    } else {
      tmp = ""
    }
    if (tmp != "") {
      out = c(unlist(strsplit(x, paste0(tmp,"_")))[1], tmp)
    } else {
      out = c("","")
    }
    return(out)
  }
  convert2minutes = function(x) {
    x = as.numeric(x)
    hour = floor(x)
    minutes = round((x-floor(x)) * 60)
    if (nchar(minutes) == 1) minutes = paste0("0",minutes)
    if (nchar(hour) == 1) minutes = paste0("0",hour)
    time = paste0(hour,":",minutes)
    return(time)
  }
  
  convert2clocktime = function(x) {
    x2 = unlist(strsplit(x,"hr"))
    if (length(x2) > 0) {
      x3 = as.numeric(unlist(strsplit(x2[1],"-")))
      myfun = function(y) {
        for (i in 1:length(y)) {
          if (y[i] < 10) {
            y[i] = paste0(0,y[i])
          } else {
            y[i] = as.character(y[i])
          }
        }
        return(y)
      }
      x4 = paste0(paste0(myfun(x3),":00"), collapse="-")
    } else {
      x4 = ""
    }
    return(x4)
  }
  
  getvar = function(x) {
    tmp = unlist(strsplit(as.character(x),"_"))
    return(paste0(tmp[1:(length(tmp)-1)],collapse="_"))
  }
  gettimeseg = function(x) {
    tmp = unlist(strsplit(as.character(x),"_"))
    tmp2 = unlist(strsplit(tmp[length(tmp)],"[.]"))[1]
    return(tmp2)
  }
  #------------------------------
  # main code
  # replace spaces by underscores, because melt function struggles with it later on
  colnames(daySUMMARY) = gsub(pattern = " ",replacement = "_", x = colnames(daySUMMARY))
  cn = names(daySUMMARY)
  # extract windowsegment specification from variable name
  tt = sapply(cn, FUN = extractlastpart)
  id.vars = colnames(tt[,which(tt[2,] == "")]) # identify other varibales, including ID
  daySUMMARY = data.table::as.data.table(daySUMMARY)
  # turn into long format with melt, this will make it too long, so further
  # down we will move key variables to wide again
  daySUMMARY2 = data.table::melt(daySUMMARY, id.vars = id.vars, 
                                 measure.vars = colnames(tt)[which(tt[2,]!= "")])
  # extract variable names
  daySUMMARY2$variable2 = sapply(daySUMMARY2$variable, FUN = getvar)
  # extract time segment names
  daySUMMARY2$timesegment2 = sapply(daySUMMARY2$variable, FUN = gettimeseg)
  daySUMMARY2 = as.data.frame(daySUMMARY2)
  # tidy up
  rem = which(colnames(daySUMMARY2) == "variable")
  daySUMMARY2 = daySUMMARY2[, -rem] # It is a data.table object so column removal works different
  id.vars = c(id.vars, "timesegment2")
  cnt = 1
  # long format that is undersirable is now moved back to wide format
  for (i in unique(daySUMMARY2$variable2)) {
    if (cnt == 1) {
      df = daySUMMARY2[which(daySUMMARY2$variable2 == i),]
      colnames(df)[which(colnames(df) == "value")] = i
      rem = which(colnames(df) == "variable2")
      df = df[,-rem]
    } else {
      df = base::merge(df, daySUMMARY2[which(daySUMMARY2$variable2 == i),], by = id.vars, all.x = T, sort = F)
      colnames(df)[which(colnames(df) == "value")] = i
      rem = which(colnames(df) == "variable2")
      df = df[, -rem]
    }
    cnt = cnt + 1
  }
  # tidy up variable names
  Nvh.1 = which(colnames(df) == "N_valid_hours.1")
  Nh.1 = which(colnames(df) == "N_hours.1")
  if (length(Nvh.1) > 0) colnames(df)[Nvh.1] = "N_valid_hours_in_window"
  if (length(Nh.1) > 0) colnames(df)[Nh.1] = "N_hours_in_window"
  df$N_valid_hours_in_window[which(df$timesegment2 == "0-24hr")] = ""
  df$N_hours_in_window[which(df$timesegment2 == "0-24hr")] = ""
  # re-order columns to be more logical
  col2move = which(colnames(df) %in% c("N_valid_hours_in_window","N_hours_in_window") == TRUE)
  col2NH = which(colnames(df) %in% c("N_valid_hours","N_hours") == TRUE)
  df = df[,c(1:max(col2NH),col2move,(max(col2NH)+1):(ncol(df)-2))]
  # re-place underscore to hyphen
  skip.qwindow_name.conversion = FALSE
  if (all(df$qwindow_timestamps == df$qwindow_names)) { # using qwindow vector without diary
    df$qwindow_timestamps = sapply(X = df$timesegment2, FUN = convert2clocktime)
    skip.qwindow_name.conversion = TRUE
  }
  
  rplc = which(df$qwindow_timestamps == "00:00 24:00")
  if (length(rplc) > 0) {
    df$qwindow_timestamps[rplc] = "00:00-24:00"
  }
  # the segment names are not correct yet, rename them
  for (ji in unique(df$ID)) {
    for (hi in unique(df$calendar_date[which(df$ID == ji)])) {
      df_tmp = df[which(df$ID == ji & df$calendar_date == hi),c("qwindow_timestamps", "qwindow_names",
                                                                "timesegment2")]
      tms = unlist(strsplit(df_tmp$qwindow_timestamps[1],"_"))
      nms = unlist(strsplit(df_tmp$qwindow_names[1],"_"))
      namekey = matrix("",length(tms),2)
      for (gi in 1:(length(tms))) {
        if (gi == 1) {
          options(warn=-1)
          qwindownumeric = !is.na(as.numeric(tms[1]))
          options(warn=0)
          if (qwindownumeric == FALSE) {
            namekey[1,] = c("00:00-24:00","0-24hr")
          } else { # if qwindow is numeric then store as such to be consistent with other output
            namekey[1,] = c("00:00-24:00","0-24hr")
          }
        } else {
          if (qwindownumeric == FALSE) {
            namekey[gi,] = c(paste0(tms[gi-1], "-", tms[gi]),paste0(nms[gi-1], "-", nms[gi],"hr"))
          } else {
            
            namekey[gi,] = c(paste0(convert2minutes(tms[gi-1]), "-", convert2minutes(tms[gi])), 
                             paste0(nms[gi-1], "-", nms[gi],"hr"))
          }
        }
        qwt_index = which(as.character(df_tmp$timesegment2) == namekey[gi,2])
        if (length(qwt_index) > 0) {
          df_tmp$qwindow_timestamps[qwt_index] = namekey[gi,1]
        } else {
          df_tmp$qwindow_timestamps[qwt_index] = "not_calculated"
        }
      }
      df[which(df$ID == ji & df$calendar_date == hi),c("qwindow_timestamps", "qwindow_names",
                                                       "timesegment2")] = df_tmp
    }
  }
  df = df[, -which(colnames(df) %in% c("qwindow_names"))]
  redundant_rows = which(df$N_valid_hours_in_window == "" & df$N_hours_in_window  == "" & 
                           apply(df[16:ncol(df)],1,FUN=function(x) any(x=="")) &
                           df[,ncol(df)-1] == "" & df[,ncol(df)] == "" | df$qwindow_timestamps =="not_calculated")
  if (length(redundant_rows) > 0) df = df[-redundant_rows,]
  colnames(df)[which(colnames(df) == "timesegment2")] = "qwindow_name"
  if (skip.qwindow_name.conversion == FALSE) {
    if (qwindownumeric == FALSE) {
      df$qwindow_name[which(df$qwindow_name == "0-24hr")] = "daystart-dayendhr"
    }
    df$qwindow_name = substr(df$qwindow_name,1,nchar(df$qwindow_name)-2) # remove last two characters, because they are hr
  }
  return(df)
}
