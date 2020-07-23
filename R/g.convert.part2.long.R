g.convert.part2.long = function(daySUMMARY) {
  
  extractlastpart = function(x) {
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
  
  colnames(daySUMMARY) = gsub(pattern = " ",replacement = "_", x = colnames(daySUMMARY))
  cn = names(daySUMMARY)
  
  tt = sapply(cn, FUN = extractlastpart)
  id.vars = colnames(tt[,which(tt[2,] == "")])
  daySUMMARY = data.table::as.data.table(daySUMMARY)
  daySUMMARY2 = data.table::melt(daySUMMARY, id.vars = id.vars, 
                                 measure.vars = colnames(tt)[which(tt[2,]!= "")])
  getvar = function(x) {
    tmp = unlist(strsplit(as.character(x),"_"))
    return(paste0(tmp[1:(length(tmp)-1)],collapse="_"))
  }
  daySUMMARY2$variable2 = sapply(daySUMMARY2$variable, FUN = getvar)
  gettimeseg = function(x) {
    tmp = unlist(strsplit(as.character(x),"_"))
    return(tmp[length(tmp)])
  }
  daySUMMARY2$timesegment2 = sapply(daySUMMARY2$variable, FUN = gettimeseg)
  daySUMMARY2 = as.data.frame(daySUMMARY2)
  rem = which(colnames(daySUMMARY2) =="variable")
  daySUMMARY2 = daySUMMARY2[,-rem] # It is a data.table object so column removal works different
  id.vars = c(id.vars,"timesegment2")
  cnt = 1
  for (i in unique(daySUMMARY2$variable2)) {
    if (cnt == 1) {
      df = daySUMMARY2[which(daySUMMARY2$variable2 == i),]
      colnames(df)[which(colnames(df) == "value")] = i
      rem = which(colnames(df) =="variable2")
      df = df[,-rem]
    } else {
      df = base::merge(df, daySUMMARY2[which(daySUMMARY2$variable2 == i),], by=id.vars, all.x=T)
      colnames(df)[which(colnames(df) == "value")] = i
      rem = which(colnames(df) =="variable2")
      df = df[,-rem]
    }
    cnt = cnt+1
  }
  Nvh.1 = which(colnames(df) == "N_valid_hours.1")
  Nh.1 = which(colnames(df) == "N_hours.1")
  if (length(Nvh.1) > 0) colnames(df)[Nvh.1] = "N_valid_hours_in_window"
  if (length(Nh.1) > 0) colnames(df)[Nh.1] = "N_hours_in_window"
  df$N_valid_hours_in_window[which(df$timesegment2 == "0-24hr")] = ""
  df$N_hours_in_window[which(df$timesegment2 == "0-24hr")] = ""
  col2move = which(colnames(df) %in% c("N_valid_hours_in_window","N_hours_in_window") == TRUE)
  col2NH = which(colnames(df) %in% c("N_valid_hours","N_hours") == TRUE)
  df = df[,c(1:max(col2NH),col2move,(max(col2NH)+1):(ncol(df)-2))]
  rplc = which(df$qwindow_timestamps == "00:00_24:00")
  if (length(rplc) > 0) {
    df$qwindow_timestamps[rplc] = "00:00-24:00"
  }
  # rename segment names
  for (ji in unique(df$ID)) {
    for (hi in unique(df$calendar_date)) {
      df_tmp = df[which(df$ID == ji & df$calendar_date == hi),c("qwindow_timestamps", "qwindow_names",
                                                                "timesegment2")]
      tms = unlist(strsplit(df_tmp$qwindow_timestamps[1],"_"))
      nms = unlist(strsplit(df_tmp$qwindow_names[1],"_"))
      namekey = matrix("",length(tms),2)
      for (gi in 1:(length(tms))) {
        if (gi == 1) {
          options(warn=-1)
          qwindownumeric = is.na(as.numeric(tms[1]))
          options(warn=0)
          if (qwindownumeric == TRUE) {
            namekey[1,] = c("00:00-24:00","0-24hr")
          } else { # if qwindow is numeric then store as such to be consistent with other output
            namekey[1,] = c("0-24","0-24hr")
          }
        } else {
          namekey[gi,] = c(paste0(tms[gi-1], "-", tms[gi]),paste0(nms[gi-1], "-", nms[gi],"hr"))
        }
        df_tmp$qwindow_timestamps[which(as.character(df_tmp$timesegment2) == namekey[gi,2])] = namekey[gi,1]
      }
      df[which(df$ID == ji & df$calendar_date == hi),c("qwindow_timestamps", "qwindow_names",
                                                       "timesegment2")] = df_tmp
    }
  }
  df = df[, -which(colnames(df) %in% c("qwindow_names"))]
  redundant_rows = which(df$N_valid_hours_in_window == "" & df$N_hours_in_window  == "" & 
          (df$qwindow_timestamps == "00:00-24:00" | df$qwindow_timestamps == "0-24") &
            df[,16] == "" & df[,20] == "" & df[,ncol(df)-2] == "" & 
            df[,ncol(df)-1] == "" & df[,ncol(df)] == "")
  if (length(redundant_rows) > 0) df = df[-redundant_rows,]
  colnames(df)[which(colnames(df) == "timesegment2")] = "qwindow_name"
  return(df)
}