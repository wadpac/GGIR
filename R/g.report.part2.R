g.report.part2 = function(metadatadir=c(),f0=c(),f1=c(),maxdur = 7,selectdaysfile=c(), store.long=FALSE) {
  ms2.out = "/meta/ms2.out"
  if (file.exists(paste0(metadatadir,ms2.out))) {
    if (length(dir(paste0(metadatadir,ms2.out))) == 0) {
      try.generate.report = FALSE
    } else {
      try.generate.report = TRUE
    }
  } else {
    try.generate.report = FALSE
  }
  if (try.generate.report == TRUE) {
    if (maxdur != 0) {
      durplot = maxdur
    } else {
      durplot = 7 #how many DAYS to plot? (only used if maxdur is not specified as a number above zero)
    }
    #---------------------------------
    # Specifying directories with meta-data and extracting filenames
    path = paste0(metadatadir,"/meta/basic/")  #values stored per long epoch, e.g. 15 minutes
    fnames = dir(path) # part 1
    # ms2.out = "/meta/ms2.out"
    fnames.ms2 = dir(paste0(metadatadir,ms2.out))  #part 2
    #---------------------------------
    # house keeping variables
    pdfpagecount = 1 # counter to keep track of files being processed (for pdf)
    pdffilenumb = 1 #counter to keep track of number of pdf-s being generated
    winSUMMARY = daySUMMARY = c()
    if (length(f0) ==  0) f0 = 1
    if (length(f1) ==  0) f1 = length(fnames)
    if (f1 > length(fnames)) f1 = length(fnames)
    #-----------------------------
    # Loop through all the files
    for (i in f0:f1) {
      cat(paste0(" ",i))
      if (pdfpagecount == 301) { # generate new pdf for every 300 plots
        pdfpagecount = 1
        pdffilenumb = pdffilenumb + 1
        dev.off()
      }
      if (pdfpagecount == 1) {
        pdf(paste0(metadatadir,"/results/QC/plots_to_check_data_quality_",pdffilenumb,".pdf"),width=7,height=7)
      }
      # First load part 1 data
      M = c()
      fname2read =paste0(path,fnames[i])
      try(expr={load(fname2read)},silent=TRUE) #reading RData-file
      if (length(M) == 0) {
        cat(paste0("Error in g.report2: Struggling to read: ",fname2read)) #fnames[i]
      }
      fname = as.character(unlist(strsplit(fnames[i],"eta_"))[2])
      selp = which(fnames.ms2 == fname)
      if(length(selp) == 0 ) {
        cat(paste0("File ",fname," not available in part 2"))
      }
      if (M$filecorrupt == FALSE & M$filetooshort == FALSE & length(selp) > 0) { #If part 1 milestone data indicates that file was useful
        # Load part 2 data
        IMP=c()
        fname2read = paste0(metadatadir,ms2.out,"/",fnames.ms2[selp])
        try(expr={load(file=fname2read)},silent=TRUE)
        if (length(IMP) == 0) {
          cat(paste0("Error in g.report2: Struggling to read: ",fname2read))
        }
        Q = g.plot(IMP,M,I,durplot)
        if (M$filecorrupt == FALSE & M$filetooshort == FALSE) {
          if (i == 1 | i == f0) {
            SUMMARY = SUM$summary
            SUMMARY$pdffilenumb = pdffilenumb
            SUMMARY$pdfpagecount = pdfpagecount
            SUM$summary = SUMMARY
            daySUMMARY = SUM$daysummary
            if (length(selectdaysfile) > 0) {
              winSUMMARY = SUM$windowsummary[,which(
                is.na(colnames(SUM$windowsummary)) == FALSE)] # added for Millenium cohort
            }
          } else {
            SUM$summary$pdffilenumb = pdffilenumb
            SUM$summary$pdfpagecount = pdfpagecount
            bind_with_prev_data = function(df1, df2) {
              df1 = data.table::rbindlist(list(df1, df2), fill=TRUE)
              df1 = as.data.frame(df1)
              # replace factors by character value
              i <- sapply(df1, is.factor)
              df1[,i] <- lapply(df1[,i], as.character)
              # replace all NA values by blank
              df1[is.na(df1)] <- ""
              if (length(which(df1 == "NaN")) > 0) {
                df1[df1=="NaN"] = ""
              }
              return(df1)
            }
            SUMMARY = bind_with_prev_data(SUMMARY, SUM$summary)
            daySUMMARY = bind_with_prev_data(daySUMMARY, SUM$daysummary)
            if (length(selectdaysfile) > 0) {
              # winsummary
              winSUMMARY2 = SUM$windowsummary[,which(is.na(colnames(SUM$windowsummary)) == FALSE)]
              winSUMMARY = bind_with_prev_data(winSUMMARY, SUM$winsummary)
            }
          }
        }
      }
      #-----------------
      # create data quality report
      if (length(C$cal.error.end) == 0) C$cal.error.end = " "
      if (M$filetooshort == TRUE | M$filecorrupt == TRUE) {
        C$cal.error.start = 0
        C$npoints = 0
      }
      tm = which(colnames(M$metalong) == "temperaturemean")
      if (length(tm) > 0) {
        tmean = as.character(mean(as.numeric(as.matrix(M$metalong[1:(nrow(M$metalong)-1),tm]))))
      } else {
        tmean= ""
      }
      #=========
      header= I$header
      mon = I$monn
      hnames = rownames(header)
      hvalues = as.character(as.matrix(header))
      pp = which(hvalues == "")
      hvalues[pp] = c("not stored in header")
      if (mon == "genea") {
        deviceSerialNumber = hvalues[which(hnames == "Serial_Number")] #serial number
      } else if (mon == "geneactive") {
        deviceSerialNumber = hvalues[which(hnames == "Device_Unique_Serial_Code")] #serial number
        if (I$dformn == "csv") { #if it was stored in csv-format then underscores were replaced by spaces (by company)
          deviceSerialNumber = hvalues[which(hnames == "Device Unique Serial Code")] #serial number
        }
      } else if (mon == "actigraph" | mon == "axivity") { #todo: create automatic extraction of information from actigraph fileheader
        deviceSerialNumber = "not extracted"
      } else if (I$monc == 5) { #todo: create automatic extraction of information from monc fileheader
        deviceSerialNumber = "not extracted"
      }
      if (length(C$offset) == 0) {
        C$offset = C$translate
      }
      if (length(C$cal.error.start) == 0) {
        C$cal.error.start = " "
      }
      if (length(C$npoints) == 0) {
        C$npoints = " "
      }
      if (length(M$NFilePagesSkipped) == 0) M$NFilePagesSkipped = 0 # to make the code work for historical part1 output.
      QC = data.frame(filename=fnames[i],
                      file.corrupt=M$filecorrupt,
                      file.too.short=M$filetooshort,
                      use.temperature=C$use.temp,
                      scale.x=C$scale[1], scale.y=C$scale[2], scale.z=C$scale[3],
                      offset.x=C$offset[1], offset.y=C$offset[2], offset.z=C$offset[3],
                      temperature.offset.x=C$tempoffset[1],  temperature.offset.y=C$tempoffset[2],
                      temperature.offset.z=C$tempoffset[3],
                      cal.error.start=C$cal.error.start,
                      cal.error.end=C$cal.error.end,
                      n.10sec.windows=C$npoints,
                      n.hours.considered = C$nhoursused, QCmessage=C$QCmessage,mean.temp=tmean,
                      device.serial.number=deviceSerialNumber,stringsAsFactors=FALSE,NFilePagesSkipped=M$NFilePagesSkipped)
      if (i == 1 | i == f0) {
        QCout = QC
      } else {
        if (ncol(QCout) == ncol(QC)) {
        } else {
          QC = cbind(QC,matrix(" ",1,(ncol(QCout) - ncol(QC))))
          colnames(QC) = colnames(QCout)
        }
        QCout = rbind(QCout,QC)
      }
      #---------------------------------------------------------------
      if (pdfpagecount == 100 | pdfpagecount == 200 | pdfpagecount == 300) {
        #store matrix temporarily to keep track of process
        write.csv(SUMMARY,paste0(metadatadir,"/results/part2_summary.csv"),row.names=F)
        write.csv(daySUMMARY,paste0(metadatadir,"/results/part2_daysummary.csv"),row.names=F)
        if (length(selectdaysfile) > 0) {
          write.csv(winSUMMARY,paste0(metadatadir,"/results/part2_windowsummary.csv"),row.names=F)
        }
        write.csv(QCout,paste0(metadatadir,"/results/QC/data_quality_report.csv"),row.names=F)
      }
      pdfpagecount = pdfpagecount + 1
    }
    if (M$filecorrupt == FALSE & M$filetooshort == FALSE) rm(IMP)
    rm(M); rm(I)
    dev.off()
    #===============================================================================
    #now delete rows with incorrect participants
    #----------------------------------------------------
    # get original folder structure and assess to what phase each file belonged
    # store final matrices again
    write.csv(SUMMARY,paste0(metadatadir,"/results/part2_summary.csv"),row.names=F)
    write.csv(daySUMMARY,paste0(metadatadir,"/results/part2_daysummary.csv"),row.names=F)
    
    # Optionally convert daySUMMARY to long format
    if (store.long == TRUE) {
      dev.mode = FALSE
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
      if (dev.mode == TRUE) {
        cn =c("test_2-34","test_56-7","othervar",
              "exp_2-34","exp_56-7","othervar2")
        daySUMMARY = data.frame(a = runif(n = 10,min = 1,max = 10), b = runif(n = 10,min = 1,max = 10),
                                c = runif(n = 10,min = 100,max = 110), d = runif(n = 10,min = 1,max = 10),
                                d = runif(n = 10,min = 1,max = 10), e = runif(n = 10,min = 200,max = 210))
        colnames(daySUMMARY) = cn
        daySUMMARY$ID =rep(c(1:2),each=5)
        daySUMMARY$day =rep(c(1:5),times=2)
        daySUMMARY = daySUMMARY[,c("ID","day",cn)]
        cn = colnames(daySUMMARY)
      } else {
        colnames(daySUMMARY) = gsub(pattern = " ",replacement = "_", x = colnames(daySUMMARY))
        cn = names(daySUMMARY)
      }
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
              namekey[1,] = c("00:00-00:24","0-24hr")
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
      colnames(df)[which(colnames(df) == "timesegment2")] = "qwindow_name"
      write.csv(df,paste0(metadatadir,"/results/part2_daysummary_long.csv"),row.names=F)
    }
    
    #---------------
    if (length(selectdaysfile) > 0) {
      write.csv(winSUMMARY,paste0(metadatadir,"/results/part2_windowsummary.csv"),row.names=F)
    }
    write.csv(QCout,paste0(metadatadir,"/results/QC/data_quality_report.csv"),row.names=F)
  }
}
