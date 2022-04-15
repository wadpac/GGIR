g.report.part2 = function(metadatadir = c(), f0 = c(), f1 = c(), maxdur = 0,
                          selectdaysfile = c(), store.long = FALSE) {
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
        pdf(paste0(metadatadir, "/results/QC/plots_to_check_data_quality_", pdffilenumb, ".pdf"),
            width = 7, height = 7)
      }
      # First load part 1 data
      M = c()
      fname2read = paste0(path, fnames[i])
      try(expr = {load(fname2read)}, silent = TRUE) #reading RData-file
      if (length(M) == 0) {
        cat(paste0("Error in g.report2: Struggling to read: ",fname2read)) #fnames[i]
      }
      fname = as.character(unlist(strsplit(fnames[i],"eta_"))[2])
      selp = which(fnames.ms2 == fname)
      if (length(selp) == 0 ) {
        cat(paste0("File ",fname," not available in part 2"))
      }
      if (M$filecorrupt == FALSE & M$filetooshort == FALSE & length(selp) > 0) { #If part 1 milestone data indicates that file was useful
        # Load part 2 data
        IMP = c()
        fname2read = paste0(metadatadir,ms2.out,"/",fnames.ms2[selp])
        try(expr = {load(file = fname2read)}, silent = TRUE)
        if (length(IMP) == 0) {
          cat(paste0("Error in g.report2: Struggling to read: ",fname2read))
        }
        Ndays = (nrow(M$metalong) * M$windowsizes[2]) / (3600 * 24)
        g.plot(IMP, M, I, durplot = ifelse(test = Ndays > durplot, yes = Ndays, no = durplot))
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
      if (length(SUMMARY) == 0 |length(daySUMMARY) == 0) {
        warning("No summary data available to be stored in csv-reports")
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
        tmean = as.character(mean(as.numeric(as.matrix(M$metalong[1:(nrow(M$metalong) - 1),tm]))))
      } else {
        tmean = ""
      }
      #=========
      header = I$header
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
      } else if (mon == "actigraph" | mon == "axivity" | mon == "verisense") {
        deviceSerialNumber = "not extracted"
      } else if (I$monc == 5) { #movisense
        deviceSerialNumber = "not extracted"
      } else if (I$monc == 0) {
        if (header != "no header") {
          deviceSerialNumber = hvalues[which(hnames == "device_serial_number")]
          if (length(deviceSerialNumber) == 0) {
            deviceSerialNumber = "not extracted"
          }
        } else {
          deviceSerialNumber = "not extracted"
        }
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
      if (length(C$tempoffset) == 0) {
        C$tempoffset = c(0, 0, 0)
      }
      if (length(M$NFilePagesSkipped) == 0) M$NFilePagesSkipped = 0 # to make the code work for historical part1 output.
      QC = data.frame(filename = fnames[i],
                      file.corrupt = M$filecorrupt,
                      file.too.short = M$filetooshort,
                      use.temperature = C$use.temp,
                      scale.x = C$scale[1], scale.y = C$scale[2], scale.z = C$scale[3],
                      offset.x = C$offset[1], offset.y = C$offset[2], offset.z = C$offset[3],
                      temperature.offset.x = C$tempoffset[1],  temperature.offset.y = C$tempoffset[2],
                      temperature.offset.z = C$tempoffset[3],
                      cal.error.start = C$cal.error.start,
                      cal.error.end = C$cal.error.end,
                      n.10sec.windows = C$npoints,
                      n.hours.considered = C$nhoursused, QCmessage = C$QCmessage, mean.temp = tmean,
                      device.serial.number = deviceSerialNumber,
                      stringsAsFactors = FALSE, NFilePagesSkipped = M$NFilePagesSkipped)
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
        SUMMARY_clean = tidyup_df(SUMMARY)
        daySUMMARY_clean = tidyup_df(daySUMMARY)
        if (length(selectdaysfile) > 0) {
          winSUMMARY_clean = tidyup_df(winSUMMARY)
        }  
        #store matrix temporarily to keep track of process
        write.csv(x = SUMMARY_clean, file = paste0(metadatadir, "/results/part2_summary.csv"), row.names = F)
        write.csv(x = daySUMMARY_clean, file = paste0(metadatadir, "/results/part2_daysummary.csv"), row.names = F)
        if (length(selectdaysfile) > 0) {
          write.csv(x = winSUMMARY_clean, file = paste0(metadatadir, "/results/part2_windowsummary.csv"), row.names = F)
        }
        write.csv(x = QCout, file = paste0(metadatadir, "/results/QC/data_quality_report.csv"), row.names = F)
      }
      pdfpagecount = pdfpagecount + 1
    }
    if (M$filecorrupt == FALSE & M$filetooshort == FALSE) rm(IMP)
    rm(M); rm(I)
    dev.off()
    
    # tidy up data.frames
    SUMMARY_clean = tidyup_df(SUMMARY)
    daySUMMARY_clean = tidyup_df(daySUMMARY)
    #===============================================================================
    # store final matrices again
    write.csv(x = SUMMARY_clean, file = paste0(metadatadir, "/results/part2_summary.csv"), row.names = F)
    write.csv(x = daySUMMARY_clean, paste0(metadatadir, "/results/part2_daysummary.csv"), row.names = F)
    if (store.long == TRUE) { # Convert daySUMMARY to long format if there are multiple segments per day
      df = g.convert.part2.long(daySUMMARY)
      df_clean = tidyup_df(df)
      write.csv(x = df_clean, file = paste0(metadatadir, "/results/part2_daysummary_longformat.csv"), row.names = F)
    }
    if (length(selectdaysfile) > 0) {
      winSUMMARY_clean = tidyup_df(winSUMMARY)
      write.csv(x = winSUMMARY_clean, file = paste0(metadatadir, "/results/part2_windowsummary.csv"), row.names = F)
    }
    write.csv(x = QCout, file = paste0(metadatadir, "/results/QC/data_quality_report.csv"), row.names = F)
  }
}
