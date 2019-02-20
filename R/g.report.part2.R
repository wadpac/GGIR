g.report.part2 = function(metadatadir=c(),f0=c(),f1=c(),maxdur = 7,selectdaysfile=c()) {
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
    fnames = dir(path)
    if (f1 > length(fnames)) f1 = length(fnames)
    # create output folders
    ms2.out = "/meta/ms2.out"
    fnames.ms2 = dir(paste0(metadatadir,ms2.out))
    
    #---------------------------------
    # house keeping variables
    pdfpagecount = 1 # counter to keep track of files being processed (for pdf)
    pdffilenumb = 1 #counter to keep track of number of pdf-s being generated
    winSUMMARY = daySUMMARY = c()
    if (length(f0) ==  0) f0 = 1
    if (length(f1) ==  0) f1 = length(fnames)
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
      M = c()
      fname2read =paste0(path,fnames[i])
      try(expr={load(fname2read)},silent=TRUE) #reading RData-file
      if (length(M) == 0) {
        cat(paste0("Error in g.report2: Struggling to read : ",fnames[i]))
      }
      if (M$filecorrupt == FALSE & M$filetooshort == FALSE) {
        fname = as.character(unlist(strsplit(fnames[i],"eta_"))[2])
        selp = which(fnames.ms2 == fname)
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
            if (ncol(SUMMARY) == ncol(SUM$summary)) {
            } else {
              if (ncol(SUM$summary) > ncol(SUMMARY)) {
                cat(" Error: Failed to merge output from incompatible analysis. Please 
                    make sure you use a consistent set of parameters within a single analysis.")
              }
              SUM$summary = cbind(SUM$summary[1:(ncol(SUM$summary)-8)],
                                  matrix(" ",1,(ncol(SUMMARY) - ncol(SUM$summary))),
                                  SUM$summary[(ncol(SUM$summary)-7):ncol(SUM$summary)])
              colnames(SUM$summary) = colnames(SUMMARY)
            }
            # daysummary
            if (ncol(daySUMMARY) == ncol(SUM$daysummary)) {
            } else {
              SUM$daysummary = cbind(SUM$daysummary,matrix(" ",1,(ncol(daySUMMARY) - ncol(SUM$daysummary))))
              colnames(SUM$daysummary) = colnames(daySUMMARY)
            }
            if (length(which(colnames(daySUMMARY) != names(SUM$daysummary)) ) > 0) {
              names(SUM$daysummary) =   colnames(daySUMMARY)
            }
            if (length(selectdaysfile) > 0) {
              # winsummary
              winSUMMARY2 = SUM$windowsummary[,which(is.na(colnames(SUM$windowsummary)) == FALSE)]
              if (ncol(winSUMMARY) == ncol(winSUMMARY2)) {
              } else {
                winSUMMARY2 = cbind(winSUMMARY2,matrix(" ",1,(ncol(winSUMMARY) - ncol(winSUMMARY2))))
                colnames(winSUMMARY2) = colnames(winSUMMARY)
              }
              if (length(which(colnames(winSUMMARY) != names(winSUMMARY2)) ) > 0) {
                names(winSUMMARY2) =   colnames(winSUMMARY)
              }
              winSUMMARY = rbind(winSUMMARY,winSUMMARY2)
            }
            SUMMARY = rbind(SUMMARY,SUM$summary)
            daySUMMARY = rbind(daySUMMARY,SUM$daysummary)
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
        SN = hvalues[which(hnames == "Serial_Number")] #serial number
      } else if (mon == "geneactive") {
        SN = hvalues[which(hnames == "Device_Unique_Serial_Code")] #serial number
        if (I$dformn == "csv") { #if it was stored in csv-format then underscores were replaced by spaces (by company)
          SN = hvalues[which(hnames == "Device Unique Serial Code")] #serial number  		
        }
      } else if (mon == "actigraph" | mon == "axivity") { #todo: create automatic extraction of information from actigraph fileheader
        SN = "not extracted" #gender
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
                      device.serial.number=SN,stringsAsFactors=FALSE,NFilePagesSkipped=M$NFilePagesSkipped)
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
    if (length(selectdaysfile) > 0) {
      write.csv(winSUMMARY,paste0(metadatadir,"/results/part2_windowsummary.csv"),row.names=F)
    }
    write.csv(QCout,paste0(metadatadir,"/results/QC/data_quality_report.csv"),row.names=F)
    # this code is now part of g.part4
    # SI = sessionInfo()
    # save(SI,file=paste0(metadatadir,"/results/QC/sessioninfo_part2.RData"))
  }
}