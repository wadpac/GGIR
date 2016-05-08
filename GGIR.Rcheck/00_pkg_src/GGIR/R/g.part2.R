g.part2 = function(datadir=c(),metadatadir=c(),f0=c(),f1=c(),strategy = 1, hrs.del.start = 0.5,hrs.del.end = 0.5,
                   maxdur = 7, includedaycrit = 16,
                   L5M5window = c(0,24), M5L5res = 10, winhr = 5,
                   qwindow=c(0,24), qlevels = c(0.1),
                   ilevels = c(0,10), mvpathreshold = c(100),
                   boutcriter = 0.8,ndayswindow=7,idloc=1,do.imp=TRUE,storefolderstructure = FALSE,
                   overwrite=FALSE,epochvalues2csv=FALSE,mvpadur=c(1,5,10),selectdaysfile=c(),
                   window.summary.size=10,dayborder=0,mvpa.2014=FALSE,closedbout=FALSE) {
  # verify whether path1 is a directory or a list of files
  outputfolder = unlist(strsplit(metadatadir,"/output_"))[2]
  outputfolder = paste("/output_",outputfolder,sep="")
  path1 = unlist(strsplit(metadatadir,"/output"))[1]
  snloc= 1
  #---------------------------------
  # Specifying directories with meta-data and extracting filenames 
  path = paste(path1,outputfolder,"/meta/basic/",sep="")  #values stored per long epoch, e.g. 15 minutes
  fnames = dir(path)
  if (f1 > length(fnames)) f1 = length(fnames)
  # create output folders
  ffdone = c()
  ms2.out = "/meta/ms2.out"
  if (file.exists(paste(metadatadir,ms2.out,sep=""))) {
  } else {
    dir.create(file.path(metadatadir,ms2.out))
  }
  
  csvfolder = "/meta/csv"
  if (epochvalues2csv==TRUE) {
    if (file.exists(paste(metadatadir,csvfolder,sep=""))) {
    } else {
      dir.create(file.path(metadatadir,csvfolder))
    }
  }
  fnames.ms2 = dir(paste(metadatadir,ms2.out,sep=""))
  ffdone = fnames.ms2
  #---------------------------------
  # house keeping variables
  pdfpagecount = 1 # counter to keep track of files being processed (for pdf)
  pdffilenumb = 1 #counter to keep track of number of pdf-s being generated
  daySUMMARY = c()
  if (length(f0) ==  0) f0 = 1
  if (length(f1) ==  0) f1 = length(fnames)
  #--------------------------------
  # Loop through all the files
  fnames = sort(fnames)
  
  #---------------------------------------
  cnt78 = 1
  for (i in f0:f1) {
    if (length(ffdone) > 0) {
      if (length(which(ffdone == as.character(unlist(strsplit(fnames[i],"eta_"))[2]))) > 0) { 
        skip = 1 #skip this file because it was analysed before")
      } else {
        skip = 0 #do not skip this file
      }
    } else {
      skip = 0
    }
    if (overwrite == TRUE) skip = 0
    if (skip ==0) {
      cat(paste(" ",i,sep=""))
      M = c()
      filename_dir = c()
      filefoldername = c()
      load(paste(path,fnames[i],sep="")) #reading RData-file
      if (M$filecorrupt == FALSE & M$filetooshort == FALSE) {
        IMP = g.impute(M,I,strategy=strategy,hrs.del.start=hrs.del.start,
                       hrs.del.end=hrs.del.end,maxdur=maxdur,ndayswindow = ndayswindow)
        if (do.imp==FALSE) { #for those interested in sensisitivity analysis
          IMP$metashort = M$metashort
          IMP$metalong = M$metalong
        }
        SUM = g.analyse(I,C,M,IMP,qlevels=qlevels,qwindow=qwindow,L5M5window=L5M5window,M5L5res=M5L5res,
                        includedaycrit=includedaycrit,ilevels=ilevels,winhr=winhr,idloc=idloc,
                        mvpathreshold =mvpathreshold ,boutcriter=boutcriter,mvpadur=mvpadur,selectdaysfile=selectdaysfile,
                        window.summary.size=window.summary.size,dayborder=dayborder,mvpa.2014=mvpa.2014,closedbout=closedbout)
        
        if (storefolderstructure == TRUE) {
          SUMMARY = SUM$summary
#           SUMMARY$pdffilenumb = pdffilenumb
#           SUMMARY$pdfpagecount = pdfpagecount
          SUM$summary = SUMMARY
        }
        name=as.character(unlist(strsplit(fnames[i],"eta_"))[2])
        if (epochvalues2csv==TRUE) {
          if (length(IMP$metashort) > 0) {
            write.csv(IMP$metashort,paste(metadatadir,"/",csvfolder,"/",name,".csv",sep=""),row.names=FALSE)
          } 
        }
        if (M$filecorrupt == FALSE & M$filetooshort == FALSE) {
          if (i == 1 | i == f0 | cnt78 == 1) {
            SUMMARY = SUM$summary
#             SUMMARY$pdffilenumb = pdffilenumb
#             SUMMARY$pdfpagecount = pdfpagecount
            SUM$summary = SUMMARY
            daySUMMARY = SUM$daysummary
            if (length(selectdaysfile) > 0) {
              winSUMMARY = SUM$windowsummary[,which(
                is.na(colnames(SUM$windowsummary)) == FALSE)] # added for Millenium cohort
            }
            cnt78 = 2
          } else {
            SUMMARY = SUM$summary
#             SUMMARY$pdffilenumb = pdffilenumb
#             SUMMARY$pdfpagecount = pdfpagecount
            SUM$summary = SUMMARY
            
            
            if (ncol(SUMMARY) == ncol(SUM$summary)) {
            } else {
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
              # windowsummary
              winSUMMARY2 = SUM$windowsummary[,which(is.na(colnames(SUM$windowsummary)) == FALSE)]
              if (ncol(winSUMMARY) == ncol(winSUMMARY2)) {
              } else {
                winSUMMARY2 = cbind(winSUMMARY2,matrix(" ",1,(ncol(winSUMMARY) - ncol(winSUMMARY2))))
                colnames(winSUMMARY2) = colnames(winSUMMARY)
              }
              if (length(which(colnames(winSUMMARY) != names(winSUMMARY2)) ) > 0) {
                names(winSUMMARY2) =   colnames(winSUMMARY)
              }
              if (length(which(colnames(daySUMMARY) != names(SUM$daysummary)) ) > 0) {
              names(SUM$windowsummary) =   colnames(winSUMMARY2)
              }
            }
            
          }
        }
        save(SUM,IMP,file=paste(metadatadir,ms2.out,"/",name,sep="")) #IMP is needed for g.plot in g.report.part2
      }
      if (M$filecorrupt == FALSE & M$filetooshort == FALSE) rm(IMP)
      
      rm(M); rm(I)
    }
  }
  SI = sessionInfo()  
  save(SI,file=paste(path1,outputfolder,"/results/QC/sessioninfo_part2.RData",sep=""))
}