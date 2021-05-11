g.part2 = function(datadir=c(),metadatadir=c(),f0=c(),f1=c(),strategy = 1, hrs.del.start = 0.5,hrs.del.end = 0.5,
                   maxdur = 7, includedaycrit = 16,
                   L5M5window = c(0,24), M5L5res = 10, winhr = 5,
                   qwindow=c(0,24), qlevels = c(0.1),
                   ilevels = c(0,10), mvpathreshold = c(100),
                   boutcriter = 0.8,ndayswindow=7,idloc=1,do.imp=TRUE,storefolderstructure = FALSE,
                   overwrite=FALSE,epochvalues2csv=FALSE,mvpadur=c(1,5,10),selectdaysfile=c(),
                   window.summary.size=10,dayborder=0,bout.metric=2,closedbout=FALSE,desiredtz="",
                   IVIS_windowsize_minutes = 60, IVIS_epochsize_seconds = NA, iglevels = c(),
                   IVIS.activity.metric=2, TimeSegments2ZeroFile=c(), qM5L5  = c(), do.parallel = TRUE,
                   myfun=c(), MX.ig.min.dur=10, maxNcores=c()) {
  snloc= 1
  if (is.numeric(qwindow)) {
    qwindow = qwindow[order(qwindow)]
  } else if (is.character(qwindow)) { 
    qwindow = g.conv.actlog(qwindow)
    # This will be an object with numeric qwindow values for all individuals and days
  }  
  #---------------------------------
  # Specifying directories with meta-data and extracting filenames
  path = paste0(metadatadir,"/meta/basic/")  #values stored per long epoch, e.g. 15 minutes
  fnames = dir(path)
  if (f1 > length(fnames)) f1 = length(fnames)
  # create output folders
  ffdone = c()
  ms2.out = "/meta/ms2.out"
  if (file.exists(paste0(metadatadir,ms2.out)) == FALSE) {
    dir.create(file.path(metadatadir,ms2.out))
  }
  csvfolder = "/meta/csv"
  if (epochvalues2csv==TRUE) {
    if (file.exists(paste(metadatadir,csvfolder,sep="")) == FALSE) {
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
  # get full file path and folder name if requested by end-user and keep this for storage in output
  if (storefolderstructure == TRUE) {
    extractfilenames = function(x) {
      x2 = as.character(unlist(strsplit(x,".RDa"))[1])
      x3 = as.character(unlist(strsplit(x2,"eta_"))[2])
      return(x3)
    }
    referencefnames = sapply(fnames,extractfilenames)
    folderstructure = getfolderstructure(datadir,referencefnames)
    fullfilenames = folderstructure$fullfilenames
    foldername = folderstructure$foldername
  }
  #--------------------------------
  # Loop through all the files
  fnames = sort(fnames)
  if (f1 > length(fnames)) f1 = length(fnames)
  if (f0 > f1) f0 = 1
  
  #---------------------------------------
  cnt78 = 1
  if (do.parallel == TRUE) {
    cores=parallel::detectCores()
    Ncores = cores[1]
    if (Ncores > 3) {
      if (length(maxNcores) == 0) maxNcores = Ncores
      Ncores2use = min(c(Ncores-1, maxNcores))
      cl <- parallel::makeCluster(Ncores2use) #not to overload your computer
      doParallel::registerDoParallel(cl)
    } else {
      cat(paste0("\nparallel processing not possible because number of available cores (",Ncores,") < 4"))
      do.parallel = FALSE
    }
  }
  t1 = Sys.time() # copied here
  if (do.parallel == TRUE) {
    cat(paste0('\n Busy processing ... see ', metadatadir, ms2.out, ' for progress\n'))
  }
  
  # check whether we are indevelopment mode:
  GGIRinstalled = is.element('GGIR', installed.packages()[,1])
  packages2passon = functions2passon = NULL
  GGIRloaded = "GGIR" %in% .packages()
  if (GGIRloaded) { #pass on package
    packages2passon = 'GGIR'
    errhand = 'pass'
  } else { # pass on functions
    functions2passon = c("g.analyse", "g.impute", "g.weardec", "g.detecmidnight",
                         "g.extractheadervars", "g.analyse.avday", "g.getM5L5", "g.IVIS",
                         "g.analyse.perday", "g.getbout", "g.analyse.perfile", "g.intensitygradient",
                         "iso8601chartime2POSIX")
    errhand = 'stop'
  }
  fe_dopar = foreach::`%dopar%`
  fe_do = foreach::`%do%`
  i = 0 # declare i because foreach uses it, without declaring it
  `%myinfix%` = ifelse(do.parallel, fe_dopar, fe_do) # thanks to https://stackoverflow.com/questions/43733271/how-to-switch-programmatically-between-do-and-dopar-in-foreach
  output_list =foreach::foreach(i=f0:f1, .packages = packages2passon,
                                .export=functions2passon, .errorhandling=errhand, .verbose = F) %myinfix% { # the process can take easily 1 minute per file, so probably there is a time gain by doing it parallel
    tryCatchResult = tryCatch({
  # for (i in f0:f1) {
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
      cat(paste0(" ",i))
      M = c()
      filename_dir = c()
      filefoldername = c()
      file2read = paste0(path,fnames[i])
      load(file2read) #reading RData-file
      if (M$filecorrupt == FALSE & M$filetooshort == FALSE) {
        
        #-----------------------
        # If required by user, ignore specific timewindows for imputation and set them to zeroinstead:
        TimeSegments2Zero = c() # set defaul
        # Check whether csv file exists with start-end end times of timewindows to be ignored (where 0 movement will be assumed)
        if (length(TimeSegments2ZeroFile) > 0) {
          TimeSegments2ZeroAll = read.csv(TimeSegments2ZeroFile)
          # Check whether this individual is part of the file
          filei = which(TimeSegments2ZeroAll$filename == as.character(unlist(strsplit(fnames[i],"eta_"))[2]))
          if (length(filei) > 0) {
            # If yes, load the timestamps that indicate the windows to be ignored
            TimeSegments2Zero = TimeSegments2ZeroAll[filei,]
            # Check that they fall withint the measurement
            TimeSegments2Zero$windowstart = as.POSIXlt(TimeSegments2Zero$windowstart,tz=desiredtz)
            TimeSegments2Zero$windowend = as.POSIXlt(TimeSegments2Zero$windowend,tz=desiredtz)
            timespan0 = iso8601chartime2POSIX(M$metashort$timestamp[1], tz= desiredtz)
            timespan1 = iso8601chartime2POSIX(M$metashort$timestamp[nrow(M$metashort)], tz= desiredtz)
            validtimes = which(TimeSegments2Zero$windowstart > timespan0 &
                                 TimeSegments2Zero$windowend < timespan1)
            # validtimes = which(as.POSIXlt(TimeSegments2Zero$windowstart,tz=desiredtz) > timespan0 &
            #                      as.POSIXlt(TimeSegments2Zero$windowend,tz=desiredtz) < timespan1)
            if (length(validtimes) > 0) {
              TimeSegments2Zero = TimeSegments2Zero[validtimes,c("windowstart","windowend")]
              
            } else {
              TimeSegments2Zero = c()
            }
          }
        }
        #------------
        if (length(myfun) > 0) {
          if (myfun$outputtype == "character") {
            # At the moment we do not have a strategy in place on how to impute categorical variables
            # produced by external functions. Therefore, for the moment ignore these variables until
            # there is a plan.
            M$metashort = M$metashort[,-which(names(M$metashort) %in% myfun$colnames == TRUE)]
          }
        }
        IMP = g.impute(M,I,strategy=strategy,hrs.del.start=hrs.del.start,
                       hrs.del.end=hrs.del.end,maxdur=maxdur,ndayswindow = ndayswindow,desiredtz=desiredtz, TimeSegments2Zero = TimeSegments2Zero)
        if (do.imp==FALSE) { #for those interested in sensisitivity analysis
          IMP$metashort = M$metashort
          IMP$metalong = M$metalong
        }
        SUM = g.analyse(I,C,M,IMP,qlevels=qlevels,qwindow=qwindow,L5M5window=L5M5window,M5L5res=M5L5res,
                        includedaycrit=includedaycrit,ilevels=ilevels,winhr=winhr,idloc=idloc,
                        mvpathreshold =mvpathreshold ,boutcriter=boutcriter,mvpadur=mvpadur,selectdaysfile=selectdaysfile,
                        window.summary.size=window.summary.size,dayborder=dayborder,bout.metric=bout.metric,closedbout=closedbout,
                        desiredtz=desiredtz,IVIS_windowsize_minutes = IVIS_windowsize_minutes,
                        IVIS_epochsize_seconds = IVIS_epochsize_seconds, iglevels = iglevels,
                        IVIS.activity.metric= IVIS.activity.metric, qM5L5  = qM5L5, myfun=myfun, MX.ig.min.dur=MX.ig.min.dur)
        name=as.character(unlist(strsplit(fnames[i],"eta_"))[2])
        if (epochvalues2csv==TRUE) {
          if (length(IMP$metashort) > 0) {
            write.csv(IMP$metashort,paste0(metadatadir,"/",csvfolder,"/",name,".csv"),row.names=FALSE)
          }
        }
        if (M$filecorrupt == FALSE & M$filetooshort == FALSE) {
          if (cnt78 == 1) {
            SUMMARY = SUM$summary
            daySUMMARY = SUM$daysummary
            if (length(selectdaysfile) > 0) {
              winSUMMARY = SUM$windowsummary[,which(
                is.na(colnames(SUM$windowsummary)) == FALSE)] # added for Millenium cohort
            }
            cnt78 = 2
          } else {
            if (length(selectdaysfile) > 0) {
              # windowsummary
              winSUMMARY2 = SUM$windowsummary[,which(is.na(colnames(SUM$windowsummary)) == FALSE)]
              if (ncol(winSUMMARY) != ncol(winSUMMARY2)) {
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
        if (length(unlist(strsplit(name,"[.]RD"))) == 1) { # to avoid getting .RData.RData
          filename = paste0(name,".RData")
        }
        if (storefolderstructure == TRUE) { # newly added 20-2-2019
          SUM$daysummary$filename_dir = fullfilenames[i] #full filename structure
          SUM$daysummary$foldername = foldername[i] #store the lowest foldername
        }
        save(SUM,IMP,file=paste0(metadatadir,ms2.out,"/",name)) #IMP is needed for g.plot in g.report.part2
      }
      if (M$filecorrupt == FALSE & M$filetooshort == FALSE) rm(IMP)
      rm(M); rm(I)
    }
  # } # end for loopp
    }) # END tryCatch
    return(tryCatchResult)
  }
  if (do.parallel == TRUE) {
    on.exit(parallel::stopCluster(cl))
  }
  for (oli in 1:length(output_list)) { # logged error and warning messages
    if (is.null(unlist(output_list[oli])) == FALSE) {
      cat(paste0("\nErrors and warnings for ",fnames[oli]))
      print(unlist(output_list[oli])) # print any error and warnings observed
    }
  }
}
