g.part1 = function(datadir=c(),outputdir=c(),f0=1,f1=c(),windowsizes = c(5,900,3600),
                   desiredtz = "",chunksize=c(),studyname=c(),
                   do.enmo = TRUE,do.lfenmo = FALSE,do.en = FALSE,
                   do.bfen = FALSE,do.hfen=FALSE,do.hfenplus = FALSE, do.mad = FALSE,
                   do.anglex=FALSE,do.angley=FALSE,do.anglez=FALSE,
                   do.enmoa = FALSE,
                   do.roll_med_acc_x=FALSE,do.roll_med_acc_y=FALSE,do.roll_med_acc_z=FALSE,
                   do.dev_roll_med_acc_x=FALSE,do.dev_roll_med_acc_y=FALSE,do.dev_roll_med_acc_z=FALSE,
                   do.lfen = FALSE, do.lfx=FALSE, do.lfy=FALSE, do.lfz=FALSE, 
                   do.hfx=FALSE, do.hfy=FALSE, do.hfz=FALSE, 
                   do.bfx=FALSE, do.bfy=FALSE, do.bfz=FALSE, 
                   do.cal = TRUE,
                   lb = 0.2, hb = 15,  n = 4, spherecrit=0.3,
                   minloadcrit=72,printsummary=TRUE,print.filename=FALSE,overwrite=FALSE,
                   backup.cal.coef="retrieve",selectdaysfile=c(),dayborder=0,dynrange=c(),
                   configtz = c(), do.parallel = TRUE, minimumFileSizeMB = 2,myfun=c(),...) {
  #get input variables (relevant when read.myacc.csv is used
  input = list(...)
  if (length(input) > 0) {
    for (i in 1:length(names(input))) {
      txt = paste(names(input)[i],"=",input[i],sep="")
      if (class(unlist(input[i])) == "character") {
        txt = paste(names(input)[i],"='",unlist(input[i]),"'",sep="")
      }
      eval(parse(text=txt))
    }
  }
  if (length(which(ls() == "rmc.dec")) == 0) rmc.dec="."
  if (length(which(ls() == "rmc.firstrow.acc")) == 0) rmc.firstrow.acc = c()
  if (length(which(ls() == "rmc.firstrow.header")) == 0) rmc.firstrow.header=c()
  if (length(which(ls() == "rmc.header.length")) == 0)  rmc.header.length= c()
  if (length(which(ls() == "rmc.col.acc")) == 0) rmc.col.acc = 1:3
  if (length(which(ls() == "rmc.col.temp")) == 0) rmc.col.temp = c()
  if (length(which(ls() == "rmc.col.time")) == 0) rmc.col.time=c()
  if (length(which(ls() == "rmc.unit.acc")) == 0) rmc.unit.acc = "g"
  if (length(which(ls() == "rmc.unit.temp")) == 0) rmc.unit.temp = "C"
  if (length(which(ls() == "rmc.unit.time")) == 0) rmc.unit.time = "POSIX"
  if (length(which(ls() == "rmc.format.time")) == 0) rmc.format.time = "%Y-%m-%d %H:%M:%OS"
  if (length(which(ls() == "rmc.bitrate")) == 0) rmc.bitrate = c()
  if (length(which(ls() == "rmc.dynamic_range")) == 0) rmc.dynamic_range = c()
  if (length(which(ls() == "rmc.unsignedbit")) == 0) rmc.unsignedbit = TRUE
  if (length(which(ls() == "rmc.origin")) == 0) rmc.origin = "1970-01-01"
  if (length(which(ls() == "rmc.desiredtz")) == 0) rmc.desiredtz= "Europe/London"
  if (length(which(ls() == "rmc.sf")) == 0) rmc.sf  = c()
  if (length(which(ls() == "rmc.headername.sf")) == 0) rmc.headername.sf = c()
  if (length(which(ls() == "rmc.headername.sn")) == 0) rmc.headername.sn = c()
  if (length(which(ls() == "rmc.headername.recordingid")) == 0) rmc.headername.recordingid = c()
  if (length(which(ls() == "rmc.header.structure")) == 0) rmc.header.structure = c()
  if (length(which(ls() == "rmc.check4timegaps")) == 0) rmc.check4timegaps = FALSE
  if (length(which(ls() == "rmc.noise")) == 0) rmc.noise = c()
  if (length(which(ls() == "rmc.col.wear")) == 0) rmc.col.wear = c()
  if (length(which(ls() == "rmc.doresample")) == 0) rmc.doresample = FALSE

  if (length(datadir) == 0 | length(outputdir) == 0) {
    if (length(datadir) == 0) {
      stop('\nVariable datadir is not defined')
    }
    if (length(outputdir) == 0) {
      stop('\nVariable outputdir is not specified')
    }
  }
  if (grepl(datadir, outputdir)) {
    stop('\nError: The file path specified by argument outputdir should NOT equal or be a subdirectory of the path specified by argument datadir')
  }
  if (f1 == 0) cat("\nWarning: f1 = 0 is not a meaningful value")
  filelist = isfilelist(datadir)
  if (filelist == FALSE) if (dir.exists(datadir) == FALSE) stop("\nDirectory specified by argument datadir, does not exist")

  #Extra code to handle raw accelerometer data in Raw data format:
  # list of all csv and bin files
  fnames = datadir2fnames(datadir,filelist)
  # check whether these are RDA
  if (length(unlist(strsplit(fnames[1],"[.]RD"))) > 1) {
    useRDA = TRUE
  } else {
    useRDA = FALSE
  }
  # check whether these are movisens files
  is.mv = ismovisens(datadir)
  # create output directory if it does not exist
  if (filelist == TRUE | useRDA == TRUE) {
    if (length(studyname) == 0) {
      studyname = "mystudy"
      stop('\nError: studyname not specified in part1. Needed for analysing lists of files')
    } else {
      outputfolder = paste("/output_",studyname,sep="")
    }
  } else {
    outputfolder = unlist(strsplit(datadir,"/"))
    outputfolder = paste("/output_",outputfolder[length(outputfolder)],sep="")
  }
  if (file.exists(paste(outputdir,outputfolder,sep=""))) {
  } else {
    dir.create(file.path(outputdir,outputfolder))
    dir.create(file.path(outputdir,outputfolder,"meta"))
    dir.create(file.path(outputdir,paste(outputfolder,"/meta",sep=""),"basic"))
    if (length(selectdaysfile) > 0) {
      dir.create(file.path(outputdir,paste(outputfolder,"/meta",sep=""),"raw"))
    }
    dir.create(file.path(outputdir,outputfolder,"results"))
    dir.create(file.path(outputdir,paste(outputfolder,"/results",sep=""),"QC"))
  }
  path3 = paste(outputdir,outputfolder,sep="") #where is output stored?
  use.temp = TRUE;
  daylimit = FALSE

  #=================================================================
  # Other parameters:
  #--------------------------------
  if (filelist == FALSE) {
    fnamesfull = c(dir(datadir,recursive=TRUE,pattern="[.]csv", full.names = TRUE),
                   dir(datadir,recursive=TRUE,pattern="[.]bin", full.names = TRUE),
                   dir(datadir,recursive=TRUE,pattern="[.]wav", full.names = TRUE),
                   dir(datadir,recursive=TRUE,pattern="[.]cwa", full.names = TRUE))
    if(is.mv == TRUE) {
      fnamesfull = dir(datadir,recursive=TRUE,pattern="acc.bin", full.names = TRUE)
      fnames = dir(datadir, recursive = FALSE)
      }
  } else {
    fnamesfull = datadir
  }
  if (useRDA == FALSE) {
    filesizes = file.info(fnamesfull)$size # in bytes
    toosmall = which(filesizes/1e6 > minimumFileSizeMB)
    fnamesfull = fnamesfull[toosmall]
    fnames = fnames[toosmall]
  }
  if (length(dir(datadir,recursive=TRUE,pattern="[.]gt3")) > 0) {
    warning(paste0("\nA .gt3x file was found in directory specified by datadir, ",
                   "at the moment GGIR is not able to process this file format.",
                   "Please convert to csv format with ActiLife software."))
  }
  # check access permissions
  Nfile_without_readpermission = length(which(file.access(paste0(fnamesfull), mode = 4) == -1)) #datadir,"/",
  if (Nfile_without_readpermission > 0) {
    cat("\nChecking that user has read access permission for all files in data directory: No")
    warning(paste0("\nThere are/is ",Nfile_without_readpermission,
                   " file(s) in directory specified with argument datadir for which the user does not have read access permission"))
  } else {
    cat("\nChecking that user has read access permission for all files in data directory: Yes")
  }
  if (file.access(outputdir, mode = 2) == 0) {
    cat("\nChecking that user has write access permission for directory specified by argument outputdir: Yes\n")
  } else {
    stop("\nUser does not seem to have write access permissions for the directory specified by argument outputdir.\n")
  }
  f16 = function(X) {
    out = unlist(strsplit(X,"/"))
    f16 = out[length(out)]
  }
  f17 = function(X) {
    out = unlist(strsplit(X,"/"))
    f17 = out[(length(out)-1)]
  }
  tmp5 = tmp6 = rep(0,length(fnamesfull))
  if (length(fnamesfull) > 0) {
    if(is.mv == FALSE) {
      fnamesshort = apply(X=as.matrix(fnamesfull),MARGIN=1,FUN=f16)
      phase = apply(X=as.matrix(fnamesfull),MARGIN=1,FUN=f17)
      for (i in 1:length(fnames)) {
        ff = unlist(strsplit(fnames[i],"/"))
        ff = ff[length(ff)]
        if (length(which(fnamesshort == ff)) > 0) {
          tmp5[i] = fnamesfull[which(fnamesshort == ff)]
          tmp6[i] = phase[which(fnamesshort == ff)]
        }
      }
    } else if (is.mv == TRUE) {
      tmp5 = fnamesfull
      phase = apply(X=as.matrix(fnamesfull),MARGIN=1,FUN=f17)
      tmp6 = phase
      }
  } else {
    stop(paste0("\nNo files to analyse. Check that there are accelerometer files",
                "in the directory specified with argument datadir"))
  }
  if (length(f0) ==  0) f0 = 1
  if (length(f1) ==  0) f1 = length(fnames)
  if (is.na(fnames[1]) == TRUE) {
    stop('\nError: File path not clearly identified. Check value of argument datadir')
  }
  if (f0 > length(fnames)) f0 = 1
  if (f1 > length(fnames)) f1 = length(fnames)
  #========================================================================
  # check which files have already been processed, such that no double work is done
  # ffdone a matrix with all the binary filenames that have been processed
  ffdone = fdone = dir(paste(outputdir,outputfolder,"/meta/basic",sep=""))

  if (length(fdone) > 0) {
    for (ij in 1:length(fdone)) {
      tmp = unlist(strsplit(fdone[ij],".RData"))
      tmp2 = unlist(strsplit(tmp[1],"meta_"))
      ffdone[ij] = tmp2[2]
    }
  } else {
    ffdone = c()
  }
  #=================================================================
  # THE LOOP TO RUN THROUGH ALL BINARY FILES AND PROCES THEM
  fnames = sort(fnames)
  if (do.parallel == TRUE) {
    closeAllConnections() # in case there is a still something running from last time, kill it.
    cores=parallel::detectCores()
    Ncores = cores[1]
    if (Ncores > 3) {
      Nmetrics2calc = do.bfen + do.enmo + do.lfenmo + do.lfen + do.en + do.hfen + do.hfenplus + do.mad +
        do.anglex + do.angley + do.anglez + do.roll_med_acc_x + do.roll_med_acc_y +
        do.roll_med_acc_z + do.dev_roll_med_acc_x + do.dev_roll_med_acc_y +
        do.dev_roll_med_acc_z + do.enmoa + do.lfx + do.lfy + do.lfz + do.hfx + 
        do.hfy + do.hfz + do.bfx +  do.bfy + do.bfz
      if (Nmetrics2calc > 4) { #Only give warning when user wants more than 4 metrics.
        warning(paste0("\nExtracting many metrics puts higher demands on memory. Please consider",
                       " reducing the value for argument chunksize or setting do.parallel to FALSE"))
      }
      if (chunksize > 0.6 & Nmetrics2calc < 3) { # default ENMO and anglez
        chunksize = 0.6 # put limit to chunksize, because when processing in parallel memory is more limited
      } else if (chunksize > 0.6 & Nmetrics2calc >= 3 & Nmetrics2calc < 6) { # if user wants to extract 3-5 metrics
        chunksize = 0.5 # put limit to chunksize, because when processing in parallel memory is more limited
      } else if (chunksize > 0.6 & Nmetrics2calc >= 6) { # if user wants to extract more than 5 metrics
        chunksize = 0.4 # put limit to chunksize, because when processing in parallel memory is more limited
      }
      cl <- parallel::makeCluster(Ncores-1) #not to overload your computer
      doParallel::registerDoParallel(cl)

    } else {
      cat(paste0("\nparallel processing not possible because number of available cores (",Ncores,") < 4"))
      do.parallel = FALSE
    }
  }
  t1 = Sys.time() # copied here
  if (do.parallel == TRUE) {
    cat(paste0('\n Busy processing ... see ', outputdir, outputfolder,'/meta/basic', ' for progress\n'))
  }
  # check whether we are indevelopment mode:
  GGIRinstalled = is.element('GGIR', installed.packages()[,1])
  packages2passon = functions2passon = NULL
  GGIRloaded = "GGIR" %in% .packages()
  if (GGIRloaded) { #pass on package
    packages2passon = 'GGIR'
    errhand = 'pass'
  } else { # pass on functions
    # packages2passon = 'Rcpp'
    functions2passon = c("g.inspectfile", "g.calibrate","g.getmeta", "g.dotorcomma", "g.applymetrics",
                         "g.binread", "g.cwaread", "g.readaccfile", "g.wavread", "g.downsample", "updateBlocksize",
                         "g.getidfromheaderobject", "g.getstarttime", "POSIXtime2iso8601", "chartime2iso8601",
                         "iso8601chartime2POSIX", "g.metric", "datadir2fnames", "read.myacc.csv",
                         "get_nw_clip_block_params", "get_starttime_weekday_meantemp_truncdata", "ismovisens")
    errhand = 'stop'
    # Note: This will not work for cwa files, because those also need Rcpp functions.
    # So, it is probably best to turn off parallel when debugging cwa data.
  }
  `%myinfix%` = ifelse(do.parallel, foreach::`%dopar%`, foreach::`%do%`) # thanks to https://stackoverflow.com/questions/43733271/how-to-switch-programmatically-between-do-and-dopar-in-foreach
  output_list =foreach::foreach(i=f0:f1, .packages = packages2passon,
                                .export=functions2passon, .errorhandling=errhand) %myinfix% {
    tryCatchResult = tryCatch({
      # for (i in f0:f1) { #f0:f1 #j is file index (starting with f0 and ending with f1)
      if (print.filename == TRUE) {
        cat(paste0("\nFile name: ",fnames[i]))
      }
      if (filelist == TRUE) {
        datafile = as.character(fnames[i])
      } else {
        datafile = paste(datadir,"/",fnames[i],sep="")
      }
      #=========================================================
      #check whether file has already been processed
      #by comparing filename to read with list of processed files
      fnames_without = as.character(unlist(strsplit(as.character(fnames[i]),".csv"))[1])
      #remove / if it was a list
      fnames_without2 = fnames_without
      teimp = unlist(strsplit(as.character(fnames_without),"/"))
      if (length(teimp) > 1) {
        fnames_without2 = teimp[length(teimp)]
      } else {
        fnames_without2 = fnames_without
      }
      fnames_without = fnames_without2
      withoutRD = unlist(strsplit(fnames_without,"[.]RD"))
      if (length(withoutRD) > 1) {
        fnames_without = withoutRD[1]
      }
      if (length(ffdone) > 0) {
        ffdone_without = 1:length(ffdone) #dummy variable
        for (index in 1:length(ffdone)) {
          ffdone_without[index] = as.character(unlist(strsplit(as.character(ffdone[index]),".csv"))[1])
        }
        if (length(which(ffdone_without == fnames_without)) > 0) {
          skip = 1 #skip this file because it was analysed before")
        } else {
          skip = 0 #do not skip this file
        }
      } else {
        skip = 0
      }
      if (length(unlist(strsplit(datafile,"[.]RD"))) > 1) {
        useRDA = TRUE
      } else {
        useRDA = FALSE
      }
      #=============================================================
      # Inspect file (and store output later on)
      options(warn=-1) #turn off warnings
      if (useRDA == FALSE) {
        I = g.inspectfile(datafile, desiredtz=desiredtz,
                          rmc.dec=rmc.dec,configtz=configtz,
                          rmc.firstrow.acc = rmc.firstrow.acc,
                          rmc.firstrow.header = rmc.firstrow.header,
                          rmc.header.length = rmc.header.length,
                          rmc.col.acc = rmc.col.acc,
                          rmc.col.temp = rmc.col.temp, rmc.col.time=rmc.col.time,
                          rmc.unit.acc = rmc.unit.acc, rmc.unit.temp = rmc.unit.temp,
                          rmc.unit.time = rmc.unit.time,
                          rmc.format.time = rmc.format.time,
                          rmc.bitrate = rmc.bitrate, rmc.dynamic_range = rmc.dynamic_range,
                          rmc.unsignedbit = rmc.unsignedbit,
                          rmc.origin = rmc.origin,
                          rmc.desiredtz = rmc.desiredtz, rmc.sf = rmc.sf,
                          rmc.headername.sf = rmc.headername.sf,
                          rmc.headername.sn = rmc.headername.sn,
                          rmc.headername.recordingid = rmc.headername.sn,
                          rmc.header.structure = rmc.header.structure,
                          rmc.check4timegaps = rmc.check4timegaps)
      } else {
        load(datafile) # to do: would be nice to only load the object I and not the entire datafile
        I$filename = fnames[i]
      }
      options(warn=0) #turn on warnings
      if (overwrite == TRUE) skip = 0
      if (skip == 0) { #if skip = 1 then skip the analysis as you already processed this file
        cat(paste0("\nP1 file ",i))
        turn.do.cal.back.on = FALSE
        if (do.cal == TRUE & I$dformc == 3) { # do not do the auto-calibration for wav files (because already done in pre-processign)
          do.cal = FALSE
          turn.do.cal.back.on = TRUE
        }
        data_quality_report_exists = file.exists(paste0(outputdir,"/",outputfolder,"/results/QC/data_quality_report.csv",sep=""))
        assigned.backup.cal.coef = FALSE
        if (length(backup.cal.coef) > 0) {
          if (backup.cal.coef == "retrieve") {
            if (data_quality_report_exists == TRUE) { # use the data_quality_report as backup for calibration coefficients
              backup.cal.coef = paste0(outputdir,outputfolder,"/results/QC/data_quality_report.csv",sep="")
              assigned.backup.cal.coef = TRUE
            }
          } else if (backup.cal.coef == "redo") { #ignore the backup calibration coefficients, and derive them again
            backup.cal.coef = c()
            assigned.backup.cal.coef = TRUE
          } else if (backup.cal.coef != "redo" & backup.cal.coef != "retrieve") {
            # Do nothing, backup.cal.coef is the path to the csv-file with calibration coefficients
            assigned.backup.cal.coef = TRUE
          }
        }
        #data_quality_report.csv does not exist and there is also no ot
        if (assigned.backup.cal.coef == FALSE) backup.cal.coef = c()
        #--------------------------------------
        if (do.cal ==TRUE & useRDA == FALSE & length(backup.cal.coef) == 0) {
          # cat(paste0("\n",rep('-',options()$width),collapse=''))
          cat("\n")
          cat("\nInvestigate calibration of the sensors with function g.calibrate:\n")
          C = g.calibrate(datafile, spherecrit=spherecrit,
                          minloadcrit=minloadcrit,printsummary=printsummary,chunksize=chunksize,
                          windowsizes=windowsizes,selectdaysfile=selectdaysfile,dayborder=dayborder,
                          desiredtz=desiredtz,
                          rmc.dec=rmc.dec,configtz=configtz,
                          rmc.firstrow.acc = rmc.firstrow.acc,
                          rmc.firstrow.header = rmc.firstrow.header,
                          rmc.header.length = rmc.header.length,
                          rmc.col.acc = rmc.col.acc,
                          rmc.col.temp = rmc.col.temp, rmc.col.time=rmc.col.time,
                          rmc.unit.acc = rmc.unit.acc, rmc.unit.temp = rmc.unit.temp,
                          rmc.unit.time = rmc.unit.time,
                          rmc.format.time = rmc.format.time,
                          rmc.bitrate = rmc.bitrate, rmc.dynamic_range = rmc.dynamic_range,
                          rmc.unsignedbit = rmc.unsignedbit,
                          rmc.origin = rmc.origin,
                          rmc.desiredtz = rmc.desiredtz, rmc.sf = rmc.sf,
                          rmc.headername.sf = rmc.headername.sf,
                          rmc.headername.sn = rmc.headername.sn,
                          rmc.headername.recordingid = rmc.headername.sn,
                          rmc.header.structure = rmc.header.structure,
                          rmc.check4timegaps = rmc.check4timegaps,
                          rmc.noise=rmc.noise,
                          rmc.col.wear=rmc.col.wear,
                          rmc.doresample=rmc.doresample)
        } else {
          C = list(cal.error.end=0,cal.error.start=0)
          C$scale=c(1,1,1)
          C$offset=c(0,0,0)
          C$tempoffset=  c(0,0,0)
          C$QCmessage = "Autocalibration not done"
          C$npoints = 0
          C$nhoursused= 0
          C$use.temp = use.temp
        }
        if (turn.do.cal.back.on == TRUE) {
          do.cal = TRUE
        }
        cal.error.end = C$cal.error.end
        cal.error.start = C$cal.error.start
        if (length(cal.error.start) == 0) {
          #file too shortcorrupt to even calculate basic calibration value
          cal.error.start = NA
        }
        check.backup.cal.coef = FALSE
        if (is.na(cal.error.start) == T | length(cal.error.end) == 0) {
          C$scale = c(1,1,1); C$offset = c(0,0,0);       C$tempoffset=  c(0,0,0)
          check.backup.cal.coef = TRUE
        } else {
          if (cal.error.start < cal.error.end) {
            C$scale = c(1,1,1); C$offset = c(0,0,0);       C$tempoffset=  c(0,0,0)
            check.backup.cal.coef = TRUE
          }
        }
        if (length(backup.cal.coef) > 0) check.backup.cal.coef = TRUE
        #if calibration fails then check whether calibration coefficients are provided in a separate csv-spreadsheet
        # this csv spreadhseet needs to be created by the end-user and should contain:
        # column with names of the accelerometer files
        # three columns respectively named scale.x, scale.y, and scale.z
        # three columns respectively named offset.x, offset.y, and offset.z
        # three columns respectively named temperature.offset.x, temperature.offset.y, and temperature.offset.z
        # the end-user can generate this document based on calibration analysis done with the same accelerometer device.
        if (length(backup.cal.coef) > 0 & check.backup.cal.coef == TRUE) {
          bcc.data = read.csv(backup.cal.coef)
          cat("\nRetrieving previously derived calibration coefficients")
          bcc.data$filename = as.character(bcc.data$filename)
          for (nri in 1:nrow(bcc.data)) {
            tmp = unlist(strsplit(as.character(bcc.data$filename[nri]),"meta_"))
            if (length(tmp) > 1) {
              bcc.data$filename[nri] = tmp[2]
              bcc.data$filename[nri] = unlist(strsplit(bcc.data$filename[nri],".RData"))[1]
            }
          }
          if (length(which(as.character(bcc.data$filename) == fnames[i])) > 0) {
            bcc.i = which(bcc.data$filename == fnames[i])
            bcc.cal.error.start = which(colnames(bcc.data) == "cal.error.start")
            bcc.cal.error.end = which(colnames(bcc.data) == "cal.error.end")
            bcc.scalei = which(colnames(bcc.data) == "scale.x" | colnames(bcc.data) == "scale.y" | colnames(bcc.data) == "scale.z")
            bcc.offseti = which(colnames(bcc.data) == "offset.x" | colnames(bcc.data) == "offset.y" | colnames(bcc.data) == "offset.z")
            bcc.temp.offseti = which(colnames(bcc.data) == "temperature.offset.x" | colnames(bcc.data) == "temperature.offset.y" | colnames(bcc.data) == "temperature.offset.z")
            C$scale = as.numeric(bcc.data[bcc.i[1],bcc.scalei])
            C$offset = as.numeric(bcc.data[bcc.i[1],bcc.offseti])
            C$tempoffset=  as.numeric(bcc.data[bcc.i[1],bcc.temp.offseti])
            cat(paste0("\nRetrieved Calibration error (g) before: ",as.numeric(bcc.data[bcc.i[1],bcc.cal.error.start])))
            cat(paste0("\nRetrieved Callibration error (g) after: ",as.numeric(bcc.data[bcc.i[1],bcc.cal.error.end])))
            cat(paste0("\nRetrieved offset correction ",c("x","y","z"),": ",C$offset))
            cat(paste0("\nRetrieved scale correction ",c("x","y","z"),": ",C$scale))
            cat(paste0("\nRetrieved tempoffset correction ",c("x","y","z"),": ",C$tempoffset))
            cat("\n")
          } else {
            # cat("\nNo matching filename found in backup.cal.coef\n")
            # cat(paste0("\nCheck that filename ",fnames[i]," exists in the csv-file\n"))
            if (do.cal ==TRUE & useRDA == FALSE) { # If no matching filename could be found, then try to derive the calibration coeficients in the normal way
              cat("\n")
              cat("\nInvestigate calibration of the sensors with function g.calibrate:\n")
              C = g.calibrate(datafile, spherecrit=spherecrit,
                              minloadcrit=minloadcrit,printsummary=printsummary,chunksize=chunksize,
                              windowsizes=windowsizes,selectdaysfile=selectdaysfile,dayborder=dayborder,
                              desiredtz=desiredtz,  rmc.dec=rmc.dec,configtz=configtz,
                              rmc.firstrow.acc = rmc.firstrow.acc,
                              rmc.firstrow.header = rmc.firstrow.header,
                              rmc.header.length = rmc.header.length,
                              rmc.col.acc = rmc.col.acc,
                              rmc.col.temp = rmc.col.temp, rmc.col.time=rmc.col.time,
                              rmc.unit.acc = rmc.unit.acc, rmc.unit.temp = rmc.unit.temp,
                              rmc.unit.time = rmc.unit.time,
                              rmc.format.time = rmc.format.time,
                              rmc.bitrate = rmc.bitrate, rmc.dynamic_range = rmc.dynamic_range,
                              rmc.unsignedbit = rmc.unsignedbit,
                              rmc.origin = rmc.origin,
                              rmc.desiredtz = rmc.desiredtz, rmc.sf = rmc.sf,
                              rmc.headername.sf = rmc.headername.sf,
                              rmc.headername.sn = rmc.headername.sn,
                              rmc.headername.recordingid = rmc.headername.sn,
                              rmc.header.structure = rmc.header.structure,
                              rmc.check4timegaps = rmc.check4timegaps,
                              rmc.noise=rmc.noise)
            }
          }
        }
        #------------------------------------------------
        cat("\nExtract signal features (metrics) with the g.getmeta function:\n")
        M = g.getmeta(datafile,
                      do.bfen=do.bfen,
                      do.enmo=do.enmo,
                      do.lfenmo=do.lfenmo,
                      do.lfen=do.lfen,
                      do.en=do.en,
                      do.hfen=do.hfen,
                      do.hfenplus=do.hfenplus,
                      do.mad=do.mad,
                      do.anglex=do.anglex, do.angley=do.angley, do.anglez=do.anglez,
                      do.roll_med_acc_x=do.roll_med_acc_x,
                      do.roll_med_acc_y=do.roll_med_acc_y,
                      do.roll_med_acc_z=do.roll_med_acc_z,
                      do.dev_roll_med_acc_x=do.dev_roll_med_acc_x, 
                      do.dev_roll_med_acc_y=do.dev_roll_med_acc_y, 
                      do.dev_roll_med_acc_z=do.dev_roll_med_acc_z,
                      do.enmoa=do.enmoa,
                      do.lfx=do.lfx, do.lfy=do.lfy, do.lfz=do.lfz, 
                      do.hfx=do.hfx, do.hfy=do.hfy, do.hfz=do.hfz,
                      do.bfx=do.bfx, do.bfy=do.bfy, do.bfz=do.bfz, 
                      lb = lb, hb = hb,  n = n,
                      desiredtz=desiredtz, daylimit=daylimit, windowsizes=windowsizes,
                      tempoffset=C$tempoffset, scale=C$scale, offset=C$offset,
                      meantempcal=C$meantempcal, chunksize=chunksize,
                      selectdaysfile=selectdaysfile,
                      outputdir=outputdir,
                      outputfolder=outputfolder,
                      dayborder=dayborder, dynrange=dynrange,
                      rmc.dec=rmc.dec, configtz=configtz,
                      rmc.firstrow.acc = rmc.firstrow.acc,
                      rmc.firstrow.header = rmc.firstrow.header,
                      rmc.header.length = rmc.header.length,
                      rmc.col.acc = rmc.col.acc,
                      rmc.col.temp = rmc.col.temp, rmc.col.time=rmc.col.time,
                      rmc.unit.acc = rmc.unit.acc, rmc.unit.temp = rmc.unit.temp,
                      rmc.unit.time = rmc.unit.time,
                      rmc.format.time = rmc.format.time,
                      rmc.bitrate = rmc.bitrate, rmc.dynamic_range = rmc.dynamic_range,
                      rmc.unsignedbit = rmc.unsignedbit,
                      rmc.origin = rmc.origin,
                      rmc.desiredtz = rmc.desiredtz, rmc.sf = rmc.sf,
                      rmc.headername.sf = rmc.headername.sf,
                      rmc.headername.sn = rmc.headername.sn,
                      rmc.headername.recordingid = rmc.headername.sn,
                      rmc.header.structure = rmc.header.structure,
                      rmc.check4timegaps = rmc.check4timegaps,
                      rmc.noise=rmc.noise,
                      rmc.col.wear=rmc.col.wear,
                      rmc.doresample=rmc.doresample,
                      myfun=myfun)
        #------------------------------------------------
        cat("\nSave .RData-file with: calibration report, file inspection report and all signal features...\n")
        # remove directory in filename if present
        filename = unlist(strsplit(fnames[i],"/"))
        if (length(filename) > 0) {
          filename = filename[length(filename)]
        } else {
          filename = fnames[i]
        }
        filename_dir=tmp5[i];filefoldername=tmp6[i]
        if (length(unlist(strsplit(fnames[1],"[.]RD"))) == 1) { # to avoid getting .RData.RData
          filename = paste0(filename,".RData")
        }
        save(M,I,C,filename_dir,filefoldername,file = paste(path3,"/meta/basic/meta_",filename,sep=""))
        # as metadatdir is not known derive it:
        metadatadir = c()
        if (length(datadir) > 0) {
          # list of all csv and bin files
          if (is.mv == TRUE) {
            for (filei in 1:length(fnames)) {
              fnames[[filei]] = strsplit(fnames[[filei]], "/")[[1]][1]
            }
            fnames = unique(fnames)
          } else if(is.mv == FALSE){
            fnames = datadir2fnames(datadir,filelist) #GGIR::
          }
          # check whether these are RDA
          if (length(unlist(strsplit(fnames[1],"[.]RD"))) > 1) {
            useRDA = TRUE
          } else {
            useRDA = FALSE
          }
        } else {
          useRDA = FALSE
        }
        if (filelist == TRUE | useRDA == TRUE) {
          metadatadir = paste(outputdir,"/output_",studyname,sep="")
        } else {
          outputfoldername = unlist(strsplit(datadir,"/"))[length(unlist(strsplit(datadir,"/")))]
          metadatadir = paste(outputdir,"/output_",outputfoldername,sep="")
        }

        rm(M); rm(I); rm(C)
        # }
      }
    }) # END tryCatch
    return(tryCatchResult)
  }
  if (do.parallel == TRUE) {
    on.exit(parallel::stopCluster(cl))
    for (oli in 1:length(output_list)) { # logged error and warning messages
      if (is.null(unlist(output_list[oli])) == FALSE) {
        cat(paste0("\nErrors and warnings for ",fnames[oli]))
        print(unlist(output_list[oli])) # print any error and warnings observed
      }
    }
  }
  if (exists("metadatadir")) { # do this because foreach may not use all cores at the end of a loop and then metadatadir can be missing
    if (length(metadatadir) > 0) {
      SI = sessionInfo()
      sessionInfoFile = paste(metadatadir,"/results/QC/sessioninfo_part1.RData",sep="")
      if (file.exists(sessionInfoFile)) {
        FI = file.info(sessionInfoFile)
        timesincecreation = abs(as.numeric(difftime(FI$ctime,Sys.time(),units="secs")))
        # if file is older than 2 hours plus a random number of seconds (max 1 hours) then overwrite it
        if (timesincecreation > (2*3600 + (sample(seq(1,3600,by=0.1),size = 1)))) {
          save(SI,file=sessionInfoFile)
        }
      } else {
        save(SI,file=sessionInfoFile)
      }
    }
  }
}
