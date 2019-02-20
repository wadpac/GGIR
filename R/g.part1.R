g.part1 = function(datadir=c(),outputdir=c(),f0=1,f1=c(),windowsizes = c(5,900,3600),
                   desiredtz = "Europe/London",chunksize=c(),studyname=c(),
                   do.enmo = TRUE,do.lfenmo = FALSE,do.en = FALSE,
                   do.bfen = FALSE,do.hfen=FALSE,do.hfenplus = FALSE, do.mad = FALSE,
                   do.anglex=FALSE,do.angley=FALSE,do.anglez=FALSE,
                   do.enmoa = FALSE,
                   do.roll_med_acc_x=FALSE,do.roll_med_acc_y=FALSE,do.roll_med_acc_z=FALSE,
                   do.dev_roll_med_acc_x=FALSE,do.dev_roll_med_acc_y=FALSE,do.dev_roll_med_acc_z=FALSE,
                   do.cal = TRUE,
                   lb = 0.2, hb = 15,  n = 4,use.temp=TRUE,spherecrit=0.3,
                   minloadcrit=72,printsummary=TRUE,print.filename=FALSE,overwrite=FALSE,
                   backup.cal.coef=c(),selectdaysfile=c(),dayborder=0,dynrange=c()) {
  if (length(datadir) == 0 | length(outputdir) == 0) {
    if (length(datadir) == 0) {
      cat("\nVariable datadir is not defined")
    }
    if (length(outputdir) == 0) {
      cat("\nVariable outputdir is not specified")
    }
  }
  if (f1 == 0) cat("\nWarning: f1 = 0 is not a meaningful value")
  filelist = isfilelist(datadir)
  #Extra code to handle raw accelerometer data in Raw data format:
  # list of all csv and bin files
  fnames = datadir2fnames(datadir,filelist)
  # check whether these are RDA
  if (length(unlist(strsplit(fnames[1],"[.]RD"))) > 1) {
    useRDA = TRUE
  } else {
    useRDA = FALSE
  }
  # create output directory if it does not exist
  if (filelist == TRUE | useRDA == TRUE) {
    if (length(studyname) == 0) {
      studyname = "mystudy"
      cat("\nError: studyname not specified in part1. Needed for analysing lists of files")
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
  # get file path if requested:
  #   if (storefolderstructure == TRUE) {
#   filelist = FALSE
#   if (length(datadir) == 1) { #could be a directory or one file
#     if (length(unlist(strsplit(datadir,"[.]bi")))>0) filelist = TRUE
#     if (length(unlist(strsplit(datadir,"[.]cs")))>0) filelist = TRUE
#   } else { #multiple files
#     filelist = TRUE    
#   }
  if (filelist == FALSE) {
    fnamesfull = c(dir(datadir,recursive=TRUE,pattern="[.]csv"),
                   dir(datadir,recursive=TRUE,pattern="[.]bin"),
                   dir(datadir,recursive=TRUE,pattern="[.]wav"),
                   dir(datadir,recursive=TRUE,pattern="[.]cwa"))
  } else {
    fnamesfull = datadir
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
  }
  if (length(f0) ==  0) f0 = 1
  if (length(f1) ==  0) f1 = length(fnames)
  if (is.na(fnames[1]) == TRUE) {
    cat("\nError: File path not clearly identified. Check path name")
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
  if (length(fnames) == 0) {
    cat("\nNo files to analyse")
  }
  # filelocationkey = matrix("",length(fnames),3)
  fnames = sort(fnames)
  for (j in f0:f1) { #f0:f1 #j is file index (starting with f0 and ending with f1)
    if (print.filename == TRUE) {
      cat(paste0("\nFile name: ",fnames[j]))
    }
    if (filelist == TRUE) {
      datafile = as.character(fnames[j])
    } else {
      datafile = paste(datadir,"/",fnames[j],sep="")
    }
    #=========================================================
    #check whether file has already been processed
    #by comparing filename to read with list of processed files
    fnames_without = as.character(unlist(strsplit(as.character(fnames[j]),".csv"))[1])
    # create list of both file name and full directory
    # filelocationkey[j,1] = fnames_without
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
    #================================================================
    # Inspect file (and store output later on)
    options(warn=-1) #turn off warnings
    if (useRDA == FALSE) {
      I = g.inspectfile(datafile, desiredtz=desiredtz)
    } else {
      load(datafile) # to do: would be nice to only load the object I and not the entire datafile
      I$filename = fnames[j]
    }
    options(warn=0) #turn on warnings
    if (overwrite == TRUE) skip = 0
    if (skip == 0) { #if skip = 1 then skip the analysis as you already processed this file
      cat(paste0("\nP1 file",j))
      turn.do.cal.back.on = FALSE
      if (do.cal == TRUE & I$dformc == 3) { # do not do the auto-calibration for wav files (because already done in pre-processign)
        do.cal = FALSE
        turn.do.cal.back.on = TRUE
      }
      #--------------------------------------
      if (do.cal ==TRUE & useRDA == FALSE) {
        # cat(paste0("\n",rep('-',options()$width),collapse=''))
        cat("\n")
        cat("\nInvestigate calibration of the sensors with function g.calibrate:\n")
        C = g.calibrate(datafile,use.temp=use.temp,spherecrit=spherecrit,
                        minloadcrit=minloadcrit,printsummary=printsummary,chunksize=chunksize,
                        windowsizes=windowsizes,selectdaysfile=selectdaysfile,dayborder=dayborder,
                        desiredtz=desiredtz)
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
      #if calibration fails then check whether calibration coefficients are provided in a separate csv-spreadsheet
      # this csv spreadhseet needs to be created by the end-user and should contain:
      # column with names of the accelerometer files
      # three columns respectively named scale.x, scale.y, and scale.z
      # three columns respectively named offset.x, offset.y, and offset.z
      # three columns respectively named temperature.offset.x, temperature.offset.y, and temperature.offset.z
      # the end-user can generate this document based on calibration analysis done with the same accelerometer device.
      if (length(backup.cal.coef) > 0 & check.backup.cal.coef == TRUE) { 
        bcc.data = read.csv(backup.cal.coef)
        cat("\nTry to retrieve back-up calibration coefficients as provided by argument backup.cal.coef:\n")
        if (length(which(as.character(bcc.data$filename) == fnames[j])) > 0) {
          cat("\nMatching filename found in backup.cal.coef\n")
          bcc.i = which(bcc.data$filename == fnames[j])
          bcc.scalei = which(colnames(bcc.data) == "scale.x" | colnames(bcc.data) == "scale.y" | colnames(bcc.data) == "scale.z")
          bcc.offseti = which(colnames(bcc.data) == "offset.x" | colnames(bcc.data) == "offset.y" | colnames(bcc.data) == "offset.z")
          bcc.temp.offseti = which(colnames(bcc.data) == "temperature.offset.x" | colnames(bcc.data) == "temperature.offset.y" | colnames(bcc.data) == "temperature.offset.z")
          C$scale = as.numeric(bcc.data[bcc.i[1],bcc.scalei])
          C$offset = as.numeric(bcc.data[bcc.i[1],bcc.offseti])
          C$tempoffset=  as.numeric(bcc.data[bcc.i[1],bcc.temp.offseti])
          cat(paste0("\nNew offset correction ",c("x","y","z"),": ",C$offset))
          cat(paste0("\nNew scale correction ",c("x","y","z"),": ",C$scale))
          cat(paste0("\nNew tempoffset correction ",c("x","y","z"),": ",C$tempoffset))
        } else {
          cat("\nNo matching filename found in backup.cal.coef\n")
          cat(paste0("\nCheck that filename ",fnames[j]," exists in the csv-file\n"))
        }
      }
      #------------------------------------------------
      cat("\nExtract signal features (metrics) with the g.getmeta function:\n")
      M = g.getmeta(datafile,                  
                    do.bfen=do.bfen,
                    do.enmo=do.enmo,
                    do.lfenmo=do.lfenmo,
                    do.en=do.en,
                    do.hfen=do.hfen,
                    do.hfenplus=do.hfenplus,
                    do.mad=do.mad,
                    do.anglex=do.anglex,do.angley=do.angley,do.anglez=do.anglez,
                    do.roll_med_acc_x=do.roll_med_acc_x,do.roll_med_acc_y=do.roll_med_acc_y,do.roll_med_acc_z=do.roll_med_acc_z,
                    do.dev_roll_med_acc_x=do.dev_roll_med_acc_x,do.dev_roll_med_acc_y=do.dev_roll_med_acc_y,do.dev_roll_med_acc_z=do.dev_roll_med_acc_z,
                    do.enmoa=do.enmoa,
                    lb = lb, hb = hb,  n = n,
                    desiredtz=desiredtz,daylimit=daylimit,windowsizes=windowsizes,
                    tempoffset=C$tempoffset,scale=C$scale,offset=C$offset,
                    meantempcal=C$meantempcal,chunksize=chunksize,
                    selectdaysfile=selectdaysfile,
                    outputdir=outputdir,
                    outputfolder=outputfolder,
                    dayborder=dayborder,dynrange=dynrange)
      #------------------------------------------------
      cat("\nSave .RData-file with: calibration report, file inspection report and all signal features...\n")
      # remove directory in filename if present
      filename = unlist(strsplit(fnames[j],"/"))
      if (length(filename) > 0) {
        filename = filename[length(filename)]
      } else {
        filename = fnames[j]
      }
      filename_dir=tmp5[j];filefoldername=tmp6[j]
      if (length(unlist(strsplit(fnames[1],"[.]RD"))) == 1) { # to avoid getting .RData.RData
        filename = paste0(filename,".RData")
      }
      save(M,I,C,filename_dir,filefoldername,file = paste(path3,"/meta/basic/meta_",filename,sep=""))
      # SI = sessionInfo()
      # save(SI,file=paste(path3,"/results/QC/sessioninfo_part1.RData",sep=""))
      
      
      # as metadatdir is not known derive it:
      metadatadir = c()
      if (length(datadir) > 0) {
        # list of all csv and bin files
        fnames = datadir2fnames(datadir,filelist)
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
      rm(M); rm(I); rm(C)
    }
    # if(length(filelocationkey) > 0) { # turned off on 20-2-2019, because information is already stored in part2, 4 and 5
    #   filelocationkey[,3] = datadir[j]
    #   filelocationkey = rbind(c("Filename with full path","Filename","data directory"),filelocationkey)
    #   write.csv(filelocationkey,paste(path3,"/results/QC/filelocationkey.csv",sep=""),row.names=FALSE)
    # }
    closeAllConnections()
  }
}
