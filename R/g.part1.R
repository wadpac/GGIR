g.part1 = function(datadir = c(), outputdir = c(), f0 = 1, f1 = c(),
                   studyname = c(), myfun = c(),
                   params_metrics = c(), params_rawdata = c(),
                   params_cleaning = c(), params_general = c(), ...) {
  
  #----------------------------------------------------------
  # Extract and check parameters
  input = list(...)
  params = extract_params(params_metrics = params_metrics,
                          params_rawdata = params_rawdata,
                          params_cleaning = params_cleaning,
                          params_general = params_general,
                          input = input, 
                          params2check = c("metrics", "rawdata",
                                           "cleaning", "general")) # load default parameters
  params_metrics = params$params_metrics
  params_rawdata = params$params_rawdata
  params_cleaning = params$params_cleaning
  params_general = params$params_general
  # identify whether datadir is list of files or a directory
  if (length(datadir) == 0 | length(outputdir) == 0) {
    if (length(datadir) == 0) {
      stop('\nVariable datadir is not defined')
    }
    if (length(outputdir) == 0) {
      stop('\nVariable outputdir is not specified')
    }
  }
  
  if (f1 == 0) cat("\nWarning: f1 = 0 is not a meaningful value")
  filelist = isfilelist(datadir)
  if (filelist == FALSE) {
    if (dir.exists(datadir) == FALSE) {
      stop("\nDirectory specified by argument datadir, does not exist")
    }
    if (grepl(datadir, outputdir)) {
      stop(paste0('\nError: The file path specified by argument outputdir should",
                " NOT equal or be a subdirectory of the path specified by argument datadir'))
    }
  }
  # list all accelerometer files
  dir2fn = datadir2fnames(datadir,filelist)
  fnames = dir2fn$fnames
  fnamesfull = dir2fn$fnamesfull
  # check whether these are movisens files
  is.mv = ismovisens(datadir)
  if (filelist == FALSE & is.mv == TRUE) {
    fnamesfull = dir(datadir, recursive = TRUE, pattern = "acc.bin", full.names = TRUE)
    fnames = dir(datadir, recursive = FALSE)
  }
  # check whether these are .RDA files
  if (length(unlist(strsplit(fnames[1],"[.]RD"))) > 1) {
    useRDA = TRUE
  } else {
    useRDA = FALSE
  }
  if (useRDA == FALSE) {
    filesizes = file.info(fnamesfull)$size # in bytes
    bigEnough = which(filesizes/1e6 > params_rawdata[["minimumFileSizeMB"]])
    fnamesfull = fnamesfull[bigEnough]
    fnames = fnames[bigEnough]
  }
  # create output directory if it does not exist
  if (filelist == TRUE | useRDA == TRUE) {
    if (length(studyname) == 0) {
      studyname = "mystudy"
      stop('\nError: studyname not specified in part1. Needed for analysing lists of files')
    } else {
      outputfolder = paste0("/output_", studyname)
    }
  } else {
    outputfolder = unlist(strsplit(datadir, "/"))
    outputfolder = paste0("/output_",outputfolder[length(outputfolder)])
  }
  if (file.exists(paste0(outputdir, outputfolder))) {
  } else {
    dir.create(file.path(outputdir, outputfolder))
    dir.create(file.path(outputdir, outputfolder, "meta"))
    dir.create(file.path(outputdir, paste0(outputfolder,"/meta"), "basic"))
    if (length(params_cleaning[["selectdaysfile"]]) > 0) {
      dir.create(file.path(outputdir, paste0(outputfolder,"/meta"), "raw"))
    }
    dir.create(file.path(outputdir, outputfolder, "results"))
    dir.create(file.path(outputdir, paste0(outputfolder, "/results"), "QC"))
  }
  path3 = paste0(outputdir, outputfolder) #where is output stored?
  use.temp = TRUE
  daylimit = FALSE
  
  # check access permissions
  Nfile_without_readpermission = length(which(file.access(paste0(fnamesfull), mode = 4) == -1)) #datadir,"/",
  if (Nfile_without_readpermission > 0) {
    cat("\nChecking that user has read access permission for all files in data directory: No")
    warning(paste0("\nThere are/is ", Nfile_without_readpermission,
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
    out = unlist(strsplit(X, "/"))
    f17 = out[(length(out) - 1)]
  }
  tmp5 = tmp6 = rep(0, length(fnamesfull))
  if (length(fnamesfull) > 0) {
    if (is.mv == FALSE) {
      fnamesshort = apply(X = as.matrix(fnamesfull),MARGIN = 1, FUN = f16)
      phase = apply(X = as.matrix(fnamesfull), MARGIN = 1, FUN = f17)
      for (i in 1:length(fnames)) {
        ff = unlist(strsplit(fnames[i], "/"))
        ff = ff[length(ff)]
        if (length(which(fnamesshort == ff)) > 0) {
          tmp5[i] = fnamesfull[which(fnamesshort == ff)]
          tmp6[i] = phase[which(fnamesshort == ff)]
        }
      }
    } else if (is.mv == TRUE) {
      tmp5 = fnamesfull
      phase = apply(X = as.matrix(fnamesfull), MARGIN = 1, FUN = f17)
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
  ffdone = fdone = dir(paste0(outputdir, outputfolder, "/meta/basic"))
  
  if (length(fdone) > 0) {
    for (ij in 1:length(fdone)) {
      tmp = unlist(strsplit(fdone[ij],".RData"))
      tmp2 = unlist(strsplit(tmp[1],"meta_"))
      ffdone[ij] = tmp2[2]
    }
  } else {
    ffdone = c()
  }
  fnames = sort(fnames)
  fnamesfull = sort(fnamesfull)
  
  #=========================================================
  # Declare core functionality, which at the end of this g.part1 is either
  # applied to the file in parallel with foreach or serially with a loop
  main_part1 = function(i, params_metrics, params_rawdata,
                        params_cleaning, params_general, datadir, fnames, fnamesfull,
                        outputdir, myfun, filelist, studyname, ffdone, tmp5, tmp6,
                        use.temp, daylimit, path3, outputfolder, is.mv) {
    if (params_general[["print.filename"]] == TRUE) {
      cat(paste0("\nFile name: ",fnames[i]))
    }
    # if (filelist == TRUE) {
    datafile = as.character(fnamesfull[i])
    # } else {
    #   # datafile = paste0(datadir,"/",fnames[i])
    #   datafile = fnamesfull[i] #paste0(datadir,"/",fnames[i])
    # }
    #=========================================================
    #check whether file has already been processed
    #by comparing filename to read with list of processed files
    fnames_without = as.character(unlist(strsplit(as.character(fnames[i]), ".csv"))[1])
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
        ffdone_without[index] = as.character(unlist(strsplit(as.character(ffdone[index]), ".csv"))[1])
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
    options(warn = -1) #turn off warnings
    if (useRDA == FALSE) {
      I = g.inspectfile(datafile, desiredtz = params_general[["desiredtz"]],
                        params_rawdata = params_rawdata,
                        configtz = params_general[["configtz"]])
    } else {
      load(datafile) # to do: would be nice to only load the object I and not the entire datafile
      I$filename = fnames[i]
    }
    options(warn = 0) #turn on warnings
    if (params_general[["overwrite"]] == TRUE) skip = 0
    if (skip == 0) { #if skip = 1 then skip the analysis as you already processed this file
      cat(paste0("\nP1 file ",i))
      turn.do.cal.back.on = FALSE
      if (params_rawdata[["do.cal"]] == TRUE & I$dformc == 3) { # do not do the auto-calibration for wav files (because already done in pre-processign)
        params_rawdata[["do.cal"]] = FALSE
        turn.do.cal.back.on = TRUE
      }
      data_quality_report_exists = file.exists(paste0(outputdir, "/", outputfolder, "/results/QC/data_quality_report.csv"))
      assigned.backup.cal.coef = FALSE
      if (length(params_rawdata[["backup.cal.coef"]]) > 0) {
        if (params_rawdata[["backup.cal.coef"]] == "retrieve") {
          if (data_quality_report_exists == TRUE) { # use the data_quality_report as backup for calibration coefficients
            params_rawdata[["backup.cal.coef"]] = paste0(outputdir,outputfolder, "/results/QC/data_quality_report.csv")
            assigned.backup.cal.coef = TRUE
          }
        } else if (params_rawdata[["backup.cal.coef"]] == "redo") { #ignore the backup calibration coefficients, and derive them again
          params_rawdata[["backup.cal.coef"]] = c()
          assigned.backup.cal.coef = TRUE
        } else if (params_rawdata[["backup.cal.coef"]] != "redo" & params_rawdata[["backup.cal.coef"]] != "retrieve") {
          # Do nothing, backup.cal.coef is the path to the csv-file with calibration coefficients
          assigned.backup.cal.coef = TRUE
        }
      }
      #data_quality_report.csv does not exist and there is also no ot
      if (assigned.backup.cal.coef == FALSE) params_rawdata[["backup.cal.coef"]] = c()
      #--------------------------------------
      if (params_rawdata[["do.cal"]] == TRUE & useRDA == FALSE & length(params_rawdata[["backup.cal.coef"]]) == 0) {
        # cat(paste0("\n",rep('-',options()$width),collapse=''))
        cat("\n")
        cat("\nInvestigate calibration of the sensors with function g.calibrate:\n")
        C = g.calibrate(datafile,
                        params_rawdata = params_rawdata,
                        params_general = params_general,
                        params_cleaning = params_cleaning)
      } else {
        C = list(cal.error.end = 0, cal.error.start = 0)
        C$scale = c(1,1,1)
        C$offset = c(0,0,0)
        C$tempoffset =  c(0,0,0)
        C$QCmessage = "Autocalibration not done"
        C$npoints = 0
        C$nhoursused = 0
        C$use.temp = use.temp
      }
      if (turn.do.cal.back.on == TRUE) {
        params_rawdata[["do.cal"]] = TRUE
      }
      cal.error.end = C$cal.error.end
      cal.error.start = C$cal.error.start
      if (length(cal.error.start) == 0) {
        #file too shortcorrupt to even calculate basic calibration value
        cal.error.start = NA
      }
      check.backup.cal.coef = FALSE
      if (is.na(cal.error.start) == T | length(cal.error.end) == 0) {
        C$scale = c(1,1,1); C$offset = c(0,0,0); C$tempoffset = c(0,0,0)
        check.backup.cal.coef = TRUE
      } else {
        if (cal.error.start < cal.error.end) {
          C$scale = c(1,1,1); C$offset = c(0,0,0); C$tempoffset =  c(0,0,0)
          check.backup.cal.coef = TRUE
        }
      }
      if (length(params_rawdata[["backup.cal.coef"]]) > 0) check.backup.cal.coef = TRUE
      #if calibration fails then check whether calibration coefficients are provided in a separate csv-spreadsheet
      # this csv spreadhseet needs to be created by the end-user and should contain:
      # column with names of the accelerometer files
      # three columns respectively named scale.x, scale.y, and scale.z
      # three columns respectively named offset.x, offset.y, and offset.z
      # three columns respectively named temperature.offset.x, temperature.offset.y, and temperature.offset.z
      # the end-user can generate this document based on calibration analysis done with the same accelerometer device.
      if (length(params_rawdata[["backup.cal.coef"]]) > 0 & check.backup.cal.coef == TRUE) {
        bcc.data = read.csv(params_rawdata[["backup.cal.coef"]])
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
          C$tempoffset =  as.numeric(bcc.data[bcc.i[1],bcc.temp.offseti])
          cat(paste0("\nRetrieved Calibration error (g) before: ",as.numeric(bcc.data[bcc.i[1],bcc.cal.error.start])))
          cat(paste0("\nRetrieved Callibration error (g) after: ",as.numeric(bcc.data[bcc.i[1],bcc.cal.error.end])))
          cat(paste0("\nRetrieved offset correction ",c("x","y","z"),": ",C$offset))
          cat(paste0("\nRetrieved scale correction ",c("x","y","z"),": ",C$scale))
          cat(paste0("\nRetrieved tempoffset correction ",c("x","y","z"),": ",C$tempoffset))
          cat("\n")
        } else {
          # cat("\nNo matching filename found in backup.cal.coef\n")
          # cat(paste0("\nCheck that filename ",fnames[i]," exists in the csv-file\n"))
          if (params_rawdata[["do.cal"]] == TRUE & useRDA == FALSE) { # If no matching filename could be found, then try to derive the calibration coeficients in the normal way
            cat("\n")
            cat("\nInvestigate calibration of the sensors with function g.calibrate:\n")
            C = g.calibrate(datafile,
                            params_rawdata = params_rawdata,
                            params_general = params_general,
                            params_cleaning = params_cleaning)
          }
        }
      }
      #------------------------------------------------
      cat("\nExtract signal features (metrics) with the g.getmeta function:\n")
      M = g.getmeta(datafile,
                    params_metrics = params_metrics,
                    params_rawdata = params_rawdata,
                    params_general = params_general,
                    daylimit = daylimit,
                    tempoffset = C$tempoffset, scale = C$scale, offset = C$offset,
                    meantempcal = C$meantempcal,
                    outputdir = outputdir,
                    outputfolder = outputfolder,
                    selectdaysfile = params_cleaning[["selectdaysfile"]],
                    myfun = myfun)
      #------------------------------------------------
      cat("\nSave .RData-file with: calibration report, file inspection report and all signal features...\n")
      # remove directory in filename if present
      filename = unlist(strsplit(fnames[i],"/"))
      if (length(filename) > 0) {
        filename = filename[length(filename)]
      } else {
        filename = fnames[i]
      }
      filename_dir = tmp5[i]; filefoldername = tmp6[i]
      if (length(unlist(strsplit(fnames[1], "[.]RD"))) == 1) { # to avoid getting .RData.RData
        filename = paste0(filename,".RData")
      }
      save(M, I, C, filename_dir, filefoldername,
           file = paste0(path3, "/meta/basic/meta_", filename))
      # as metadatdir is not known derive it:
      metadatadir = c()
      if (length(datadir) > 0) {
        # list of all csv and bin files
        if (is.mv == TRUE) {
          for (filei in 1:length(fnames)) {
            fnames[[filei]] = strsplit(fnames[[filei]], "/")[[1]][1]
          }
          fnames = unique(fnames)
        } else if (is.mv == FALSE) {
          dir2fn = datadir2fnames(datadir, filelist)
          fnames = dir2fn$fnames
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
        metadatadir = paste0(outputdir,"/output_",studyname)
      } else {
        outputfoldername = unlist(strsplit(datadir, "/"))[length(unlist(strsplit(datadir, "/")))]
        metadatadir = paste0(outputdir, "/output_", outputfoldername)
      }
      rm(M); rm(I); rm(C)
    }
  } # end of main_part1
  
  #--------------------------------------------------------------------------------
  # Run the code either parallel or in serial (file index starting with f0 and ending with f1)
 
  
  if (params_general[["do.parallel"]] == TRUE) {
    # Check whether we are in development mode (this never applies when the package is installed):
    GGIRinstalled = is.element('GGIR', installed.packages()[,1])
    packages2passon = functions2passon = NULL
    GGIRloaded = "GGIR" %in% .packages()
    if (GGIRloaded) { # not in devleopment mode, pass on package
      packages2passon = 'GGIR'
      errhand = 'pass'
    } else { # development mode, pass on individual function that were sourced by developer
      # pass on functions
      # packages2passon = 'Rcpp'
      functions2passon = c("g.inspectfile", "g.calibrate","g.getmeta", "g.dotorcomma", "g.applymetrics",
                           "g.binread", "g.cwaread", "g.readaccfile", "g.wavread", "g.downsample", "updateBlocksize",
                           "g.getidfromheaderobject", "g.getstarttime", "POSIXtime2iso8601", "chartime2iso8601",
                           "iso8601chartime2POSIX", "datadir2fnames", "read.myacc.csv",
                           "get_nw_clip_block_params", "get_starttime_weekday_meantemp_truncdata", "ismovisens",
                           "g.extractheadervars", "g.imputeTimegaps", "resample", "parseGT3Xggir",
                           "read.gt3x_ggir")
      errhand = 'stop'
      # Note: This will not work for cwa files, because those also need Rcpp functions.
      # So, it is probably best to turn off parallel when debugging cwa data.
    }
    cores = parallel::detectCores()
    Ncores = cores[1]
    if (Ncores > 3) {
      Nmetrics2calc = sum(unlist(params_metrics[c("do.anglex", "do.angley", "do.anglez",
                                                  "do.zcx", "do.zcy", "do.zcz",
                                                  "do.enmo", "do.lfenmo", "do.en", "do.mad", "do.enmoa",
                                                  "do.roll_med_acc_x", "do.roll_med_acc_y", "do.roll_med_acc_z",
                                                  "do.dev_roll_med_acc_x", "do.dev_roll_med_acc_y", "do.dev_roll_med_acc_z",
                                                  "do.bfen", "do.hfen", "do.hfenplus", "do.lfen",
                                                  "do.lfx", "do.lfy", "do.lfz", "do.hfx", "do.hfy", "do.hfz",
                                                  "do.bfx", "do.bfy", "do.bfz", "do.brondcounts")]))
      if (Nmetrics2calc > 4) { #Only give warning when user wants more than 4 metrics.
        warning(paste0("\nExtracting many metrics puts higher demands on memory. Please consider",
                       " reducing the value for argument chunksize or setting do.parallel to FALSE"))
      }
      if (params_rawdata[["chunksize"]] > 0.6 & Nmetrics2calc < 3) { # default ENMO and anglez
        params_rawdata[["chunksize"]] = 0.6 # put limit to chunksize, because when processing in parallel memory is more limited
      } else if (params_rawdata[["chunksize"]] > 0.6 & Nmetrics2calc >= 3 & Nmetrics2calc < 6) { # if user wants to extract 3-5 metrics
        params_rawdata[["chunksize"]] = 0.5 # put limit to chunksize, because when processing in parallel memory is more limited
      } else if (params_rawdata[["chunksize"]] > 0.6 & Nmetrics2calc >= 6) { # if user wants to extract more than 5 metrics
        params_rawdata[["chunksize"]] = 0.4 # put limit to chunksize, because when processing in parallel memory is more limited
      }
      if (length(params_general[["maxNcores"]]) == 0) params_general[["maxNcores"]] = Ncores
      Ncores2use = min(c(Ncores - 1, params_general[["maxNcores"]], (f1 - f0) + 1))
      cl <- parallel::makeCluster(Ncores2use) #not to overload your computer
      doParallel::registerDoParallel(cl)
      
    } else {
      cat(paste0("\nparallel processing not possible because number of available cores (",Ncores,") < 4"))
      params_general[["do.parallel"]] = FALSE
    }
    cat(paste0('\n Busy processing ... see ', outputdir, outputfolder,'/meta/basic', ' for progress\n'))
    `%myinfix%` = ifelse(params_general[["do.parallel"]], foreach::`%dopar%`, foreach::`%do%`)
    output_list = foreach::foreach(i = f0:f1, .packages = packages2passon,
                                   .export = functions2passon, .errorhandling = errhand) %myinfix% {
                                     tryCatchResult = tryCatch({
                                       main_part1(i, params_metrics, params_rawdata,
                                                  params_cleaning, params_general, datadir, fnames, fnamesfull,
                                                  outputdir, myfun, filelist, studyname, ffdone, tmp5, tmp6,
                                                  use.temp, daylimit, path3, outputfolder, is.mv)
                                     }) 
                                     return(tryCatchResult)
                                   }
    on.exit(parallel::stopCluster(cl))
    for (oli in 1:length(output_list)) { # logged error and warning messages
      if (is.null(unlist(output_list[oli])) == FALSE) {
        cat(paste0("\nErrors and warnings for ",fnames[oli]))
        print(unlist(output_list[oli])) # print any error and warnings observed
      }
    }
  } else { # simple loop to process files serially file by file
    for (i in f0:f1) {
      cat(paste0(i, " "))
      main_part1(i, params_metrics, params_rawdata,
                 params_cleaning, params_general, datadir, fnames, fnamesfull,
                 outputdir, myfun, filelist, studyname, ffdone, tmp5, tmp6,
                 use.temp, daylimit, path3, outputfolder, is.mv)
      
    }
  }
}
