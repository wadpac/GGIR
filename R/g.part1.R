g.part1 = function(datadir = c(), metadatadir = c(), f0 = 1, f1 = c(), myfun = c(),
                   params_metrics = c(), params_rawdata = c(),
                   params_cleaning = c(), params_general = c(), verbose = TRUE, ...) {
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
  
  if (f1 == 0) warning("\nWarning: f1 = 0 is not a meaningful value")
  filelist = isfilelist(datadir)
  if (ismovisens(datadir)) filelist = TRUE

  # list all accelerometer files
  dir2fn = datadir2fnames(datadir, filelist)
  fnames = dir2fn$fnames
  fnamesfull = dir2fn$fnamesfull
  filesizes = file.size(fnamesfull) # in bytes
  bigEnough = which(filesizes/1e6 > params_rawdata[["minimumFileSizeMB"]])
  fnamesfull = fnamesfull[bigEnough]
  
  if (length(bigEnough) > 0) {
    fnames_toosmall = fnames[-bigEnough]
  } else {
    fnames_toosmall = fnames
  }
  
  fnames = fnames[bigEnough]

  if (verbose && length(fnames_toosmall) > 0) {
    warning(paste0("\nSkipping files that are too small for analysis: ", toString(fnames_toosmall),
                   " (configurable with parameter minimumFileSizeMB)."), call. = FALSE) 
  }

  if (length(fnamesfull) == 0) {
    stop(paste0("\nNo files to analyse. Check that there are accelerometer files ",
                "in the directory specified with argument datadir"), call. = FALSE)
  }

  # create output directory if it does not exist
  if (!file.exists(metadatadir)) {
    dir.create(metadatadir)
    dir.create(file.path(metadatadir, "meta"))
    dir.create(file.path(metadatadir, "meta", "basic"))
    dir.create(file.path(metadatadir, "results"))
    dir.create(file.path(metadatadir, "results", "QC"))
  }
  use.temp = TRUE
  daylimit = FALSE

  # check access permissions
  Nfile_without_readpermission = length(which(file.access(paste0(fnamesfull), mode = 4) == -1)) #datadir,"/",
  if (Nfile_without_readpermission > 0) {
    if (verbose == TRUE) cat("\nChecking that user has read access permission for all files in data directory: No")
    warning(paste0("\nThere are/is ", Nfile_without_readpermission,
                   " file(s) in directory specified with argument datadir for which the user does not have read access permission\n"))
  } else {
    if (verbose == TRUE) cat("\nChecking that user has read access permission for all files in data directory: Yes\n")
  }

  if (length(f0) ==  0) f0 = 1
  if (length(f1) ==  0) f1 = length(fnames)
  if (f0 > length(fnames)) f0 = 1
  if (f1 > length(fnames)) f1 = length(fnames)
  #========================================================================
  # check which files have already been processed, such that no double work is done
  # ffdone a matrix with all the binary filenames that have been processed
  ffdone = dir(file.path(metadatadir, "meta", "basic"))

  if (length(ffdone) > 0) {
    for (ij in 1:length(ffdone)) {
      tmp = unlist(strsplit(ffdone[ij],".RData"))
      tmp2 = unlist(strsplit(tmp[1],"meta_"))
      ffdone[ij] = tmp2[2]
    }
  }

  # Following lines turned off because does not work when using subfolders
  # It seems best to leave sorting efforts until the very end when reports are created
  # fnames = sort(fnames)
  # fnamesfull = sort(fnamesfull)


  #=========================================================
  # Declare core functionality, which at the end of this g.part1 is either
  # applied to the file in parallel with foreach or serially with a loop
  main_part1 = function(i, params_metrics, params_rawdata,
                        params_cleaning, params_general, datadir, fnames, fnamesfull,
                        myfun, filelist, ffdone,
                        use.temp, daylimit, metadatadir, verbose) {
    tail_expansion_log = NULL
    if (params_general[["print.filename"]] == TRUE & verbose == TRUE) {
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
    if (params_general[["overwrite"]] == FALSE) {
      fname_without = unlist(strsplit(fnames[i], ".csv"))[1]
      fname_without = unlist(strsplit(fname_without,"[.]RD"))[1]

      ffdone_without = unlist(strsplit(ffdone, ".csv"))

      if (length(which(ffdone_without == fname_without)) > 0) {
        return() # skip this file because it was analysed before
      } 
    }
    #=============================================================
    # Inspect file (and store output later on)
    I = g.inspectfile(datafile, desiredtz = params_general[["desiredtz"]],
                        params_rawdata = params_rawdata,
                        configtz = params_general[["configtz"]])
    if (verbose == TRUE) cat(paste0("\nP1 file ",i))
    turn.do.cal.back.on = FALSE
    if (params_rawdata[["do.cal"]] == TRUE & I$dformc == FORMAT$WAV) { # do not do the auto-calibration for wav files (because already done in pre-processign)
      params_rawdata[["do.cal"]] = FALSE
      turn.do.cal.back.on = TRUE
    }
    data_quality_report_exists = file.exists(paste0(metadatadir, "/results/QC/data_quality_report.csv"))
    assigned.backup.cal.coef = FALSE
    if (length(params_rawdata[["backup.cal.coef"]]) > 0) {
      if (params_rawdata[["backup.cal.coef"]] == "retrieve") {
        if (data_quality_report_exists == TRUE) { # use the data_quality_report as backup for calibration coefficients
          params_rawdata[["backup.cal.coef"]] = paste0(metadatadir, "/results/QC/data_quality_report.csv")
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
    # Set default C object
    C = list(cal.error.end = 0, cal.error.start = 0)
    C$scale = c(1,1,1)
    C$offset = c(0,0,0)
    C$tempoffset =  c(0,0,0)
    C$QCmessage = "Autocalibration not done"
    C$npoints = 0
    C$nhoursused = 0
    C$use.temp = use.temp
    Cdefault = C
    if (params_rawdata[["do.cal"]] == TRUE & length(params_rawdata[["backup.cal.coef"]]) == 0 &
        is.null(I$sf) == FALSE) {
      # cat(paste0("\n",rep('-',options()$width),collapse=''))
      if (verbose == TRUE) cat("\n")
      if (verbose == TRUE) cat("\nInvestigate calibration of the sensors with function g.calibrate:\n")
      C = g.calibrate(datafile,
                      params_rawdata = params_rawdata,
                      params_general = params_general,
                      params_cleaning = params_cleaning,
                      inspectfileobject = I,
                      verbose = verbose)
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
      bcc.data = data.table::fread(params_rawdata[["backup.cal.coef"]], data.table = FALSE)
      if (isTRUE(params_rawdata[["do.cal"]]) & verbose == TRUE) {
        cat("\nRetrieving previously derived calibration coefficients")
      }
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
        bcc.meantempcali = which(colnames(bcc.data) == "meantempcal")
        bcc.QCmessagei = which(colnames(bcc.data) == "QCmessage")
        bcc.npointsi = which(colnames(bcc.data) == "n.10sec.windows")
        bcc.nhoursusedi = which(colnames(bcc.data) == "n.hours.considered")
        bcc.use.tempi = which(colnames(bcc.data) == "use.temperature")
        C$scale = as.numeric(bcc.data[bcc.i[1],bcc.scalei])
        C$offset = as.numeric(bcc.data[bcc.i[1],bcc.offseti])
        C$tempoffset =  as.numeric(bcc.data[bcc.i[1],bcc.temp.offseti])
        C$meantempcal = bcc.data[bcc.i[1], bcc.meantempcali]
        C$cal.error.start = as.numeric(bcc.data[bcc.i[1],bcc.cal.error.start])
        C$cal.error.end = as.numeric(bcc.data[bcc.i[1],bcc.cal.error.end])
        C$QCmessage = bcc.data[bcc.i[1],bcc.QCmessagei]
        C$npoints = bcc.data[bcc.i[1], bcc.npointsi]
        C$nhoursused = bcc.data[bcc.i[1], bcc.nhoursusedi]
        C$use.temp = bcc.data[bcc.i[1], bcc.use.tempi]
        if (verbose == TRUE) {
          cat(paste0("\nRetrieved Calibration error (g) before: ",as.numeric(bcc.data[bcc.i[1],bcc.cal.error.start])))
          cat(paste0("\nRetrieved Callibration error (g) after: ",as.numeric(bcc.data[bcc.i[1],bcc.cal.error.end])))
          cat(paste0("\nRetrieved offset correction ",c("x","y","z"),": ",C$offset))
          cat(paste0("\nRetrieved scale correction ",c("x","y","z"),": ",C$scale))
          cat(paste0("\nRetrieved tempoffset correction ",c("x","y","z"),": ",C$tempoffset))
          cat("\n")
        }
      } else {
        # cat("\nNo matching filename found in backup.cal.coef\n")
        # cat(paste0("\nCheck that filename ",fnames[i]," exists in the csv-file\n"))
        if (params_rawdata[["do.cal"]] == TRUE) { # If no matching filename could be found, then try to derive the calibration coeficients in the normal way
          if (verbose == TRUE) {
            cat("\n")
            cat("\nInvestigate calibration of the sensors with function g.calibrate:\n")
          }
          C = g.calibrate(datafile,
                          params_rawdata = params_rawdata,
                          params_general = params_general,
                          params_cleaning = params_cleaning,
                          inspectfileobject = I,
                          verbose = verbose)
          if (is.null(C)) C = Cdefault # in case gt3x file is corrupt then this value is needed
        }
      }
    }
    #------------------------------------------------
    if (verbose == TRUE) cat("\nExtract signal features (metrics) with the g.getmeta function:\n")
    M = g.getmeta(datafile,
                  params_metrics = params_metrics,
                  params_rawdata = params_rawdata,
                  params_general = params_general,
                  params_cleaning = params_cleaning,
                  daylimit = daylimit,
                  tempoffset = C$tempoffset, scale = C$scale, offset = C$offset,
                  meantempcal = C$meantempcal,
                  myfun = myfun,
                  inspectfileobject = I,
                  verbose = verbose)

    if (!is.null(params_general[["recordingEndSleepHour"]])) {
      # Identify gap between last timestamp and following midnight
      ws3 = M$windowsizes[1]
      ws2 = M$windowsizes[2]
      # Check whether gap is less then criteria
      last_ts = c(Sys.time(), Sys.time())
      secs_to_midnight = c(0, 0)
      lastTimeLong = as.character(tail(M$metalong$timestamp, n = 1))
      lastTimeShort = as.character(tail(M$metashort$timestamp, n = 1))
      last_ts[1] = iso8601chartime2POSIX(x = lastTimeLong, tz = params_general[["desiredtz"]])
      last_ts[2] = iso8601chartime2POSIX(x = lastTimeShort, tz = params_general[["desiredtz"]])
      refhour = 24 + params_general[["dayborder"]]
      for (wsi in 1:2) {
        secs_to_midnight[wsi] = (refhour * 3600) -
          (as.numeric(format(last_ts[wsi], format = "%H", tz = params_general[["desiredtz"]])) * 3600 +
             as.numeric(format(last_ts[wsi], format = "%M", tz = params_general[["desiredtz"]])) * 60  +
             as.numeric(format(last_ts[wsi], format = "%S", tz = params_general[["desiredtz"]])))
      }
      # only expand if recording ends at 19PM or later
      max_expand_time = (refhour - (params_general[["recordingEndSleepHour"]] - params_general[["dayborder"]])) * 3600
      if (secs_to_midnight[1] <= max_expand_time) {
        # If yes, expand data
        secs_to_midnight = secs_to_midnight + (8 * 3600) # also add 8 hour till the morning
        N_long_epochs_expand = ceiling(secs_to_midnight[1] / ws2) + 1
        N_short_epochs_expand = ceiling(secs_to_midnight[2] / ws3) + 1
        if (N_short_epochs_expand / (ws2 / ws3) < N_long_epochs_expand) {
          N_short_epochs_expand = N_long_epochs_expand * (ws2 / ws3)
        }
        # N_short_epochs_expand = ((N_long_epochs_expand) * (ws2/ws3))
        # Expand metashort
        NR = nrow(M$metashort)
        metashort_expand = M$metashort[NR,]
        metashort_expand[, grep(pattern = "timestamp|angle", x = names(metashort_expand), invert = TRUE, value = FALSE)] = 0
        if ("EN" %in% names(metashort_expand)) {
          metashort_expand$EN = 1
        }
        expand_indices = (NR + 1):(NR + N_short_epochs_expand)
        expand_tsPOSIX = seq(last_ts[2] + ws3, last_ts[2] + (N_short_epochs_expand * ws3), by = ws3)
        M$metashort[expand_indices,] = metashort_expand
        M$nonwear[expand_indices] = 0
        M$metashort$timestamp[expand_indices] = POSIXtime2iso8601(expand_tsPOSIX, tz = params_general[["desiredtz"]])
        anglecol = grep(pattern = "angle", x = names(metashort_expand), value = FALSE)
        if (length(anglecol) > 0) {
          M$metashort[expand_indices,anglecol] = round(sin((1:length(expand_indices)) / (ws2/ws3))) * 15
        }
        tail_expansion_log = list(short = length(expand_indices))
        # Expand metalong
        NR = nrow(M$metalong)
        metalong_expand = M$metalong[NR,]
        metalong_expand[, grep(pattern = "timestamp", x = names(metalong_expand), invert = TRUE, value = FALSE)] = 0
        metalong_expand[, "nonwearscore"] = -1
        metalong_expand$en = tail(M$metalong$en, n = 1)
        expand_indices = (NR + 1):(NR + N_long_epochs_expand)
        expand_tsPOSIX = seq(last_ts[1] + ws2, last_ts[1] + (N_long_epochs_expand * ws2), by = ws2)
        M$metalong[expand_indices,] = metalong_expand
        M$metalong$timestamp[expand_indices] = POSIXtime2iso8601(expand_tsPOSIX, tz = params_general[["desiredtz"]])
        # Keep log of data expansion
        tail_expansion_log[["long"]] = length(expand_indices)
      } else {
        tail_expansion_log = NULL
      }
    }
    #------------------------------------------------
    if (verbose == TRUE) cat("\nSave .RData-file with: calibration report, file inspection report and all signal features...\n")
    # remove directory in filename if present
    filename = unlist(strsplit(fnames[i],"/"))
    if (length(filename) > 0) {
      filename = filename[length(filename)]
    } else {
      filename = fnames[i]
    }
    filefoldername = filename_dir = fnames[i] # not used elsewhere in GGIR, but just keeping it for consistency
    if (length(unlist(strsplit(fnames[1], "[.]RD"))) == 1) { # to avoid getting .RData.RData
      filename = paste0(filename,".RData")
    }
    GGIRversion = utils::packageVersion("GGIR")
    desiredtz_part1 = params_general[["desiredtz"]]
    save(M, I, C, filename_dir, filefoldername, tail_expansion_log, GGIRversion,
         desiredtz_part1, file = paste0(metadatadir, "/meta/basic/meta_", filename))
    rm(M); rm(I); rm(C)
  } # end of main_part1

  #--------------------------------------------------------------------------------
  # Run the code either parallel or in serial (file index starting with f0 and ending with f1)
  if (params_general[["do.parallel"]] == TRUE) {
    cores = parallel::detectCores()
    Ncores = cores[1]
    if (Ncores > 3) {

      if (length(params_general[["maxNcores"]]) == 0) params_general[["maxNcores"]] = Ncores
      Ncores2use = min(c(Ncores - 1, params_general[["maxNcores"]], (f1 - f0) + 1))
      if (Ncores2use > 1) {
        cl <- parallel::makeCluster(Ncores2use, type = "MPI") # not to overload your computer
        parallel::clusterExport(cl = cl, 
                                varlist = c(unclass(lsf.str(envir = asNamespace("GGIR"), all = T)),
                                            "MONITOR", "FORMAT"),
                                envir = as.environment(asNamespace("GGIR"))
        )
        parallel::clusterEvalQ(cl, Sys.setlocale("LC_TIME", "C"))
        doParallel::registerDoParallel(cl)
      } else {
        # Don't process in parallel if only one core
        params_general[["do.parallel"]] = FALSE
      }
    } else {
      if (verbose == TRUE) cat(paste0("\nparallel processing not possible because number of available cores (",Ncores,") < 4"))
      params_general[["do.parallel"]] = FALSE
    }
  }
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
      functions2passon = c("g.inspectfile", "g.calibrate","g.getmeta", "g.dotorcomma",
                           "g.applymetrics",
                           "g.readaccfile", "updateBlocksize",
                           "g.getstarttime", "POSIXtime2iso8601",
                           "iso8601chartime2POSIX", "datadir2fnames", "read.myacc.csv",
                           "get_nw_clip_block_params",
                           "get_starttime_weekday_truncdata", "ismovisens",
                           "g.extractheadervars", "g.imputeTimegaps", "extract_params",
                           "load_params",
                           "check_params", "detect_nonwear_clipping", "inspect_binFile_brand")
      errhand = 'stop'
      # Note: This will not work for cwa files, because those also need Rcpp functions.
      # So, it is probably best to turn off parallel when debugging cwa data.
    }
    Nmetrics2calc = sum(unlist(params_metrics[c("do.anglex", "do.angley", "do.anglez",
                                                "do.zcx", "do.zcy", "do.zcz",
                                                "do.enmo", "do.lfenmo", "do.en", "do.mad", "do.enmoa",
                                                "do.roll_med_acc_x", "do.roll_med_acc_y", "do.roll_med_acc_z",
                                                "do.dev_roll_med_acc_x", "do.dev_roll_med_acc_y", "do.dev_roll_med_acc_z",
                                                "do.bfen", "do.hfen", "do.hfenplus", "do.lfen",
                                                "do.lfx", "do.lfy", "do.lfz", "do.hfx", "do.hfy", "do.hfz",
                                                "do.bfx", "do.bfy", "do.bfz", "do.brondcounts",
                                                "do.neishabouricounts")]))
    if (Nmetrics2calc > 4) { #Only give warning when user wants more than 4 metrics.
      warning(paste0("\nExtracting many metrics puts higher demands on memory. Please consider",
                     " reducing the value for argument chunksize or setting do.parallel to FALSE"))
    }
    if (params_rawdata[["chunksize"]] > 0.6 & Nmetrics2calc >= 3 & Nmetrics2calc < 6) { # if user wants to extract 3-5 metrics
      params_rawdata[["chunksize"]] = 0.5 # put limit to chunksize, because when processing in parallel memory is more limited
    } else if (params_rawdata[["chunksize"]] > 0.6 & Nmetrics2calc >= 6) { # if user wants to extract more than 5 metrics
      params_rawdata[["chunksize"]] = 0.4 # put limit to chunksize, because when processing in parallel memory is more limited
    }


    if (verbose == TRUE) cat(paste0('\n Busy processing ... see ', metadatadir,'/meta/basic', ' for progress\n'))
    `%myinfix%` = foreach::`%dopar%`
    output_list = foreach::foreach(i = f0:f1, .packages = packages2passon,
                                   .export = functions2passon, .errorhandling = errhand) %myinfix% {
                                     tryCatchResult = tryCatch({
                                       main_part1(i, params_metrics, params_rawdata,
                                                  params_cleaning, params_general, datadir, fnames, fnamesfull,
                                                  myfun, filelist, ffdone,
                                                  use.temp, daylimit, metadatadir, verbose)
                                     })
                                     return(tryCatchResult)
                                   }
    on.exit(parallel::stopCluster(cl))
    for (oli in 1:length(output_list)) { # logged error and warning messages
      if (is.null(unlist(output_list[oli])) == FALSE) {
        if (verbose == TRUE) cat(paste0("\nErrors and warnings for ",fnames[oli]))
        print(unlist(output_list[oli])) # print any error and warnings observed
      }
    }
  } else { # simple loop to process files serially file by file
    errors = list()
    for (i in f0:f1) {
      if (verbose == TRUE) cat(paste0(i, " "))
      function_to_evaluate = expression(
        main_part1(i, params_metrics, params_rawdata,
                   params_cleaning, params_general, datadir, fnames, fnamesfull,
                   myfun, filelist, ffdone,
                   use.temp, daylimit, metadatadir, verbose)
      )
      if (params_general[["use_trycatch_serial"]] == TRUE) {
        tryCatch(
          eval(function_to_evaluate),
          error = function(e) {
            err_msg = conditionMessage(e)
            errors[[as.character(fnames[i])]] <<- err_msg
          }
        )
      } else {
        eval(function_to_evaluate)
      }
    }
    # show logged errors after the loop:
    if (params_general[["use_trycatch_serial"]] == TRUE && verbose == TRUE) {
      if (length(errors) > 0) {
        cat(paste0("\n\nErrors in part 1... for:"))
        cat(paste0("\n-", names(errors), ": ", unlist(errors), collapse = ""))
      }
    }
  }
}
