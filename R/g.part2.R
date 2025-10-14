g.part2 = function(datadir = c(), metadatadir = c(), f0 = c(), f1 = c(),
                   myfun=c(), params_cleaning = c(), params_247 = c(),
                   params_phyact = c(), params_output = c(), params_general = c(),
                   verbose = TRUE, ...) {
  
  #----------------------------------------------------------
  # Extract and check parameters
  input = list(...)
  params = extract_params(params_cleaning = params_cleaning,
                          params_247 = params_247,
                          params_phyact = params_phyact,
                          params_output = params_output,
                          params_general = params_general, input = input,
                          params2check = c("cleaning", "247", "phyact", "output", "general")) # load default parameters
  params_cleaning = params$params_cleaning
  params_247 = params$params_247
  params_phyact = params$params_phyact
  params_output = params$params_output
  params_general = params$params_general
  #-----------------------------
  use_qwindow_as_diary = TRUE # If there is a diary specified via qwindow use it as qwindow
  qwindow = qwindow_original = NULL
  if (is.numeric(params_247[["qwindow"]])) {
    params_247[["qwindow"]] = params_247[["qwindow"]][order(params_247[["qwindow"]])]
  } else if (is.character(params_247[["qwindow"]])) {
    if (length(grep(pattern = "onlyfilter|filteronly", x = params_247[["qwindow"]])) > 0) {
      # Do not use diary specified for qwindow if it has the word
      # "onlyfilter" or "filteronly", but use it for filterning nighttime nonwear
      # note that this filtering is only use if parameter nonwearFiltermaxHours is specified.
      use_qwindow_as_diary = FALSE 
    }
    tmp_activityDiary_file = paste0(metadatadir, "/meta/activityDiary_", basename(params_247[["qwindow"]]), ".RData")
    
    if (!file.exists(tmp_activityDiary_file) || (file.exists(tmp_activityDiary_file) && 
                                                 file.info(params_247[["qwindow"]])$mtime >= file.info(tmp_activityDiary_file)$mtime)) {
      if (verbose == TRUE) cat("\nConverting activity diary...")
      # This will be an object with numeric qwindow values for all individuals and days
      qwindow = g.conv.actlog(params_247[["qwindow"]],
                                              params_247[["qwindow_dateformat"]],
                                              epochSize = params_general[["windowsizes"]][1])
      save(qwindow, file = tmp_activityDiary_file)
    } else {
      load(tmp_activityDiary_file) # loads qwindow
    }
    qwindow_original = params_247[["qwindow"]]
    params_247[["qwindow"]] = qwindow 
  }

  #---------------------------------------------------------------
  # If non-wear filter is requested, the same diary may be needed as for qwindow
  if (!is.null(params_cleaning[["nonwearFiltermaxHours"]])) {
    if (!is.null(params_cleaning[["nonwearFilterWindow"]])) {
      # user specified nonwearFilterWindow
      # If nonwearFilterWindow is numeric, then it can be used as such
      # If nonwearFilterWindow is a file path then check whether it is the same as for qwindow
      if (is.character(params_cleaning[["nonwearFilterWindow"]])) {
        if (!is.null(params_247[["qwindow"]]) &&
            is.character(qwindow_original) &&
            !is.null(qwindow) &&
            params_cleaning[["nonwearFilterWindow"]] == qwindow_original) {
          # If qwindow is specified as diary and this diary path is the same
          # as the one specified for nonwearFilterWindow, reuse qwindow object loaded above
          params_cleaning[["nonwearFilterWindow"]] = qwindow
        }  else {
          # Alternatively, load diary as specified
          tmp_nonwearFilterDiary_file =  paste0(metadatadir, "/meta/nonwearFilterDiary_",
                                                basename(params_cleaning[["nonwearFilterWindow"]]), ".RData")
          if (!file.exists(tmp_nonwearFilterDiary_file) || 
              (file.exists(tmp_nonwearFilterDiary_file) &&
               file.info(params_cleaning[["nonwearFilterWindow"]])$mtime >= file.info(tmp_nonwearFilterDiary_file)$mtime)) {
            nonwearFilterWindow = g.conv.actlog(params_cleaning[["nonwearFilterWindow"]],
                                                params_247[["qwindow_dateformat"]],
                                                epochSize = params_general[["windowsizes"]][1])
            save(nonwearFilterWindow, file = tmp_nonwearFilterDiary_file)
          } else {
            # reload if previously stored
            load(file = tmp_nonwearFilterDiary_file)
          }
          params_cleaning[["nonwearFilterWindow"]] = nonwearFilterWindow
        }
      }
    } else {
      # user did not specify nonwearFilterWindow, try to use qwindow instead:
      if (!is.null(params_247[["qwindow"]]) &&
          is.character(qwindow_original) &&
          !is.null(qwindow)) {
        params_cleaning[["nonwearFilterWindow"]] = qwindow
      } else {
        stop(paste0("Parameters nonwearFilterWindow is not specified and ",
                    "qwindow is not specified with a file path, either of these ",
                    "is needed when working with nonwearFiltermaxHours"), call. = FALSE)
      }
    }
  }
  #---------------------------------------------------------------
  # Now we no longer need file path stored in params_247[["qwindow"]]
  # it is time to overwrite it with the data.frame used inside part 2
  if (exists("qwindow")) {
    rm(qwindow)
  }
  
  #---------------------------------
  # Specifying directories with meta-data and extracting filenames
  path = paste0(metadatadir,"/meta/basic/")  #values stored per long epoch, e.g. 15 minutes
  checkMilestoneFolders(metadatadir, partNumber = 2)
  fnames = dir(path)
  if (f1 > length(fnames)) f1 = length(fnames)
  # create output folders
  ffdone = c()
  ms2.out = "/meta/ms2.out"
  if (file.exists(paste0(metadatadir,ms2.out)) == FALSE) {
    dir.create(file.path(metadatadir,ms2.out))
  }
  csvfolder = "/meta/csv"
  if (params_output[["epochvalues2csv"]] == TRUE) {
    if (file.exists(paste0(metadatadir, csvfolder)) == FALSE) {
      dir.create(file.path(metadatadir,csvfolder))
    }
  }
  ffdone = dir(paste0(metadatadir,ms2.out))
  if (params_output[["do.part2.png"]] == TRUE) {
    QC_plotfolder = paste0(metadatadir, "/results/QC/plots_part2")
    if (!dir.exists(QC_plotfolder)) {
      dir.create(QC_plotfolder)
    }
  }
  #---------------------------------
  # house keeping variables
  daySUMMARY = c()
  if (length(f0) == 0) f0 = 1
  if (length(f1) == 0) f1 = length(fnames)
  #--------------------------------
  # get full file path and folder name if requested by end-user and keep this for storage in output
  foldername = fullfilenames = folderstructure = referencefnames = c()
  if (params_output[["storefolderstructure"]] == TRUE) {
    extractfilenames = function(x) {
      x2 = as.character(unlist(strsplit(x,".RDa"))[1])
      x3 = unlist(strsplit(x2,"eta_"))[2]
      return(x3)
    }
    referencefnames = as.character(sapply(fnames, extractfilenames))
    folderstructure = getfolderstructure(datadir, referencefnames)
    fullfilenames = folderstructure$fullfilenames
    foldername = folderstructure$foldername
  }
  #--------------------------------
  # Loop through all the files
  # fnames = sort(fnames)
  if (f1 > length(fnames)) f1 = length(fnames)
  if (f0 > f1) f0 = 1
  
  #---------------------------------------
  cnt78 = 1
  
  #=========================================================
  # Declare core functionality, which at the end of this g.part2 is either
  # applied to the file in parallel with foreach or serially with a loop
  main_part2 = function(i, ffdone, fnames,
                        metadatadir = c(),
                        myfun=c(), params_cleaning = c(), params_247 = c(),
                        params_phyact = c(), params_output = c(), params_general = c(),
                        path, ms2.out, foldername, fullfilenames, folderstructure, referencefnames,
                        daySUMMARY, csvfolder, cnt78, verbose, use_qwindow_as_diary) {
    Nappended = I_list = tail_expansion_log = desiredtz_part1 = NULL
    if (length(ffdone) > 0) {
      if (length(which(ffdone == as.character(unlist(strsplit(fnames[i], "eta_"))[2]))) > 0) {
        skip = 1 #skip this file because it was analysed before")
      } else {
        skip = 0 #do not skip this file
      }
    } else {
      skip = 0
    }
    if (params_general[["overwrite"]] == TRUE) skip = 0
    if (skip == 0) {
      M = I = c()
      filename_dir = c()
      filefoldername = c()
      file2read = paste0(path,fnames[i])
      load(file2read) #reading RData-file
      if (!is.null(desiredtz_part1)) {
        params_general[["desiredtz"]] = desiredtz_part1
      }
      # convert to character/numeric if stored as factor in metashort and metalong
      M$metashort = correctOlderMilestoneData(M$metashort)
      M$metalong = correctOlderMilestoneData(M$metalong)
      # extract ID centrally to be used in GGIR
      if (is.null(I$sf)) {
        M$filecorrupt = TRUE
      } else {
        hvars = g.extractheadervars(I)
        ID = extractID(hvars = hvars, idloc = params_general[["idloc"]], fname = I$filename)
      }
      if (M$filecorrupt == FALSE & M$filetooshort == FALSE) {
        #-----------------------
        # If required by user, ignore specific timewindows for imputation and set them to zeroinstead:
        TimeSegments2Zero = c() # set defaul
        # Check whether csv file exists with start-end end times of timewindows to be ignored (where 0 movement will be assumed)
        if (length(params_cleaning[["TimeSegments2ZeroFile"]]) > 0) {
          TimeSegments2ZeroAll = data.table::fread(params_cleaning[["TimeSegments2ZeroFile"]], data.table = FALSE)
          # Check whether this individual is part of the file
          filei = which(TimeSegments2ZeroAll$filename == as.character(unlist(strsplit(fnames[i], "eta_"))[2]))
          if (length(filei) > 0) {
            # If yes, load the timestamps that indicate the windows to be ignored
            TimeSegments2Zero = TimeSegments2ZeroAll[filei,]
            # Check that they fall withint the measurement
            TimeSegments2Zero$windowstart = as.POSIXlt(TimeSegments2Zero$windowstart,tz = params_general[["desiredtz"]])
            TimeSegments2Zero$windowend = as.POSIXlt(TimeSegments2Zero$windowend,tz = params_general[["desiredtz"]])
            timespan0 = iso8601chartime2POSIX(M$metashort$timestamp[1], tz = params_general[["desiredtz"]])
            timespan1 = iso8601chartime2POSIX(M$metashort$timestamp[nrow(M$metashort)],
                                              tz = params_general[["desiredtz"]])
            validtimes = which(TimeSegments2Zero$windowstart > timespan0 &
                                 TimeSegments2Zero$windowend < timespan1)
            if (length(validtimes) > 0) {
              TimeSegments2Zero = TimeSegments2Zero[validtimes,c("windowstart","windowend")]
              
            } else {
              TimeSegments2Zero = c()
            }
          }
        }
        if (length(myfun) > 0) {
          for (mi in 1:length(myfun$outputtype)) {
            if (myfun$outputtype[mi] == "character") {
              # At the moment we do not have a strategy in place on how to impute categorical variables
              # produced by external functions. Therefore, for the moment ignore these variables until
              # there is a plan.
              M$metashort = M$metashort[,-which(names(M$metashort) %in% myfun$colnames == TRUE)]
            }
          }
        }
        qwindowImp = params_cleaning[["nonwearFilterWindow"]]
        if (use_qwindow_as_diary == FALSE) {
          # reset qwindow to default, because it is only used
          # for filtering short nighttime nonwear 
          params_247[["qwindow"]] = c(0, 24)
        }
        if (inherits(qwindowImp, "data.frame")) {
          qwindowImp = qwindowImp[which(qwindowImp$ID == ID),]
          if (nrow(qwindowImp) == 0) {
            qwindowImp = c(0, 6) # If participant not present in diary
          }
        } else {
          qwindowImp = NULL
        }
        IMP = g.impute(M, I,
                       params_cleaning = params_cleaning,
                       dayborder = params_general[["dayborder"]],
                       desiredtz = params_general[["desiredtz"]],
                       TimeSegments2Zero = TimeSegments2Zero,
                       acc.metric = params_general[["acc.metric"]],
                       ID = ID, qwindowImp = qwindowImp)
        
        if (params_output[["do.part2.png"]] == TRUE & length(IMP) > 0) {
          Ndays = (nrow(M$metalong) * M$windowsizes[2]) / (3600 * 24)
          if (params_cleaning[["maxdur"]] != 0) {
            durplot = params_cleaning[["maxdur"]]
          } else {
            durplot = 7 #how many DAYS to plot? (only used if maxdur is not specified as a number above zero)
          }
          IDname = gsub(pattern = "meta_|[.]bin|[.]RData|[.]csv|[.]gt3x", replacement = "", x = fnames[i])
          png(paste0(QC_plotfolder, "/plot_", IDname, ".png"),
              width = 7, height = 7, units = "in", res = 300)
          g.plot(IMP, M, I,
                 durplot = ifelse(test = Ndays > durplot, yes = Ndays, no = durplot))
          dev.off()
        }
        if (params_cleaning[["do.imp"]] == FALSE) { #for those interested in sensitivity analysis
          IMP$metashort = M$metashort
          # IMP$metalong = M$metalong
        }
        # do not use expanded time with expand_tail_max_hours in summary reports
        if (length(tail_expansion_log) != 0) {
          M_bu = M; IMP_bu = IMP # back up to be reversed later on
          expanded_time_short = which(IMP$r5long == -1)
          expanded_time_long = which(IMP$rout$r5 == -1)
          M$metashort = M$metashort[-expanded_time_short,]
          IMP$metashort = IMP$metashort[-expanded_time_short,]
          M$metalong = M$metalong[-expanded_time_long,]
          IMP$rout = IMP$rout[-expanded_time_long,]
        }
        
        SUM = g.analyse(I, C, M, IMP,
                        params_247 = params_247,
                        params_phyact = params_phyact,
                        params_general = params_general,
                        params_cleaning = params_cleaning,
                        myfun = myfun, ID = ID)
        RDname = as.character(unlist(strsplit(fnames[i], "eta_"))[2])
        # reset M and IMP so that they include the expanded time (needed for sleep detection in parts 3 and 4)
        if (length(tail_expansion_log) != 0) {
          M = M_bu
          IMP = IMP_bu
        }
        if (params_output[["epochvalues2csv"]] == TRUE) {
          if (length(IMP$metashort) > 0) {
            data.table::fwrite(IMP$metashort, paste0(metadatadir, "/", csvfolder, "/", RDname, ".csv"),
                               row.names = FALSE, sep = params_output[["sep_reports"]],
                               dec = params_output[["dec_reports"]])
          }
        }
        if (M$filecorrupt == FALSE & M$filetooshort == FALSE) {
          if (cnt78 == 1) {
            SUMMARY = SUM$summary
            daySUMMARY = SUM$daysummary
            cnt78 = 2
          }
        }
        
        NumberRDinFilename = length(unlist(strsplit(RDname,"[.]RD")))
        if (NumberRDinFilename == 1) { # to avoid getting .RData.RData
          RDname = paste0(RDname,".RData")
        }
        if (params_output[["storefolderstructure"]] == TRUE) {
          SUM$daysummary$filename_dir = fullfilenames[i] #full filename structure
          SUM$daysummary$foldername = foldername[i] #lowest foldername
          SUM$summary$filename_dir = fullfilenames[i]
          SUM$summary$foldername = foldername[i]
        }
        if (!is.null(params_general[["maxRecordingInterval"]])) {
          getValue = function(x, name) {
            if (name %in% names(x)) {
              out = x[name]
            } else {
              out = NULL
            }
            return(out)
          }
          if (!is.null(I_list)) {
            # This recording is the result of appending 2 or more recordings
            NappendedRecordings = Nappended
            sf_appendedRecordings = paste0(unlist(lapply(X = I_list, FUN = getValue, name = "sf")), collapse = " ")
            names_appendedRecordings = paste0(unlist(lapply(X = I_list, FUN = getValue, name = "filename")), collapse = " ")
            # Note if overlap is positive it is overlap, if it is negative there was a gap
            overlap_appendedRecordings = paste0(unlist(lapply(X = I_list[[1]], FUN = getValue, name = "interval")), collapse = " ")
          } else {
            # This recording is has not been appended
            NappendedRecordings = 0
            sf_appendedRecordings = getValue(I, "sf")
            names_appendedRecordings = getValue(I, "filename")
            overlap_appendedRecordings = ""
          }
          SUM$summary$NappendedRecordings = NappendedRecordings
          SUM$summary$sf_appendedRecordings  = sf_appendedRecordings
          SUM$summary$names_appendedRecordings = names_appendedRecordings
          SUM$summary$overlap_hrs_appendedRecordings = overlap_appendedRecordings
        }
        # convert daysummary and summary variables to numeric
        char2num_df = function(df) {
          # save as numeric columns that can be coerced to numeric
          df = lapply(df, function(x) tryCatch(as.numeric(as.character(x)), 
                                               error = function(cond) return(x),
                                               warning = function(cond) return(x)))
          # list to data frame
          df = as.data.frame(df, check.names = FALSE)
          return(df)
        }
        # Only apply this to column that are not ID because ID can be number
        # combined with letter E, which would then resolve to numeric
        noID = which(colnames(SUM$summary) != "ID")
        SUM$summary[noID] = char2num_df(SUM$summary[noID])
        noIDday = which(colnames(SUM$daysummary) != "ID")
        SUM$daysummary[noIDday] = char2num_df(SUM$daysummary[noIDday])
        GGIRversion = utils::packageVersion("GGIR")
        save(SUM, IMP, tail_expansion_log, GGIRversion, file = paste0(metadatadir, ms2.out, "/", RDname)) #IMP is needed for g.plot in g.report.part2
      }
      if (M$filecorrupt == FALSE & M$filetooshort == FALSE) rm(IMP)
      rm(M); rm(I)
    }
  } # end of main_part2
  
  #--------------------------------------------------------------------------------
  # Run the code either parallel or in serial (file index starting with f0 and ending with f1)
  cores = parallel::detectCores()
  Ncores = cores[1]
  if (params_general[["do.parallel"]] == TRUE) {
    if (Ncores > 3) {
      if (length(params_general[["maxNcores"]]) == 0) params_general[["maxNcores"]] = Ncores
      Ncores2use = min(c(Ncores - 1, params_general[["maxNcores"]], (f1 - f0) + 1))
      if (Ncores2use > 1) {
        cl <- parallel::makeCluster(Ncores2use) # not to overload your computer
        parallel::clusterExport(cl = cl,
                                varlist = c(unclass(lsf.str(envir = asNamespace("GGIR"), all = T)),
                                            "MONITOR", "FORMAT"),
                                envir = as.environment(asNamespace("GGIR")))
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
    if (verbose == TRUE) cat(paste0('\n Busy processing ... see ', metadatadir, ms2.out, ' for progress\n'))
    # check whether we are indevelopment mode:
    GGIRinstalled = is.element('GGIR', installed.packages()[,1])
    packages2passon = functions2passon = NULL
    GGIRloaded = "GGIR" %in% .packages()
    if (GGIRloaded) { #pass on package
      packages2passon = 'GGIR'
      errhand = 'pass'
    } else {
      # pass on functions
      functions2passon = c("g.analyse", "g.impute", "g.weardec", "g.detecmidnight",
                           "g.extractheadervars", "g.analyse.avday", "g.getM5L5", "g.IVIS",
                           "g.analyse.perday", "g.getbout", "g.analyse.perfile", "g.intensitygradient",
                           "iso8601chartime2POSIX", "extract_params", "load_params", "check_params",
                           "correctOlderMilestoneData", "cosinor_IS_IV_Analyses", "extractID")
      errhand = 'stop'
    }
    i = 0 # declare i because foreach uses it, without declaring it
    `%myinfix%` = foreach::`%dopar%`
    output_list = foreach::foreach(i = f0:f1, .packages = packages2passon,
                                   .export = functions2passon, .errorhandling = errhand, .verbose = F) %myinfix% {
                                     tryCatchResult = tryCatch({
                                       main_part2(i, ffdone, fnames, metadatadir,
                                                  myfun, params_cleaning, params_247,
                                                  params_phyact, params_output, params_general,
                                                  path, ms2.out, foldername, fullfilenames,
                                                  folderstructure, referencefnames,
                                                  daySUMMARY, csvfolder, cnt78,
                                                  verbose, use_qwindow_as_diary)
                                       
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
  } else {
    errors = list()
    for (i in f0:f1) {
      if (verbose == TRUE) cat(paste0(i, " "))
      function_to_evaluate = expression(
        main_part2(i, ffdone, fnames, metadatadir,
                   myfun, params_cleaning, params_247,
                   params_phyact, params_output, params_general,
                   path, ms2.out, foldername, fullfilenames,
                   folderstructure, referencefnames,
                   daySUMMARY, csvfolder, 
                   cnt78, verbose, use_qwindow_as_diary)
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
        cat(paste0("\n\nErrors in part 2... for:"))
        cat(paste0("\n-", names(errors), ": ", unlist(errors), collapse = ""))
      }
    }
  }
}
