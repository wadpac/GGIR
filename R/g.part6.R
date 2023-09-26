g.part6 = function(datadir = c(), metadatadir = c(), f0 = c(), f1 = c(),
                   params_general = c(), params_phyact = c(), params_247 = c(),
                   verbose = TRUE, ...) {
  
  # This function called by function GGIR
  # and aims to facilitate time-pattern analysis building on the labelled time
  # series derived in GGIR part 5
  #----------------------------------------------------------
  # Extract and check parameters
  input = list(...)
  params = extract_params(params_general = params_general,
                          params_phyact = params_phyact,
                          params_247 = params_247,
                          input = input, params2check = c("general", "phyact", "247"))
  params_general = params$params_general
  params_phyact = params$params_phyact
  params_247 = params$params_247
  
  #======================================================================
  # create new folder (if not existent) for storing milestone data
  ms6.out = "/meta/ms6.out"
  if (!file.exists(paste(metadatadir, ms6.out, sep = ""))) {
    dir.create(file.path(metadatadir, ms6.out))
  }
  
  #======================================================================
  # compile lists of milestone data filenames
  
  # Identify correct subfolder
  expected_ts_path = paste0(metadatadir, "/meta/ms5.outraw/", params_phyact[["part6_threshold_combi"]])
  expected_ms5raw_path = paste0(metadatadir, "/meta/ms5.outraw")
  if (!dir.exists(expected_ts_path)) {
    if (!dir.exists(expected_ms5raw_path)) {
      stop("\nPath ", expected_ms5raw_path, " does not exist", call. = FALSE)
    } else {
      subDirs = list.dirs(expected_ms5raw_path)
      if (length(subDirs) > 0) {
        expected_ts_path = paste0(expected_ms5raw_path, "/", subDirs[1])
        warning(paste0("\nThreshold combi ", params_phyact[["part6_threshold_combi"]],
                " in time series ouput. Instead now using", subDirs[1]), call. = FALSE)
      } else {
        stop(paste0("\nNo subfolders found inside ", expected_ms5raw_path), call. = FALSE)
      }
    }
  }
  fnames.ms5raw = dir(expected_ts_path)
  # It can be that the use stored rdata and csv files
  # only use rdata in that case
  getExt = function(x) {
    tmp = unlist(strsplit(basename(x), "\\."))
    return(tmp[length(tmp)])
  }
  EXT = tolower(unlist(lapply(fnames.ms5raw, FUN = getExt)))
  uniqueEXT = unique(EXT)
  if (length(uniqueEXT) == 2) {
    fnames.ms5raw = fnames.ms5raw[which(EXT == "rdata")]
    EXT = EXT[which(EXT == "rdata")]
  }
  EXT = EXT[1]
  # Identify existing part 6 milestone data
  fnames.ms6 = dir(paste(metadatadir, "/meta/ms6.out", sep = ""))
  ffdone = fnames.ms6
  #------------------------------------------------
  # specify parameters
  N5 = length(fnames.ms5raw)
  if (length(f0) == 0) f0 = 1
  if (length(f1) == 0) f1 = N5
  
  if (f0 > N5) {
    stop("Argument f0 is larger than the number of files in meta/ms5.outraw.", call. = FALSE)
  }
  if (f1 > N5 | f1 == 0) f1 = N5
  if (f0 == 0) f0 = 1
  
  
  resultsdir = paste0(metadatadir , "/results")
  if (!dir.exists(resultsdir)) dir.create(resultsdir)
  
  #=========================================================
  # Recording-group (e.g. household members) level co-analysis,
  # which at the end of this g.part6 is either
  if (params_247[["part6CoAnalysis"]] == TRUE) {
    part6AlignIndividuals(GGIR_ts_dir = expected_ts_path,
                          outputdir = resultsdir,
                          path_ggirms = paste0(metadatadir , "/meta"),
                          desiredtz = params_general[["desiredtz"]],
                          verbose = verbose)
    
    part6PairwiseAggregation(outputdir = paste0(metadatadir, "/results"),
                             desiredtz = params_general[["desiredtz"]],
                             verbose = verbose)
  }
  

  
  #=========================================================
  # Declare recording level functionality, which at the end of this g.part6 is either
  # applied to the file in parallel with foreach or serially with a loop
  main_part6_recordlevel = function(i, metadatadir = c(), f0 = c(), f1 = c(),
                                    fnames.ms5raw, ffdone, EXT, verbose) {
    if (length(ffdone) > 0) {
      if (length(which(ffdone == fnames.ms5raw[i])) > 0) {
        skip = 1 #skip this file because it was analysed before")
      } else {
        skip = 0 #do not skip this file
      }
    } else {
      skip = 0
    }
    if (params_general[["overwrite"]] == TRUE) skip = 0

    if (skip == 0) {
      # Load time series:
      
      if (EXT == "rdata") {
        load(file = paste0(metadatadir, "/meta/ms5.outraw/",
                           params_phyact[["part6_threshold_combi"]], "/", fnames.ms5raw[i]))
      } else {
        mdat = data.table::fread(file = paste0(metadatadir, "/meta/ms5.outraw/",
                           params_phyact[["part6_threshold_combi"]],  "/", fnames.ms5raw[i]))
        stop("THIS PART OF THE FUNCTIONALITY HAS NOT BEEN COMPLETED YET")
      }
      epochSize = diff(mdat$timenum[1:2])
      # Select relevant section of the time series
      wakeuptimes = which(diff(c(1, mdat$SleepPeriodTime)) == -1)
      onsettimes = which(diff(c(1, mdat$SleepPeriodTime)) == 1) 
      getIndex = function(x, ts, wakeuptimes, onsettimes, epochSize) {
        if (x == "start") {
          y = 1 # simply the start of the time series
        } else if (x == "end") {
          y = nrow(ts) # simply the end of the time series
        } else if (substr(x, start = 1, stop = 1) == "W") {
          n = as.numeric(substr(x, start = 2, stop = 5))
          if (n > 0) { # nth Wakeup after the start
            y = wakeuptimes[n]
          } else if (n < 0) { # nth Wakeup before the end
            y = wakeuptimes[length(wakeuptimes) - n]
          }
        } else if (substr(x, start = 1, stop = 1) == "O") {
          n = as.numeric(substr(x, start = 2, stop = 5))
          if (n > 0) { # nth Onset after the start
            y = onsettimes[n]
          } else if (n < 0) { # nth Onset before the end
            y = onsettimes[length(wakeuptimes) - n]
          }
        } else if (substr(x, start = 1, stop = 1) == "H") {
          n = as.numeric(substr(x, start = 2, stop = 5))
          if (n > 0) { # nth Hour after the start
            y = round((n * 3600) / epochSize)
          } else if (n < 0) { # nth Hour before the end
            y = nrow(ts) - round((n * 3600) / epochSize)
          }
        }
        return(y)
      }
      t0 = getIndex(params_247[["part6Window"]][1], ts = mdat, wakeuptimes, onsettimes, epochSize)
      t1 = getIndex(params_247[["part6Window"]][2], ts = mdat, wakeuptimes, onsettimes, epochSize)
      
      mdat = mdat[t0:t1, ]

      # Perform analysis


      # Summarise results

      # Store results in milestone data
    }
    # Function has no output because ideally all relevant output
    # is store in milestone data by now
  }
  
  if (params_247[["part6CR"]] == TRUE) {
    #======================================================================
    # loop through milestone data-files or filenames stored in output of g.part5
    # setup parallel backend to use many processors
    if (params_general[["do.parallel"]] == TRUE) {
      cores = parallel::detectCores()
      Ncores = cores[1]
      if (Ncores > 3) {
        if (length(params_general[["maxNcores"]]) == 0) params_general[["maxNcores"]] = Ncores
        Ncores2use = min(c(Ncores - 1, params_general[["maxNcores"]], (f1 - f0) + 1))
        if (Ncores2use > 1) {
          cl <- parallel::makeCluster(Ncores2use) # not to overload your computer
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
      if (verbose == TRUE) cat(paste0('\n Busy processing ... see ', metadatadir, ms6.out, ' for progress\n'))
      # check whether we are in development mode:
      GGIRinstalled = is.element('GGIR', installed.packages()[,1])
      packages2passon = functions2passon = NULL
      GGIRloaded = "GGIR" %in% .packages()
      if (GGIRloaded) { #pass on package
        packages2passon = 'GGIR'
        errhand = 'pass'
      } else {
        # pass on functions
        functions2passon = c()
        errhand = 'stop'
      }
      i = 0 # declare i because foreach uses it, without declaring it
      `%myinfix%` = foreach::`%dopar%`
      output_list = foreach::foreach(i = f0:f1,  .packages = packages2passon,
                                     .export = functions2passon, .errorhandling = errhand) %myinfix% {
                                       tryCatchResult = tryCatch({
                                         main_part6_recordlevel(i, metadatadir, f0, f1,
                                                                fnames.ms5raw, ffdone, EXT, verbose)
                                       })
                                       return(tryCatchResult)
                                     }
      on.exit(parallel::stopCluster(cl))
      for (oli in 1:length(output_list)) { # logged error and warning messages
        if (is.null(unlist(output_list[oli])) == FALSE) {
          if (verbose == TRUE) cat(paste0("\nErrors and warnings for ", fnames.ms5raw[oli]))
          print(unlist(output_list[oli])) # print any error and warnings observed
        }
      }
    } else {
      for (i in f0:f1) {
        if (verbose == TRUE) cat(paste0(i, " "))
        main_part6_recordlevel(i, metadatadir, f0, f1,
                               fnames.ms5raw, ffdone, EXT, verbose)
      }
    }
  }
}
