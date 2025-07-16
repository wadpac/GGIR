g.part3 = function(metadatadir = c(), f0, f1, myfun = c(),
                   params_sleep = c(), params_metrics = c(),
                   params_output = c(),
                   params_general = c(), verbose = TRUE, ...) {
  #----------------------------------------------------------
  # Extract and check parameters
  input = list(...)
  params = extract_params(params_sleep = params_sleep,
                          params_metrics = params_metrics,
                          params_general = params_general,
                          params_output = params_output, input = input,
                          params2check = c("sleep", "metrics",
                                           "general", "output")) # load default parameters
  params_sleep = params$params_sleep
  params_metrics = params$params_metrics
  params_general = params$params_general
  params_output = params$params_output

  checkMilestoneFolders(metadatadir, partNumber = 3)
  #------------------------------------------------------
  fnames = dir(paste(metadatadir,"/meta/ms2.out", sep = ""))
  if (f1 > length(fnames) | f1 == 0) f1 = length(fnames)
  if (f0 > length(fnames) | f0 == 0) f0 = 1
  #========================================================================
  # check which files have already been processed, such that no double work is done
  # ffdone a matrix with all the binary filenames that have been processed
  ffdone = fdone = dir(paste(metadatadir,"/meta/ms3.out", sep = ""))
  if (length(fdone) > 0) {
    for (ij in 1:length(fdone)) {
      tmp = unlist(strsplit(fdone[ij], ".RData"))
      ffdone[ij] = tmp[1]
    }
  } else {
    ffdone = c()
  }
  #=========================================================
  # Declare core functionality, which at the end of this g.part3 is either
  # applied to the file in parallel with foreach or serially with a loop
  main_part3 = function(i, metadatadir = c(), f0, f1, myfun = c(),
                        params_sleep = c(), params_metrics = c(),
                        params_output = c(),
                        params_general = c(), fnames, ffdone, verbose) {
    tail_expansion_log =  NULL
    nightsperpage = 7
    FI = file.info(paste(metadatadir, "/meta/ms2.out/", fnames[i], sep = ""))
    if (is.na(FI$size) == TRUE) FI$size = 0
    if (FI$size == 0 | is.na(FI$size) == TRUE | length(FI$size) == 0) {
      if (verbose == TRUE) {
        cat(paste("P3 file ", fnames[i], sep = ""))
        cat("Filename not recognised")
      }
    }
    fname = unlist(strsplit(fnames[i], ".RData"))[1]
    #=========================================================
    #check whether file has already been processed
    #by comparing filename to read with list of processed files
    if (length(ffdone) > 0) {
      # 1=skip this file because it was analysed before"), 0 =do not skip file
      skip = ifelse(test = length(which(ffdone == fname)) > 0, yes = 1, no = 0)
    } else {
      skip = 0
    }
    if (params_general[["overwrite"]] == TRUE) skip = 0
    if (skip == 0) {
      # Load previously stored meta-data from part1.R
      SUM = IMP = M = c()
      load(paste(metadatadir, "/meta/basic/meta_", fnames[i], sep = ""))
      load(paste(metadatadir, "/meta/ms2.out/", fnames[i], sep = ""))
      if (M$filecorrupt == FALSE & M$filetooshort == FALSE) {
        if (params_general[["sensor.location"]] == "hip") {
          if (is.null(params_sleep[["longitudinal_axis"]])) {
            # If user does not specify longitudinal axis explicitly then re-use
            # estimated longitudinal axis from part 2
            if (is.na(SUM$summary$if_hip_long_axis_id) == FALSE) {
              params_sleep[["longitudinal_axis"]] = SUM$summary$if_hip_long_axis_id
            } else {
              # If not available default to 2
              params_sleep[["longitudinal_axis"]] = 2
            }
          }
        }
        SLE = g.sib.det(M, IMP, I, twd = c(-12,12),
                        acc.metric = params_general[["acc.metric"]],
                        desiredtz = params_general[["desiredtz"]],
                        myfun = myfun,
                        sensor.location = params_general[["sensor.location"]],
                        params_sleep = params_sleep, zc.scale = params_metrics[["zc.scale"]])

        # SleepRegulartiyIndex calculation
        if (!is.null(SLE$output)) {
          if (nrow(SLE$output) > 2*24*(3600/M$windowsizes[1])) { # only calculate SRI if there are at least two days of data
            SleepRegularityIndex = CalcSleepRegularityIndex(data = SLE$output,
                                                            epochsize = M$windowsizes[1],
                                                            desiredtz = params_general[["desiredtz"]],
                                                            SRI1_smoothing_wsize_hrs = params_sleep[["SRI1_smoothing_wsize_hrs"]],
                                                            SRI1_smoothing_frac = params_sleep[["SRI1_smoothing_frac"]])
          } else {
            SleepRegularityIndex = NA
          }
        } else {
          SleepRegularityIndex = NA
        }
        L5list = SLE$L5list
        SPTE_end = SLE$SPTE_end
        SPTE_start = SLE$SPTE_start
        tib.threshold = SLE$tib.threshold
        longitudinal_axis = SLE$longitudinal_axis
        part3_guider = SLE$part3_guider
        if (length(SLE$output) > 0 & SLE$detection.failed == FALSE) {
          ID = SUM$summary$ID
          if (is.list(ID)) ID = unlist(ID)
          datename = as.character(unlist(strsplit(format(as.matrix(M$metashort[1]))," "))[1])
          plottitle = " "
          if (params_output[["do.part3.pdf"]] == TRUE) {
            pdf(paste(metadatadir, "/meta/sleep.qc/graphperday_id_", ID, "_",
                      I$filename, ".pdf", sep = ""), width = 8.2, height = 11.7)
            g.sib.plot(SLE, M, I, plottitle ,nightsperpage = nightsperpage, desiredtz = params_general[["desiredtz"]])
            dev.off()
          }
          sib.cla.sum = c()
          sib.cla.sum = g.sib.sum(SLE, M,
                                  ignorenonwear = params_sleep[["ignorenonwear"]],
                                  desiredtz = params_general[["desiredtz"]])
          rec_starttime = IMP$metashort[1,1] # this may be used in g.loadlog to align sleeplog with recording
          ms3out_filename = paste(metadatadir, "/meta/ms3.out/", fname, ".RData",sep = "")
          GGIRversion = utils::packageVersion("GGIR")
          save(sib.cla.sum, L5list, SPTE_end, SPTE_start, tib.threshold, rec_starttime, ID,
               longitudinal_axis, SleepRegularityIndex, tail_expansion_log, GGIRversion,
               file = ms3out_filename, part3_guider)
        }
      }
    }
  }

  if (params_general[["do.parallel"]] == TRUE) {
    cores = parallel::detectCores()
    Ncores = cores[1]
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
    if (verbose == TRUE) cat(paste0('\n Busy processing ... see ', metadatadir,'/meta/ms3.out', ' for progress\n'))
    # check whether we are indevelopment mode:
    GGIRinstalled = is.element('GGIR', installed.packages()[,1])
    packages2passon = functions2passon = NULL
    GGIRloaded = "GGIR" %in% .packages()
    if (GGIRloaded) { #pass on package
      packages2passon = 'GGIR'
      errhand = 'pass'
    } else {
      # pass on functions
      functions2passon = c("g.sib.det", "g.detecmidnight", "iso8601chartime2POSIX",
                           "g.sib.plot", "g.sib.sum", "HASPT", "HASIB", "CalcSleepRegularityIndex",
                           "extract_params", "load_params", "check_params")
      errhand = 'stop'
    }
    i = 0 # declare i because foreach uses it, without declaring it
    `%myinfix%` = foreach::`%dopar%`
    output_list = foreach::foreach(i = f0:f1, .packages = packages2passon,
                                   .export = functions2passon, .errorhandling = errhand) %myinfix% {
                                     tryCatchResult = tryCatch({
                                       main_part3(i, metadatadir, f0, f1, myfun,
                                                  params_sleep, params_metrics,
                                                  params_output,
                                                  params_general, fnames, ffdone, verbose)
                                     })
                                     return(tryCatchResult)
                                   }

    on.exit(parallel::stopCluster(cl))
    for (oli in 1:length(output_list)) { # logged error and warning messages
      if (is.null(unlist(output_list[oli])) == FALSE) {
        if (verbose == TRUE) cat(paste0("\nErrors and warnings for ", fnames[oli]))
        print(unlist(output_list[oli])) # print any error and warnings observed
      }
    }
  } else {
    for (i in f0:f1) {
      if (verbose == TRUE) cat(paste0(i, " "))
      main_part3(i, metadatadir, f0, f1, myfun,
                 params_sleep, params_metrics,
                 params_output,
                 params_general, fnames, ffdone, verbose)
    }
  }
}
