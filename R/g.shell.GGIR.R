g.shell.GGIR = function(mode = 1:5, datadir = c(), outputdir = c(),
                        studyname = c(), f0 = 1, f1 = 0,
                        do.report = c(2, 4, 5), configfile = c(),
                        myfun = c(), ...) {
  #get input variables
  input = list(...)
  # Check for duplicated arguments
  if (length(input) > 0) {
    if (length(input) > 1) {
      argNames = names(input)
      dupArgNames = duplicated(argNames)
      if (any(dupArgNames)) { # Warn user about those duplicated arguments
        for (dupi in unique(argNames[dupArgNames])) {
          dupArgValues = input[which(argNames %in% dupi)]
          if (all(dupArgValues == dupArgValues[[1]])) { # duplicated arguments, but no confusion about what value should be
            warning(paste0("\nArgument ", dupi, " has been provided more than once. Try to avoid this."))
          } else {# duplicated arguments, and confusion about what value should be,
            warning(paste0("\nArgument ", dupi, " has been provided more than once and with inconsistent values. Please fix."))
          }
        }
      }
    }
  }

  #===========================
  # Establish default start / end file index to process
  filelist = isfilelist(datadir)
  if (dir.exists(outputdir) == FALSE) stop("\nDirectory specified by argument outputdir, does not exist")
  derivef0f1 = FALSE
  if (length(f0) == 0 | length(f1) == 0) {
    derivef0f1 = TRUE
  } else {
    if (f0 == 0 | f1 == 0) derivef0f1 = TRUE
  }
  if (derivef0f1 == TRUE) { # What file to start with?
    f0 = 1
    if (filelist == FALSE) {  # What file to end with?
      f1 <- length(dir(datadir, recursive = TRUE, ignore.case = TRUE, pattern = "[.](csv|bin|Rda|wa|cw|gt3)")) # modified by JH
    } else {
      f1 = length(datadir) #modified
    }
  }
  # Establish which parts need to be processed:
  dopart1 = dopart2 = dopart3 = dopart4 = dopart5 = FALSE
  if (length(which(mode == 0)) > 0) {
    dopart1 = dopart2 = dopart3 = dopart4 = dopart5 = TRUE
  } else {
    # if (length(which(mode == 0)) > 0) dopart0 = TRUE
    if (length(which(mode == 1)) > 0) dopart1 = TRUE
    if (length(which(mode == 2)) > 0) dopart2 = TRUE
    if (length(which(mode == 3)) > 0) dopart3 = TRUE
    if (length(which(mode == 4)) > 0) dopart4 = TRUE
    if (length(which(mode == 5)) > 0) dopart5 = TRUE
  }

  # test whether RData input was used and if so, use original outputfolder
  if (length(datadir) > 0) {
    # list of all csv and bin files
    dir2fn = datadir2fnames(datadir, filelist)
    fnames = dir2fn$fnames
    fnamesfull = dir2fn$fnamesfull
    # check whether these are RDA
    if (length(unlist(strsplit(fnames[1], "[.]RD"))) > 1) {
      useRDA = TRUE
    } else {
      useRDA = FALSE
    }
  } else {
    useRDA = FALSE
  }
  if (filelist == TRUE | useRDA == TRUE) {
    metadatadir = paste0(outputdir, "/output_", studyname)
  } else {
    outputfoldername = unlist(strsplit(datadir, "/"))[length(unlist(strsplit(datadir, "/")))]
    metadatadir = paste0(outputdir, "/output_", outputfoldername)
  }

  # Configuration file - check whether it exists or auto-load
  configfile_csv = c()
  ex = "csv"
  if (length(configfile) > 0) { # Get extension of file
    ex = unlist(strsplit(basename(configfile), split ="\\."))
    ex = ex[length(ex)]
  }
  if (ex == "csv") { # at a later point there may also be other file extensions
    if (dir.exists(metadatadir) | length(configfile) > 0) {
      # so if outputdir was not created (first time) and if configfile is not
      # specified then we can safely assume that there is no configfile
      # and this section will be skipped, alternatively an attempt is
      # made to retrieve settings from the configfile
      if (length(configfile) > 0) {
        if (!file.exists(configfile)) {
          stop("\nDo not supply argument configfile if the configfile does not exist yet")
        } else {
          configfile_csv = configfile
        }
      }
      if (dir.exists(metadatadir) & length(configfile) == 0) {
        config_file_in_outputdir = paste0(metadatadir, "/config.csv")
        # So, if both exist, we prioritise configfile over the configfile
        # stored in outputdir and this is skipped
        if (file.exists(config_file_in_outputdir)) configfile_csv = config_file_in_outputdir
      }
    }
  }

  #----------------------------------------------------------
  # Extract parameters from user input, configfile and/or defaults.
  params = extract_params(input = input, configfile_csv = configfile_csv) # load default parameters here in g.shell.GGIR
  params_sleep = params$params_sleep
  params_metrics = params$params_metrics
  params_rawdata = params$params_rawdata
  params_247 = params$params_247
  params_phyact = params$params_phyact
  params_cleaning = params$params_cleaning
  params_output = params$params_output
  params_general = params$params_general

  if (dopart3 == TRUE & params_metrics[["do.anglez"]] == FALSE) {
    params_metrics[["do.anglez"]] = TRUE
  }

  if (length(myfun) != 0) { # Run check on myfun object, if provided
    warning("\nAre you using GGIR as online service to others? If yes, then make sure you prohibit the",
            " user from specifying argument myfun as this poses a security risk.")
    check_myfun(myfun, params_general[["windowsizes"]])
  }

  #-----------------------------------------------------------
  # Print GGIR header to console
  GGIRversion = ""
  SI = sessionInfo()
  try(expr = {GGIRversion = SI$loadedOnly$GGIR$Version}, silent = TRUE)
  if (length(GGIRversion) == 0) {
    try(expr = {GGIRversion = SI$otherPkgs$GGIR$Version}, silent = TRUE)
  }
  if (length(GGIRversion) == 0) GGIRversion = "could not extract version"
  GGIRversion = paste0(" ",GGIRversion)
  rm(SI)
  cat(paste0("\n   GGIR version: ",GGIRversion,"\n"))
  cat("\n   Do not forget to cite GGIR in your publications via a version number and\n")
  cat("   Migueles et al. 2019 JMPB. doi: 10.1123/jmpb.2018-0063. \n")
  cat("   See also: https://cran.r-project.org/package=GGIR/vignettes/GGIR.html#citing-ggir")
  cat("\n")
  cat("\n   To make your research reproducible and interpretable always report:")
  cat("\n     (1) Accelerometer brand and product name")
  cat("\n     (2) How you configured the accelerometer")
  cat("\n     (3) Study protocol and wear instructions given to the participants")
  cat("\n     (4) GGIR version")
  cat("\n     (5) How GGIR was used: Share the config.csv file or your R script.")
  cat("\n     (6) How you post-processed / cleaned GGIR output")
  cat("\n     (7) How reported outcomes relate to the specific variable names in GGIR")

  #-----------------------------------------------------------
  # Now run GGIR parts 1-5
  print_console_header = function(headerTitle) {
    cat('\n')
    cat(paste0(rep('_', options()$width), collapse = ''))
    cat("\n",headerTitle,"\n")
  }
  if (dopart1 == TRUE) {
    print_console_header("Part 1")
    g.part1(datadir = datadir, outputdir = outputdir, f0 = f0, f1 = f1,
            studyname = studyname, myfun = myfun,
            params_rawdata = params_rawdata, params_metrics = params_metrics,
            params_cleaning = params_cleaning, params_general = params_general)
  }
  if (dopart2 == TRUE) {
    print_console_header("Part 2")
    if (f1 == 0) f1 = length(dir(paste0(metadatadir, "/meta/basic")))
    g.part2(datadir = datadir, metadatadir = metadatadir, f0 = f0, f1 = f1,
            myfun = myfun, params_cleaning = params_cleaning,
            params_247 = params_247, params_phyact = params_phyact,
            params_output = params_output,
            params_general = params_general)
  }
  if (dopart3 == TRUE) {
    print_console_header("Part 3")
    if (f1 == 0) f1 = length(dir(paste(metadatadir,"/meta/basic",sep="")))
    g.part3(metadatadir = metadatadir, f0 = f0, f1 = f1, myfun = myfun,
            params_sleep = params_sleep, params_output = params_output,
            params_metrics = params_metrics,
            params_general = params_general)
  }
  if (dopart4 == TRUE) {
    print_console_header("Part 4")
    if (f1 == 0) f1 = length(dir(paste(metadatadir,"/meta/ms3.out",sep="")))
    g.part4(datadir = datadir, metadatadir = metadatadir, f0 = f0, f1 = f1,
            params_sleep = params_sleep, params_metrics = params_metrics,
            params_general = params_general, params_output = params_output,
            params_cleaning = params_cleaning)
  }
  if (dopart5 == TRUE) {
    print_console_header("Part 5")
    if (f1 == 0) f1 = length(dir(paste0(metadatadir, "/meta/ms3.out"))) # this is intentionally ms3 and not ms4, do not change!
    g.part5(datadir = datadir, metadatadir = metadatadir, f0 = f0, f1 = f1,
            params_sleep = params_sleep, params_metrics = params_metrics,
            params_general = params_general, params_output = params_output,
            params_cleaning = params_cleaning, params_247 = params_247,
            params_phyact = params_phyact)
  }
  #--------------------------------------------------
  # Store configuration parameters in config file
  LS = ls()
  LS = LS[which(LS %in% c("input", "txt", "derivef0f1", "dopart1", "dopart2", "dopart3", "LS",
                          "dopart4", "dopart5", "fnames", "useRDA", "metadatadir", "ci", "config",
                          "configfile", "filelist", "outputfoldername", "numi", "logi",
                          "conv2logical", "conv2num", "SI", "params", "argNames", "dupArgNames",
                          "print_console_header", "configfile_csv", "myfun", "ex", "dir2fn", "fnamesfull") == FALSE)]
  config.parameters = mget(LS)
  config.matrix = as.data.frame(createConfigFile(config.parameters))
  if (dir.exists(metadatadir)) {
    write.csv(config.matrix, file = paste0(metadatadir, "/config.csv"), row.names = FALSE)
  } else {
    if (dir.exists(datadir) == FALSE) {
      warning("\nCould not write config file because studyname or datadir are not correctly specified.")
    }
  }
  #==========================
  # Report generation:
  # check a few basic assumptions before continuing
  if (length(which(do.report == 4 | do.report == 5)) > 0 | params_output[["visualreport"]] == TRUE) {
    if (file.exists(paste0(metadatadir,"/meta/ms4.out"))) {
    } else {
      cat("Warning: First run g.shell.GGIR with mode = 4 to generate required milestone data\n")
      cat("before you can use argument visualreport or create a report for part 4\n")
    }
  }
  if (length(which(do.report == 2)) > 0) {
    print_console_header("Report part 2")
    N.files.ms2.out = length(dir(paste0(metadatadir, "/meta/ms2.out")))
    if (N.files.ms2.out < f0) f0 = 1
    if (N.files.ms2.out < f1) f1 = N.files.ms2.out
    if (f1 == 0) f1 = N.files.ms2.out
    if (length(params_247[["qwindow"]]) > 2 | is.character(params_247[["qwindow"]])) {
      store.long = TRUE
    } else {
      store.long = FALSE
    }
    g.report.part2(metadatadir = metadatadir, f0 = f0, f1 = f1,
                   maxdur = params_cleaning[["maxdur"]],
                   selectdaysfile = params_cleaning[["selectdaysfile"]],
                   store.long = store.long)
  }
  if (length(which(do.report == 4)) > 0) {
    print_console_header("Report part 4")
    N.files.ms4.out = length(dir(paste0(metadatadir, "/meta/ms4.out")))
    if (N.files.ms4.out < f0) f0 = 1
    if (N.files.ms4.out < f1) f1 = N.files.ms4.out
    if (f1 == 0) f1 = N.files.ms4.out
    g.report.part4(datadir = datadir, metadatadir = metadatadir, f0 = f0, f1 = f1,
                   loglocation = params_sleep[["loglocation"]],
                   storefolderstructure = params_output[["storefolderstructure"]],
                   data_cleaning_file = params_cleaning[["data_cleaning_file"]],
                   sleepwindowType = params_sleep[["sleepwindowType"]])
  }
  if (length(which(do.report == 5)) > 0) {
    print_console_header("Report part 5")
    N.files.ms5.out = length(dir(paste0(metadatadir, "/meta/ms5.out")))
    if (N.files.ms5.out < f0) f0 = 1
    if (N.files.ms5.out < f1) f1 = N.files.ms5.out
    if (f1 == 0) f1 = N.files.ms5.out
    g.report.part5(metadatadir = metadatadir, f0 = f0, f1 = f1,
                   loglocation = params_sleep[["loglocation"]],
                   includenightcrit = params_sleep[["includenightcrit"]],
                   includedaycrit = params_cleaning[["includedaycrit"]],
                   data_cleaning_file = params_cleaning[["data_cleaning_file"]],
                   includedaycrit.part5 = params_cleaning[["includedaycrit.part5"]],
                   minimum_MM_length.part5 = params_cleaning[["minimum_MM_length.part5"]],
                   week_weekend_aggregate.part5 = params_output[["week_weekend_aggregate.part5"]],
                   LUX_day_segments = params_247[["LUX_day_segments"]])
  }
  if (params_output[["visualreport"]] == TRUE) {
    print_console_header("Generate visual reports")
    g.plot5(metadatadir = metadatadir, f0 = f0, f1 = f1,
            dofirstpage = params_output[["dofirstpage"]],
            viewingwindow = params_output[["viewingwindow"]],
            overwrite = params_general[["overwrite"]],
            desiredtz = params_general[["desiredtz"]],
            metric = params_general[["acc.metric"]],
            threshold.lig = params_phyact[["threshold.lig"]],
            threshold.mod = params_phyact[["threshold.mod"]],
            threshold.vig = params_phyact[["threshold.vig"]])
  }
}
