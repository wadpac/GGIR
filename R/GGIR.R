GGIR = function(mode = 1:5, datadir = c(), outputdir = c(),
                studyname = c(), f0 = 1, f1 = 0,
                do.report = c(2, 4, 5, 6), configfile = c(),
                myfun = c(), verbose = TRUE,
                ...) {
  #get input variables
  input = list(...)
  # Check for duplicated arguments
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
  
  if (length(datadir) == 0) {
    stop('\nVariable datadir is not specified')
  }
  
  if (length(outputdir) == 0) {
    stop('\nVariable outputdir is not specified')
  }
  
  # Convert paths from Windows specific slashed to generic slashes
  outputdir = gsub(pattern = "\\\\", replacement = "/", x = outputdir)
  datadir = gsub(pattern = "\\\\", replacement = "/", x = datadir)
  
  #===========================
  # Establish default start / end file index to process
  filelist = isfilelist(datadir)
  
  if (filelist == FALSE) {
    if (dir.exists(datadir) == FALSE && 1 %in% mode) {
      # Note: The check whether datadir exists is only relevant when running part 1
      # For other scenarios it can be convenient to keep specifying datadir
      # even if the actual path is no longer available, because GGIR uses the most
      # distal folder name to identify the output directory. For example,
      # if part 2-5 are processed on a different system then part 1.
      
      stop("\nDirectory specified by argument datadir does not exist")
    }
    if (datadir == outputdir || grepl(paste(datadir, '/', sep = ''), outputdir)) {
      stop(paste0('\nError: The file path specified by argument outputdir should ',
                  'NOT equal or be a subdirectory of the path specified by argument datadir'))
    }
  }
  if (dir.exists(outputdir) == FALSE) {
    stop("\nDirectory specified by argument outputdir does not exist")
  }
  if (file.access(outputdir, mode = 2) == 0) {
    if (verbose == TRUE) cat("\nChecking that user has write access permission for directory specified by argument outputdir: Yes\n")
  } else {
    stop("\nUser does not seem to have write access permissions for the directory specified by argument outputdir.\n")
  }
  if (filelist == TRUE) {
    if (length(studyname) == 0) {
      stop('\nError: studyname must be specified if datadir is a list of files')
    }
  }
  if (is.null(f0) || f0 < 1) { # What file to start with?
    f0 = 1
  }
  if ((is.null(f1) || f1 < 1) && 1 %in% mode) {  # What file to end with?
    # Do not modify f1 here when not attempting to process GGIR part 1
    # we only expect datadir to exist when running part 1
    if (filelist == FALSE) {
      f1 <- length(dir(datadir, recursive = TRUE, ignore.case = TRUE, pattern = "[.](csv|bin|Rda|wa|cw|gt3)")) # modified by JH
    } else {
      f1 = length(datadir) #modified
    }
  }
  if (is.null(f1)) {
    f1 = 0
  }
  # Establish which parts need to be processed:
  dopart1 = dopart2 = dopart3 = dopart4 = dopart5 = dopart6 = FALSE
  if (length(which(mode == 0)) > 0) {
    dopart1 = dopart2 = dopart3 = dopart4 = dopart5 = TRUE # intentionally not including dopart6
  } else {
    # if (length(which(mode == 0)) > 0) dopart0 = TRUE
    if (length(which(mode == 1)) > 0) dopart1 = TRUE
    if (length(which(mode == 2)) > 0) dopart2 = TRUE
    if (length(which(mode == 3)) > 0) dopart3 = TRUE
    if (length(which(mode == 4)) > 0) dopart4 = TRUE
    if (length(which(mode == 5)) > 0) dopart5 = TRUE
    if (length(which(mode == 6)) > 0) dopart6 = TRUE
  }
  
  if (filelist == TRUE) {
    metadatadir = paste0(outputdir, "/output_", studyname)
  } else {
    outputfoldername = unlist(strsplit(datadir, "/"))[length(unlist(strsplit(datadir, "/")))]
    metadatadir = paste0(outputdir, "/output_", outputfoldername)
  }
  
  # Configuration file - check whether it exists or auto-load
  configfile_csv = c()
  ex = "csv"
  if (length(configfile) > 0) { # Get extension of file
    ex = unlist(strsplit(basename(configfile), split = "\\."))
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
  
  if (params_general[["dataFormat"]] == "ukbiobank") {
    warning("\nRunnning part 3, 4, and 5 are disabled when dataFormat is ukbiobank epoch", call. = FALSE)
    dopart3 = dopart4 = dopart5 = FALSE
    mode = mode[which(mode <= 2)]
  }
  
  if (dopart3 == TRUE & params_metrics[["do.anglez"]] == FALSE & params_general[["dataFormat"]] == "raw") {
    params_metrics[["do.anglez"]] = TRUE
  }
  
  if (length(myfun) != 0) { # Run check on myfun object, if provided
    check_myfun(myfun, params_general[["windowsizes"]])
  }
  
  if (params_output[["visualreport"]] == TRUE & params_general[["dataFormat"]] != "raw") {
    params_output[["visualreport"]] == FALSE
    warning(paste0("Turning off visualreport generation because",
                   " dataFormat is not raw."), call. = FALSE)
  }
  
  # check package dependencies
  if (params_metrics$do.neishabouricounts == TRUE) {
    is_actilifecounts_installed = is.element('actilifecounts', installed.packages()[,1])
    if (is_actilifecounts_installed == FALSE) {
      stop("If you want to derive Neishabouricounts, please install package: actilifecounts.", call. = FALSE)
    } else {
      if (utils::packageVersion("actilifecounts") < "1.1.0") {
        stop("Please update R package actilifecounts to version 1.1.0 or higher", call. = FALSE)
      }
    }
  }
  
  if (params_247$cosinor == TRUE) {
    is_ActCR_installed = is.element('ActCR', installed.packages()[,1])
    if (is_ActCR_installed == FALSE) {
      stop("If you want to derive circadian rhythm indicators, please install package: ActCR.", call. = FALSE)
    }
  }
  if (1 %in% mode) {
    checkFormat = TRUE
    if (all(dir.exists(datadir)) == TRUE) {
      rawaccfiles = dir(datadir, full.names = TRUE)[f0:f1]
    } else if (all(file.exists(datadir))) {
      rawaccfiles = datadir[f0:f1]
    } else {
      checkFormat = FALSE
    }
    
    if (checkFormat == TRUE) {
      is_GGIRread_installed = is.element('GGIRread', installed.packages()[,1])
      is_read.gt3x_installed = is.element('read.gt3x', installed.packages()[,1])
      # skip this check if GGIRread and read.gt3x are both available
      if (is_GGIRread_installed == FALSE | is_read.gt3x_installed == FALSE) {
        getExt = function(x) {
          tmp = unlist(strsplit(x, "[.]"))
          return(tmp[length(tmp)])
        }
        rawaccfiles_formats = unique(unlist(lapply(rawaccfiles, FUN = getExt)))
        # axivity (cwa, wav), geneactive (bin), genea (bin):
        if (any(grepl("cwa|wav|bin", rawaccfiles_formats))) {
          if (is_GGIRread_installed == FALSE) {
            stop("If you are working with axivity, geneactiv, or genea files, please install package: GGIRread.", call. = FALSE)
          }
        }
        # actigraph (gt3x)
        if (any(grepl("gt3x", rawaccfiles_formats))) {
          if (is_read.gt3x_installed == FALSE) {
            stop(paste0("If you are working with actigraph files, please install package: read.gt3x.", call. = FALSE))
          }
        }
      }
    }
  }
  
  #-----------------------------------------------------------
  # Print GGIR header to console
  GGIRversion = "could not extract version"
  if (is.element('GGIR', installed.packages()[,1])) {
    GGIRversion = as.character(utils::packageVersion("GGIR"))
    if (length(GGIRversion) != 1) GGIRversion = sessionInfo()$otherPkgs$GGIR$Version
  }
  if (verbose == TRUE) {
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
  }
  #-----------------------------------------------------------
  # Now run GGIR parts 1-5
  print_console_header = function(headerTitle) {
    cat('\n')
    cat(paste0(rep('_', options()$width), collapse = ''))
    cat("\n",headerTitle,"\n")
  }
  if (dopart1 == TRUE) {
    if (verbose == TRUE) print_console_header("Part 1")
    
    if (!is.null(params_general[["maxRecordingInterval"]]) & params_general[["overwrite"]] == TRUE) {
      # When we want to overwrite previously processed data and append recordings
      # it is necessary to first empty folder meta/basic to avoid confusion with
      # previously appended data
      basic_folder = paste0(metadatadir, "/meta/basic")
      if (dir.exists(basic_folder)) {
        basic_ms_files = dir(basic_folder, full.names = TRUE)
        if (length(basic_ms_files) > 0) {
          for (fnr in basic_ms_files) unlink(fnr, recursive = TRUE)
          rm(fnr)
        }
        rm(basic_folder, basic_ms_files)
      }
    }
    if (params_general[["dataFormat"]] == "raw") {
      g.part1(datadir = datadir, metadatadir = metadatadir, f0 = f0, f1 = f1,
              studyname = studyname, myfun = myfun,
              params_rawdata = params_rawdata, params_metrics = params_metrics,
              params_cleaning = params_cleaning, params_general = params_general,
              verbose = verbose)
    } else {
      # Skip g.part1, but instead convert epoch data to a format that
      # looks as if it came out of g.part1
      warning(paste0("\nBe aware that you are using epoch level aggregates of raw data ",
                     "computed outside GGIR by which their reproducibility and ",
                     "transparancy is also outside the scope of GGIR. GGIR",
                     " input arguments related to raw data handling are ignored."),
              call. = FALSE)
      convertEpochData(datadir = datadir,
                       metadatadir = metadatadir,
                       params_general = params_general,
                       verbose = verbose)
    }
    if (!is.null(params_general[["maxRecordingInterval"]])) {
      # Append recordings when ID and brand match and gap between
      # recordings does not exceed maxRecordingInterval,
      # where GGIR prohibits user from use a value larger
      # than 504 hours (21 days)
      appendRecords(metadatadir = metadatadir,
                    desiredtz = params_general[["desiredtz"]],
                    idloc = params_general[["idloc"]],
                    maxRecordingInterval = params_general[["maxRecordingInterval"]])
    }
  }
  if (dopart2 == TRUE) {
    if (verbose == TRUE) print_console_header("Part 2")
    if (f1 == 0) f1 = length(dir(paste0(metadatadir, "/meta/basic")))
    g.part2(datadir = datadir, metadatadir = metadatadir, f0 = f0, f1 = f1,
            myfun = myfun, params_cleaning = params_cleaning,
            params_247 = params_247, params_phyact = params_phyact,
            params_output = params_output,
            params_general = params_general,
            verbose = verbose)
  }
  if (dopart3 == TRUE) {
    if (verbose == TRUE) print_console_header("Part 3")
    if (f1 == 0) f1 = length(dir(paste0(metadatadir,"/meta/basic")))
    g.part3(metadatadir = metadatadir, f0 = f0, f1 = f1, myfun = myfun,
            params_sleep = params_sleep, params_output = params_output,
            params_metrics = params_metrics,
            params_general = params_general,
            verbose = verbose)
  }
  if (dopart4 == TRUE) {
    if (verbose == TRUE) print_console_header("Part 4")
    if (f1 == 0) f1 = length(dir(paste0(metadatadir,"/meta/ms3.out")))
    g.part4(datadir = datadir, metadatadir = metadatadir, f0 = f0, f1 = f1,
            params_sleep = params_sleep, params_metrics = params_metrics,
            params_general = params_general, params_output = params_output,
            params_cleaning = params_cleaning,
            verbose = verbose)
  }
  if (dopart5 == TRUE) {
    if (verbose == TRUE) print_console_header("Part 5")
    if (f1 == 0) f1 = length(dir(paste0(metadatadir, "/meta/ms3.out"))) # this is intentionally ms3 and not ms4, do not change!
    g.part5(datadir = datadir, metadatadir = metadatadir, f0 = f0, f1 = f1,
            params_sleep = params_sleep, params_metrics = params_metrics,
            params_general = params_general, params_output = params_output,
            params_cleaning = params_cleaning, params_247 = params_247,
            params_phyact = params_phyact,
            verbose = verbose)
  }
  if (dopart6 == TRUE) {
    if (verbose == TRUE) print_console_header("Part 6")
    if (f1 == 0) {
      f1 = length(dir(paste0(metadatadir, "/meta/ms5.outraw/", 
                             params_phyact[["part6_threshold_combi"]])))
    }
    g.part6(datadir = datadir, metadatadir = metadatadir, f0 = f0, f1 = f1,
            params_general = params_general, params_phyact = params_phyact,
            params_247 = params_247,
            verbose = verbose)
  }
  
  #--------------------------------------------------
  # Store configuration parameters in config file
  LS = ls()
  LS = LS[which(LS %in% c("input", "txt", "dopart1", "dopart2", "dopart3", "LS",
                          "dopart4", "dopart5", "dopart6", "metadatadir", "ci", "config",
                          "configfile", "filelist", "outputfoldername", "numi", "logi",
                          "conv2logical", "conv2num", "SI", "params", "argNames", "dupArgNames",
                          "print_console_header", "configfile_csv", "myfun", "ex",
                          "GGIRversion",  "dupArgValues", "verbose", "is_GGIRread_installed", 
                          "is_read.gt3x_installed", "is_ActCR_installed", 
                          "is_actilifecounts_installed", "rawaccfiles", 
                          "checkFormat", "getExt") == FALSE)]
  
  config.parameters = mget(LS)
  config.matrix = as.data.frame(createConfigFile(config.parameters, GGIRversion))
  config.matrix$context[which(config.matrix$context == "")] = "not applicable"
  if (dir.exists(metadatadir)) {
    data.table::fwrite(config.matrix, file = paste0(metadatadir, "/config.csv"),
                       row.names = FALSE, sep = params_output[["sep_config"]])
  } else {
    warning("\nCould not write config file.")
  }
  #==========================
  # Report generation:
  # -----
  # check a few basic assumptions before continuing
  if (length(which(do.report == 2)) > 0) {
    N.files.ms2.out = length(dir(paste0(metadatadir, "/meta/ms2.out")))
    if (N.files.ms2.out > 0) {
      if (verbose == TRUE) print_console_header("Report part 2")
      # if (N.files.ms2.out < f0) f0 = 1
      # if (N.files.ms2.out < f1) f1 = N.files.ms2.out
      if (length(f0) == 0) f0 = 1
      if (f1 == 0) f1 = N.files.ms2.out
      if (length(params_247[["qwindow"]]) > 2 |
          is.character(params_247[["qwindow"]]) |
          (length(params_247[["qwindow"]]) == 2 & !all(c(0, 24) %in% params_247[["qwindow"]]))) {
        store.long = TRUE
      } else {
        store.long = FALSE
      }
      g.report.part2(metadatadir = metadatadir, f0 = f0, f1 = f1,
                     maxdur = params_cleaning[["maxdur"]],
                     store.long = store.long, do.part2.pdf = params_output[["do.part2.pdf"]],
                     verbose = verbose, sep_reports = params_output[["sep_reports"]])
    }
  }
  if (length(which(do.report == 4)) > 0) {
    N.files.ms4.out = length(dir(paste0(metadatadir, "/meta/ms4.out")))
    if (N.files.ms4.out > 0) {
      if (verbose == TRUE) print_console_header("Report part 4")
      if (N.files.ms4.out < f0) f0 = 1
      if (N.files.ms4.out < f1) f1 = N.files.ms4.out
      if (f1 == 0) f1 = N.files.ms4.out
      g.report.part4(datadir = datadir, metadatadir = metadatadir, f0 = f0, f1 = f1,
                     loglocation = params_sleep[["loglocation"]],
                     storefolderstructure = params_output[["storefolderstructure"]],
                     data_cleaning_file = params_cleaning[["data_cleaning_file"]],
                     sleepwindowType = params_sleep[["sleepwindowType"]],
                     verbose = verbose, sep_reports = params_output[["sep_reports"]])
    }
  }
  if (length(which(do.report == 5)) > 0) {
    N.files.ms5.out = length(dir(paste0(metadatadir, "/meta/ms5.out")))
    if (N.files.ms5.out > 0) {
      if (verbose == TRUE) print_console_header("Report part 5")
      if (N.files.ms5.out < f0) f0 = 1
      if (N.files.ms5.out < f1) f1 = N.files.ms5.out
      if (f1 == 0) f1 = N.files.ms5.out
      g.report.part5(metadatadir = metadatadir, f0 = f0, f1 = f1,
                     loglocation = params_sleep[["loglocation"]],
                     params_cleaning = params_cleaning,
                     week_weekend_aggregate.part5 = params_output[["week_weekend_aggregate.part5"]],
                     LUX_day_segments = params_247[["LUX_day_segments"]],
                     verbose = verbose, sep_reports = params_output[["sep_reports"]])
      g.report.part5_dictionary(metadatadir = metadatadir, sep_reports = params_output[["sep_reports"]])
    }
  }
  if (length(which(do.report == 6)) > 0) {
    N.files.ms6.out = length(dir(paste0(metadatadir, "/meta/ms6.out")))
    if (N.files.ms6.out > 0) {
      if (verbose == TRUE) print_console_header("Report part 6")
      if (N.files.ms6.out < f0) f0 = 1
      if (N.files.ms6.out < f1) f1 = N.files.ms6.out
      if (f1 == 0) f1 = N.files.ms6.out
      g.report.part6(metadatadir = metadatadir, f0 = f0, f1 = f1,
                     params_cleaning = params_cleaning,
                     verbose = verbose, sep_reports = params_output[["sep_reports"]])
    }
  }
  if (params_output[["visualreport"]] == TRUE) {
    # g.plot uses metadata from part 1, 3, and 4
    files.basic = gsub("^meta_", "", dir(paste0(metadatadir, "/meta/basic"))) # without "meta_" to ease matching to data in ms3.out and ms4.out
    files.ms3.out = dir(paste0(metadatadir, "/meta/ms3.out"))
    files.ms4.out = dir(paste0(metadatadir, "/meta/ms4.out"))
    # at least one metafile from the same recording in each folder
    files.available = Reduce(intersect, list(files.basic, files.ms3.out, files.ms4.out))
    
    if (length(files.available) > 0) {
      if (verbose == TRUE) print_console_header("Generate visual reports")
      g.plot5(metadatadir = metadatadir,
              dofirstpage = params_output[["dofirstpage"]],
              viewingwindow = params_output[["viewingwindow"]],
              f0 = f0, f1 = f1,
              overwrite = params_general[["overwrite"]],
              metric = params_general[["acc.metric"]],
              desiredtz = params_general[["desiredtz"]],
              threshold.lig = params_phyact[["threshold.lig"]],
              threshold.mod = params_phyact[["threshold.mod"]],
              threshold.vig = params_phyact[["threshold.vig"]],
              visualreport_without_invalid = params_output[["visualreport_without_invalid"]],
              includedaycrit = params_cleaning[["includedaycrit"]],
              includenightcrit = params_cleaning[["includenightcrit"]],
              verbose = TRUE)
    }
  }
}
