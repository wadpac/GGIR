extract_params = function(params_sleep = c(), params_metrics = c(),
                          params_rawdata = c(), params_247 = c(),
                          params_phyact = c(), params_cleaning = c(),
                          params_output = c(), params_general = c(), input = c(), configfile_csv = c(),
                          params2check = c("sleep", "metrics", "rawdata", "247", "phyact",
                                           "cleaning", "output", "general")) {
  # Order of priority using parameters:
  # 1. Arguments provided as direct input by user
  # 2. Arguments provided via configuration file
  # 3. Default values in parameter objects
  #==================================================================================
  # Get default values in parameter objects
  if (length(params_sleep) == 0) {
    params = load_params(topic = "sleep")
    params_sleep = params$params_sleep
  }
  if (length(params_metrics) == 0) {
    params = load_params(topic = "metrics")
    params_metrics = params$params_metrics
  }
  if (length(params_rawdata) == 0) {
    params = load_params(topic = "rawdata")
    params_rawdata = params$params_rawdata
  }
  if (length(params_247) == 0) {
    params = load_params(topic = "247")
    params_247 = params$params_247
  }
  if (length(params_phyact) == 0) {
    params = load_params(topic = "phyact")
    params_phyact = params$params_phyact
  }
  if (length(params_cleaning) == 0) {
    params = load_params(topic = "cleaning")
    params_cleaning = params$params_cleaning
  }
  if (length(params_output) == 0) {
    params = load_params(topic = "output")
    params_output = params$params_output
  }
  if (length(params_general) == 0) {
    params = load_params(topic = "general")
    params_general = params$params_general
  }
  #==================================================================================
  # Overwrite them by arguments provided via configuration file
  if (length(configfile_csv) > 0) {
    config = data.table::fread(file = configfile_csv, stringsAsFactors = FALSE, data.table = FALSE)
    argNames = names(input)
    if (nrow(config) > 1) {
      for (ci in 1:nrow(config)) {
        varName = as.character(config[ci, 1])
        if (varName %in% c(argNames, "") == FALSE) {
          # only use config file values if argument is not provided as argument to g.shell.GGIR and if no empty
          
          # establish variable class
          conv2logical = conv2num = c()
          suppressWarnings(try(expr = {conv2num = as.numeric(config[ci,2])},silent = TRUE))
          suppressWarnings(try(expr = {conv2logical = as.logical(config[ci,2])},silent = TRUE))
          if (length(conv2num) > 0) {
            numi = is.na(conv2num) == FALSE
          } else {
            numi = FALSE
          }
          logi = FALSE
          if (numi == FALSE & is.na(conv2logical) == FALSE) logi = TRUE
          newValue = "notfound"
          if (logi == TRUE) {
            newValue = as.logical(config[ci,2])
          } else if (numi == TRUE) {
            newValue = as.numeric(config[ci,2])
          } else if (numi == FALSE & logi == FALSE) {
            if (length(config[ci,2]) > 0 & !is.na(config[ci,2])) {
              if (config[ci,2] == 'c()') {
                if (config[ci,1] == "def.noc.sleep") def.noc.sleep = c()
                if (config[ci,1] == "backup.cal.coef") backup.cal.coef = c()
                # Note that we do not re-assign the c(), because they are the default for most arguments that
                # can hold a c() anyway. def.noc.sleep is the only exception.
              } else if (as.character(config[ci,2]) == "NULL") {
                newValue = "c()"
              } else if (config[ci,2] != 'c()' & as.character(config[ci,2]) != "NULL") {
                if (grepl("c\\(", config[ci,2])) { # vector
                  tmp = c(gsub(pattern = "c|\\(|\\)", x = config[ci,2], replacement = ""))
                  tmp = unlist(strsplit(tmp, ","))
                  suppressWarnings(try(expr = {isna = is.na(as.numeric(tmp[1]))}, silent = TRUE))
                  if (length(isna) == 0) isna = FALSE
                  if (isna == TRUE) {
                    newValue = tmp # vector of characters
                  } else {
                    newValue = as.numeric(tmp) # vector of numbers
                  }
                } else {
                  newValue = config[ci,2] #paste0("'",config[ci,2],"'")
                }
              }
            }
          }
          
          # Ignore arguments that are irrelevant or related to deprecated code
          # Note VvH 7 Dec 2022: I have added closedbout and boutmetric 
          # because for the time being many groups may still have this in 
          # their config.csv files. Eventuallythese can be removed here, which will
          # trigger an error for anyone who still uses config file with those arguments.
          ArgNames2Ignore = c("f0", "f1", "studyname", "datadir", 
                              "outputdir", "do.report", "R_version",
                              "GGIR_version", "GGIRversion", "config_file", "mode",
                              "config_file_in_outputdir", "imputeTimegaps",
                              "argNames", "dupArgNames","do.sgAccEN", "do.sgAnglex", 
                              "do.sgAngley", "do.sgAnglez", "frag.classes.spt", "i", 
                              "isna", "tmp", "vecchar", "dupi", "GGIRread_version",
                              "closedbout", "bout.metric", "sleeplogidnum", "LC_TIME_backup",
                              "constrain2range", "is_readxl_installed")
          # Find argument in the various parameter objects
          if (newValue[1] != "notfound" & varName %in% ArgNames2Ignore == FALSE) {
            if (varName %in% names(params_general)) {
              params_general[[varName]] = newValue
            } else {
              if (varName %in% names(params_rawdata)) {
                params_rawdata[[varName]] = newValue
              } else {
                if (varName %in% names(params_metrics)) {
                  params_metrics[[varName]] = newValue
                } else {
                  if (varName %in% names(params_sleep)) {
                    params_sleep[[varName]] = newValue
                  } else {
                    if (varName %in% names(params_phyact)) {
                      params_phyact[[varName]] = newValue
                    } else {
                      if (varName %in% names(params_output)) {
                        params_output[[varName]] = newValue
                      } else {
                        if (varName %in% names(params_247)) {
                          params_247[[varName]] = newValue
                        } else {
                          if (varName %in% names(params_cleaning)) {
                            params_cleaning[[varName]] = newValue
                          } else {
                            warning(paste0("\nIgnoring argument/parameter ", varName,
                                           " as stored in the configuration file",
                                           " as no (longer) used by GGIR. To avoid this",
                                           " warning remove the corresponding rows",
                                           " from the config.csv file."), call. = FALSE)
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  #==================================================================================
  # Overwrite them by arguments provided as direct input by user
  if (length(input) > 0) {
    argNames = names(input)
    if (exists("relyonsleeplog") == TRUE)  params_sleep[["relyonguider"]] = params_sleep[["relyonsleeplog"]]
    update_params = function(x, aN, input) {
      if (is.null(input[[aN]])) {
        x[aN] = list(NULL)
      } else {
        x[[aN]] = input[[aN]]
      }
      return(x)
    }
    expected_sleep_params = names(params_sleep)
    expected_metrics_params = names(params_metrics)
    expected_rawdata_params = names(params_rawdata)
    expected_247_params = names(params_247)
    expected_phyact_params = names(params_phyact)
    expected_cleaning_params = names(params_cleaning)
    expected_output_params = names(params_output)
    expected_general_params = names(params_general)
    for (aN in argNames) {
      if (aN %in% expected_sleep_params == TRUE) { # Sleep
        params_sleep = update_params(x = params_sleep, aN, input)
      } else if (aN %in% expected_metrics_params == TRUE) { # Metrics
        params_metrics = update_params(x = params_metrics, aN, input)
      } else if (aN %in% expected_rawdata_params == TRUE) { # Rawdata
        params_rawdata = update_params(x = params_rawdata, aN, input)
      } else if (aN %in% expected_247_params == TRUE) { # 247
        params_247 = update_params(x = params_247, aN, input)
      } else if (aN %in% expected_phyact_params == TRUE) { # phyact
        params_phyact = update_params(x = params_phyact, aN, input)
      } else if (aN %in% expected_cleaning_params == TRUE) { # cleaning
        if (aN == "data_masking_strategy") { # account for this parameter name change
          params_cleaning[["strategy"]] = input[[aN]]
        }
        params_cleaning = update_params(x = params_cleaning, aN, input)
      } else if (aN %in% expected_output_params == TRUE) { # output
        params_output = update_params(x = params_output, aN, input)
      } else if (aN %in% expected_general_params == TRUE) { # general
        params_general = update_params(x = params_general, aN, input)
      }
    }
  }
  #==================================================================================
  # Check that all parameter values have expect data class
  # and perform some checks on reasonable parameter combinations
  # prevent checking of parameters that are not used in GGIR part
  if (!"sleep" %in% params2check) params_sleep = c()
  if (!"metrics" %in% params2check) params_metrics = c()
  if (!"rawdata" %in% params2check) params_rawdata = c()
  if (!"247" %in% params2check) params_247 = c()
  if (!"phyact" %in% params2check) params_phyact = c()
  if (!"cleaning" %in% params2check) params_cleaning = c()
  if (!"output" %in% params2check) params_output = c()
  if (!"general" %in% params2check) params_general = c()
  
  params = check_params(params_sleep = params_sleep, params_metrics = params_metrics, 
                        params_rawdata = params_rawdata, params_247 = params_247,
                        params_phyact = params_phyact, params_cleaning = params_cleaning,
                        params_output = params_output, params_general = params_general)
  return(params)
}