extract_params = function(params_sleep = c(), params_metrics = c(),
                          params_rawdata = c(), input = c()) {
  #----------------------------------------------------------
  # If no params are provide use defaults
  if (length(params_sleep) == 0) {
    params = load_params(group = "sleep") # for now just load all parameters
    params_sleep = params$params_sleep
  }
  if (length(params_metrics) == 0) {
    params = load_params(group = "metrics") # for now just load all parameters
    params_metrics = params$params_metrics
  }
  if (length(params_rawdata) == 0) {
    params = load_params(group = "rawdata") # for now just load all parameters
    params_rawdata = params$params_rawdata
  }
  # If parameters were manually assigned then add them to params_sleep object, where 
  # existing value is overwritten
  if (length(input) > 0) {
    argNames = names(input)
    if (exists("relyonsleeplog") == TRUE)  params_sleep[["relyonguider"]] = params_sleep[["relyonsleeplog"]]
    expected_sleep_params = names(params_sleep)
    expected_metrics_params = names(params_metrics)
    expected_rawdata_params = names(params_rawdata)
    for (aN in argNames) {
      
      if (aN %in% expected_sleep_params == TRUE) { # Sleep
        if (is.null(input[[aN]])) {
          params_sleep[aN] = list(NULL)
        } else {
          params_sleep[[aN]] = input[[aN]]
        }
      } else if (aN %in% argNames == TRUE & aN %in% expected_metrics_params == TRUE) { # Metrics
        if (is.null(input[[aN]])) {
          params_metrics[aN] = list(NULL)
        } else {
          params_metrics[[aN]] = input[[aN]]
        }
      } else if (aN %in% argNames == TRUE & aN %in% expected_rawdata_params == TRUE) { # Rawdata
        if (is.null(input[[aN]])) {
          params_rawdata[aN] = list(NULL)
        } else {
          params_rawdata[[aN]] = input[[aN]]
        }
      }
    }
  }
  # Check class of parameter values
  params = check_params(params_sleep, params_metrics, params_rawdata)
  return(params)
}