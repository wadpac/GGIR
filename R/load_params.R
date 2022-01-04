load_params = function(jsonfile = c(), group = "sleep") {
  if (length(jsonfile) > 0) {
    # PLACEHOLDER FOR WHEN JSON FILE CONFIG FILE WILL BE FACILITATE
  } else {
    if (group == "sleep") {  
      params_sleep = list(anglethreshold = 5,
                          timethreshold = 5, 
                          ignorenonwear = TRUE,
                          constrain2range = TRUE,
                          sensor.location = "wrist",
                          HASPT.algo = "HDCZA",
                          HASIB.algo ="vanHees2015",
                          Sadeh_axis = "Y",
                          longitudinal_axis = 2,
                          HASPT.ignore.invalid = FALSE)
    }
  }
  invisible(list(params_sleep = params_sleep))
}