load_params = function(group = c("sleep", "metrics")) {
  params_sleep = params_metrics= c()
  # if (length(jsonfile) > 0) {
  #   # PLACEHOLDER FOR WHEN JSON FILE CONFIG FILE WILL BE FACILITATE
  # } else {
  if ("sleep" %in% group) {  
    params_sleep = list(anglethreshold = 5,
                        timethreshold = 5, 
                        ignorenonwear = TRUE,
                        constrain2range = TRUE,
                        sensor.location = "wrist",
                        HASPT.algo = "HDCZA",
                        HASIB.algo ="vanHees2015",
                        Sadeh_axis = "Y",
                        longitudinal_axis = c(),
                        HASPT.ignore.invalid = FALSE,
                        loglocation = c(),
                        colid = 1,
                        coln1 = 2,
                        nnights = c(),
                        outliers.only = FALSE,
                        excludefirstlast = FALSE,
                        criterror = 3,
                        includenightcrit = 16,
                        relyonguider = FALSE,
                        sleeplogidnum = TRUE,
                        def.noc.sleep = 1,
                        excludefirst.part4 = FALSE,
                        excludelast.part4 = FALSE,
                        sleeplogsep = ",",
                        sleepwindowType = "SPT",
                        sensor.location = "wrist",
                        do.visual = FALSE)
  }
  if ("metrics" %in% group) {  
    params_metrics = list(do.anglex = FALSE,
                          do.angley = FALSE,
                          do.anglez = TRUE,
                          do.zcx = FALSE,
                          do.zcy = FALSE,
                          do.zcz = FALSE)
  }
  # }
  invisible(list(params_sleep = params_sleep, params_metrics=params_metrics))
}