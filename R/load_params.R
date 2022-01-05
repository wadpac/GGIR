load_params = function(group = c("sleep", "metrics", "rawdata")) {
  params_sleep = params_metrics = params_rawdata = c()
  # if (length(jsonfile) > 0) {
  #   # PLACEHOLDER FOR WHEN JSON FILE CONFIG FILE WILL BE FACILITATE
  # } else {
  if ("sleep" %in% group) {  
    params_sleep = list(anglethreshold = 5, timethreshold = 5, 
                        ignorenonwear = TRUE, constrain2range = TRUE,
                        sensor.location = "wrist", HASPT.algo = "HDCZA",
                        HASIB.algo ="vanHees2015", Sadeh_axis = "Y",
                        longitudinal_axis = c(),
                        HASPT.ignore.invalid = FALSE,
                        loglocation = c(), colid = 1, coln1 = 2,
                        nnights = c(), outliers.only = FALSE,
                        excludefirstlast = FALSE,
                        criterror = 3, includenightcrit = 16,
                        relyonguider = FALSE, sleeplogidnum = TRUE,
                        def.noc.sleep = 1, excludefirst.part4 = FALSE,
                        excludelast.part4 = FALSE,
                        sleeplogsep = ",", sleepwindowType = "SPT",
                        sensor.location = "wrist", do.visual = FALSE)
  }
  if ("metrics" %in% group) {
    params_metrics = list(do.anglex = FALSE, do.angley = FALSE, do.anglez = TRUE,
                          do.zcx = FALSE, do.zcy = FALSE, do.zcz = FALSE,
                          do.enmo = TRUE, do.lfenmo = FALSE, do.en = FALSE,
                          do.mad = FALSE, do.enmoa = FALSE,
                          do.roll_med_acc_x = FALSE, do.roll_med_acc_y = FALSE,
                          do.roll_med_acc_z = FALSE, do.dev_roll_med_acc_x = FALSE,
                          do.dev_roll_med_acc_y = FALSE, do.dev_roll_med_acc_z = FALSE,
                          do.bfen = FALSE, do.hfen = FALSE, do.hfenplus = FALSE, do.lfen = FALSE,
                          do.lfx = FALSE, do.lfy = FALSE, do.lfz = FALSE,
                          do.hfx = FALSE, do.hfy = FALSE, do.hfz = FALSE,
                          do.bfx = FALSE, do.bfy = FALSE, do.bfz = FALSE,
                          hb = 15, lb = 0.2, n = 4)
  }
  if ("rawdata" %in% group) {
    params_rawdata = list(
      chunksize = 1, spherecrit=0.3, minloadcrit = 72, printsummary = FALSE,
      do.cal = TRUE, backup.cal.coef = "retrieve", dynrange = c(),
      minimumFileSizeMB = 2, rmc.dec = ".",
      rmc.firstrow.acc = c(), rmc.firstrow.header = c(),
      rmc.header.length = c(), rmc.col.acc = 1:3,
      rmc.col.temp = c(), rmc.col.time = c(),
      rmc.unit.acc = "g",  rmc.unit.temp = "C",
      rmc.unit.time = "POSIX", rmc.format.time = "%Y-%m-%d %H:%M:%OS",
      rmc.bitrate = c(),  rmc.dynamic_range = c(),
      rmc.unsignedbit = TRUE, rmc.origin = "1970-01-01",
      rmc.desiredtz = "",  rmc.sf = c(),
      rmc.headername.sf = c(), rmc.headername.sn = c(),
      rmc.headername.recordingid = c(), rmc.header.structure = c(),
      rmc.check4timegaps = FALSE,  rmc.noise = c(),
      rmc.col.wear = c(), rmc.doresample = FALSE,
      interpolationType = 1)
  }
  
  # }
  invisible(list(params_sleep = params_sleep, params_metrics=params_metrics, params_rawdata = params_rawdata))
}