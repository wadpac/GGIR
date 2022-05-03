load_params = function(group = c("sleep", "metrics", "rawdata", 
                                 "247", "phyact", "cleaning",
                                 "output", "general")) {
  if (length(group) == 0) {
    warning("\nArgument group not specified in load_params")
  }
  # initialize params objects
  params_sleep = params_metrics = params_rawdata = c()
  params_247 = params_phyact = params_cleaning = c()
  params_output = params_general = c()
  
  # if (length(jsonfile) > 0) {
  #   # PLACEHOLDER FOR WHEN JSON FILE CONFIG FILE WILL BE FACILITATE
  # } else {
  if ("sleep" %in% group) {  
    params_sleep = list(anglethreshold = 5, timethreshold = 5, 
                        ignorenonwear = TRUE, constrain2range = TRUE,
                        HASPT.algo = "HDCZA",
                        HASIB.algo = "vanHees2015", Sadeh_axis = "Y",
                        longitudinal_axis = c(),
                        HASPT.ignore.invalid = FALSE,
                        loglocation = c(), colid = 1, coln1 = 2,
                        nnights = c(),
                        relyonguider = FALSE, sleeplogidnum = TRUE,
                        def.noc.sleep = 1, 
                        sleeplogsep = ",", sleepwindowType = "SPT",
                        possible_nap_window = c(9, 18),
                        possible_nap_dur = c(15, 240),
                        nap_model = c()) 
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
                          do.brondcounts = FALSE,
                          hb = 15, lb = 0.2, n = 4)
  }
  if ("rawdata" %in% group) {
    params_rawdata = list(
      chunksize = 1, spherecrit = 0.3, minloadcrit = 72, printsummary = FALSE,
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
      rmc.check4timegaps = FALSE,  rmc.noise = 13,
      rmc.col.wear = c(), rmc.doresample = FALSE,
      interpolationType = 1,
      imputeTimegaps = TRUE)
  }
  if ("247" %in% group) {
    params_247 = list(qwindow = c(0,24), qlevels = c(),
                      qwindow_dateformat = "%d-%m-%Y", ilevels = c(),
                      IVIS_windowsize_minutes = 60, IVIS_epochsize_seconds = c(),
                      IVIS.activity.metric = 2, qM5L5 = c(),
                      MX.ig.min.dur = 10, M5L5res = 10, winhr = 5, iglevels = c(),
                      LUXthresholds = c(0, 100, 500, 1000, 3000, 5000, 10000), 
                      LUX_cal_constant = c(), LUX_cal_exponent = c(), LUX_day_segments = c(),
                      window.summary.size = 10, L5M5window = c(0, 24))
  }
  if ("phyact" %in% group) {
    params_phyact = list(mvpathreshold = 100, boutcriter = 0.8,
                         mvpadur = c(1,5,10), closedbout = FALSE,
                         boutcriter.in = 0.9, boutcriter.lig = 0.8,
                         boutcriter.mvpa = 0.8, threshold.lig = 40,
                         threshold.mod = 100, threshold.vig = 400,
                         boutdur.mvpa = c(1,5,10), boutdur.in = c(10,20,30),
                         boutdur.lig = c(1,5,10), frag.metrics = c(), bout.metric = 6)
  }
  if ("cleaning" %in% group) {
    params_cleaning = list(includedaycrit = 16, ndayswindow = 7,
                           selectdaysfile = c(), strategy = 1, maxdur = 0,
                           hrs.del.start = 0, hrs.del.end = 0, 
                           includedaycrit.part5 = 2/3, excludefirstlast.part5 = FALSE,
                           TimeSegments2ZeroFile = c(), do.imp = TRUE,
                           data_cleaning_file = c(), minimum_MM_length.part5 = 23,
                           excludefirstlast = FALSE, #<= to cleaning
                           includenightcrit = 16, #<= to cleaning
                           excludefirst.part4 = FALSE, # => to cleaning
                           excludelast.part4 = FALSE, max_calendar_days = 0)
  }
  if ("output" %in% group) {
    params_output = list(epochvalues2csv = FALSE, save_ms5rawlevels = FALSE,
                         save_ms5raw_format = "csv", save_ms5raw_without_invalid = TRUE,
                         storefolderstructure = FALSE, timewindow = c("MM","WW"), 
                         viewingwindow = 1, dofirstpage = TRUE, visualreport = TRUE,
                         week_weekend_aggregate.part5 = FALSE, do.part3.pdf = TRUE,
                         outliers.only = FALSE, criterror = 3, do.visual = TRUE,
                         do.sibreport = FALSE)
    
  }
  if ("general" %in% group) {
    params_general = list(overwrite = FALSE, acc.metric = "ENMO",
                          maxNcores = c(), print.filename = FALSE,
                          do.parallel = TRUE, windowsizes = c(5,900,3600),
                          desiredtz = "", configtz = c(), idloc = 1, dayborder = 0,
                          part5_agg2_60seconds = FALSE,
                          sensor.location = "wrist")
  }
  # }
  invisible(list(params_sleep = params_sleep,
                 params_metrics = params_metrics,
                 params_rawdata = params_rawdata,
                 params_247 = params_247,
                 params_phyact = params_phyact,
                 params_cleaning = params_cleaning,
                 params_output = params_output,
                 params_general = params_general))
}