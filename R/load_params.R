load_params = function(topic = c("sleep", "metrics", "rawdata",
                                 "247", "phyact", "cleaning",
                                 "output", "general")) {

  # initialize params objects
  params_sleep = params_metrics = params_rawdata = c()
  params_247 = params_phyact = params_cleaning = c()
  params_output = params_general = c()

  # if (length(jsonfile) > 0) {
  #   # PLACEHOLDER FOR WHEN JSON FILE CONFIG FILE WILL BE FACILITATE
  # } else {
  if ("sleep" %in% topic) {
    params_sleep = list(anglethreshold = 5, timethreshold = 5,
                        ignorenonwear = TRUE,
                        HASPT.algo = "HDCZA",
                        HASIB.algo = "vanHees2015", Sadeh_axis = "Y",
                        longitudinal_axis = c(),
                        HASPT.ignore.invalid = FALSE,
                        loglocation = c(), colid = 1, coln1 = 2,
                        nnights = c(),
                        relyonguider = FALSE,
                        def.noc.sleep = 1,
                        sleeplogsep = NULL, sleepwindowType = "SPT",
                        possible_nap_window = c(9, 18),
                        possible_nap_dur = c(15, 240),
                        possible_nap_gap = 0,
                        possible_nap_edge_acc = Inf,
                        nap_model = c(), sleepefficiency.metric = 1,
                        HDCZA_threshold = c(),
                        sib_must_fully_overlap_with_TimeInBed = c(TRUE, TRUE))
  }
  if ("metrics" %in% topic) {
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
                          do.neishabouricounts = FALSE,
                          hb = 15, lb = 0.2, n = 4,
                          zc.lb = 0.25, zc.hb = 3, zc.sb = 0.01, zc.order = 2, zc.scale = 1,
                          actilife_LFE = FALSE)
  }
  if ("rawdata" %in% topic) {
    params_rawdata = list(
      chunksize = 1, spherecrit = 0.3, minloadcrit = 168, printsummary = FALSE,
      do.cal = TRUE, backup.cal.coef = "retrieve", dynrange = c(),
      minimumFileSizeMB = 2, rmc.dec = ".",
      rmc.firstrow.acc = c(), rmc.firstrow.header = c(),
      rmc.header.length = c(), rmc.col.acc = 1:3,
      rmc.col.temp = c(), rmc.col.time = c(),
      rmc.unit.acc = "g",  rmc.unit.temp = "C",
      rmc.unit.time = "POSIX", rmc.format.time = "%Y-%m-%d %H:%M:%OS",
      rmc.bitrate = c(),  rmc.dynamic_range = c(),
      rmc.unsignedbit = TRUE, rmc.origin = "1970-01-01",
      rmc.desiredtz = NULL, rmc.configtz = NULL,  rmc.sf = c(),
      rmc.headername.sf = c(), rmc.headername.sn = c(),
      rmc.headername.recordingid = c(), rmc.header.structure = c(),
      rmc.check4timegaps = FALSE,  rmc.noise = 13, nonwear_range_threshold = 150,
      rmc.col.wear = c(), rmc.doresample = FALSE,
      interpolationType = 1,
      imputeTimegaps = TRUE, frequency_tol = 0.1, rmc.scalefactor.acc = 1)
  }
  if ("247" %in% topic) {
    params_247 = list(qwindow = c(0,24), qlevels = c(),
                      qwindow_dateformat = "%d-%m-%Y", ilevels = c(),
                      IVIS_windowsize_minutes = 60, IVIS_epochsize_seconds = c(),
                      IVIS.activity.metric = 2, IVIS_acc_threshold = 20, qM5L5 = c(),
                      MX.ig.min.dur = 10, M5L5res = 10, winhr = 5, iglevels = c(),
                      LUXthresholds = c(0, 100, 500, 1000, 3000, 5000, 10000),
                      LUX_cal_constant = c(), LUX_cal_exponent = c(), LUX_day_segments = c(),
                      L5M5window = c(0, 24), cosinor = FALSE,
                      part6CR = FALSE, part6HCA = FALSE,
                      part6Window = c("start", "end"),
                      clevels = c(30, 150), part6DFA = FALSE)
  }
  if ("phyact" %in% topic) {
    params_phyact = list(mvpathreshold = 100, boutcriter = 0.8,
                         mvpadur = c(1,5,10),
                         boutcriter.in = 0.9, boutcriter.lig = 0.8,
                         boutcriter.mvpa = 0.8, threshold.lig = 40,
                         threshold.mod = 100, threshold.vig = 400,
                         boutdur.mvpa = c(1,5,10), boutdur.in = c(10,20,30),
                         boutdur.lig = c(1,5,10), frag.metrics = c(),
                         part6_threshold_combi = NULL)
  }
  if ("cleaning" %in% topic) {
    params_cleaning = list(includedaycrit = 16, ndayswindow = 7,
                           strategy = 1, data_masking_strategy = 1,
                           maxdur = 0,
                           hrs.del.start = 0, hrs.del.end = 0,
                           includedaycrit.part5 = 2/3, excludefirstlast.part5 = FALSE,
                           TimeSegments2ZeroFile = c(), do.imp = TRUE,
                           data_cleaning_file = c(), minimum_MM_length.part5 = 23,
                           excludefirstlast = FALSE, #<= to cleaning
                           includenightcrit = 16, #<= to cleaning
                           excludefirst.part4 = FALSE, # => to cleaning
                           excludelast.part4 = FALSE, max_calendar_days = 0,
                           nonWearEdgeCorrection = TRUE, nonwear_approach = "2023",
                           segmentWEARcrit.part5 = 0.5,
                           segmentDAYSPTcrit.part5 = c(0.9, 0),
                           study_dates_file = c(), study_dates_dateformat = "%d-%m-%Y",
                           includecrit.part6 = c(2/3, 2/3),
                           includenightcrit.part5 = 0)
  }
  if ("output" %in% topic) {
    params_output = list(epochvalues2csv = FALSE, save_ms5rawlevels = FALSE,
                         save_ms5raw_format = "csv", save_ms5raw_without_invalid = TRUE,
                         storefolderstructure = FALSE, timewindow = c("MM","WW"),
                         viewingwindow = 1, dofirstpage = TRUE, visualreport = TRUE,
                         week_weekend_aggregate.part5 = FALSE, do.part3.pdf = TRUE,
                         outliers.only = FALSE, criterror = 3, do.visual = TRUE,
                         do.sibreport = FALSE, do.part2.pdf = TRUE,
                         sep_reports = ",", sep_config = ",", 
                         dec_reports = ".", dec_config = ".", 
                         visualreport_without_invalid = TRUE,
                         old_visualreport = TRUE, visualreport_hrsPerRow = 36,
                         visualreport_focus = "day",
                         visualreport_validcrit = 0, require_complete_lastnight_part5 = FALSE)

  }
  if ("general" %in% topic) {
    params_general = list(overwrite = FALSE, acc.metric = "ENMO",
                          maxNcores = c(), print.filename = FALSE,
                          do.parallel = TRUE, windowsizes = c(5,900,3600),
                          desiredtz = "", configtz = c(), idloc = 1, dayborder = 0,
                          part5_agg2_60seconds = FALSE,
                          sensor.location = "wrist",
                          expand_tail_max_hours = NULL, recordingEndSleepHour = NULL,
                          dataFormat = "raw", maxRecordingInterval = NULL,
                          extEpochData_timeformat = "%d-%m-%Y %H:%M:%S")
  }
  invisible(list(params_sleep = params_sleep,
                 params_metrics = params_metrics,
                 params_rawdata = params_rawdata,
                 params_247 = params_247,
                 params_phyact = params_phyact,
                 params_cleaning = params_cleaning,
                 params_output = params_output,
                 params_general = params_general))
}
