library(GGIR)
context("External function reporttype type")

test_that("Embedding external functions with reporttype = 'type'", {
  skip_on_cran()
  
  # reporttype = 'type' validates the myfun object

  type_fun = function(data, parameters) {
    rep(c("rest", "walk"), length.out = floor(nrow(data) / 15))
  }

  myfun = list(FUN = type_fun,
               parameters = NULL,
               expected_sample_rate = 3,
               expected_unit = "g",
               colnames = "activity_type",
               minlength = 1,
               outputres = 5,
               outputtype = "character",
               aggfunction = function(x) x[1],
               timestamp = FALSE,
               reporttype = "type")

  expect_equal(GGIR:::check_myfun(myfun, windowsizes = 5), 0)

  myfun_bad = myfun
  myfun_bad$colnames = c("activity_type", "activity_type_2")
  expect_error(
    GGIR:::check_myfun(myfun_bad, windowsizes = 5),
    regexp = "reporttype = 'type'.*only one output column"
  )
  
  
  
  # reporttype = 'type' creates the expected character output
  
  sf = 3
  ws3 = 5
  data = matrix(c(rep(1, 15), rep(2, 15), rep(3, 15)), ncol = 3)
  colnames(data) = c("x", "y", "z")
  
  type_fun = function(data, parameters) {
    data.frame(activity_type = c("rest", "walk", "rest"))
  }
  
  myfun = list(FUN = type_fun,
               parameters = NULL,
               expected_sample_rate = sf,
               expected_unit = "g",
               colnames = "activity_type",
               minlength = 1,
               outputres = ws3,
               outputtype = "character",
               aggfunction = function(x) x[1],
               timestamp = FALSE,
               reporttype = "type")
  
  out = expect_warning(
    GGIR:::applyExtFunction(data, myfun, sf, ws3)$OutputExternalFunction,
    regexp = "Note: If function applyExtFunction is used directly"
  )
  
  expect_equal(nrow(out), 3)
  expect_equal(ncol(out), 1)
  expect_equal(as.character(out[, 1]), c("rest", "walk", "rest"))
  
  
  # reporttype = 'type' is converted to seconds per epoch in part 5
  IMP = list(
    metashort = data.frame(
      time = as.POSIXct(
        "2016-06-23 09:00:00",
        tz = "Europe/London"
      ) + c(0, 5, 10, 15, 20),
      ENMO = c(0.01, 0.02, 0.03, 0.04, 0.05),
      activity_type = c("rest", "walk", "walk", NA, "rest"),
      stringsAsFactors = FALSE
    ),
    rout = matrix(0, nrow = 1, ncol = 5),
    windowsizes = c(5, 60, 60)
  )
  
  M = list(
    metalong = data.frame(timestamp = IMP$metashort$time),
    metashort = IMP$metashort
  )
  
  params_247 = list()
  params_general = list(
    sensor.location = "wrist",
    acc.metric = "ENMO",
    windowsizes = c(5, 60, 60)
  )
  
  myfun = list(
    FUN = identity,
    parameters = NULL,
    expected_sample_rate = 3,
    expected_unit = "g",
    colnames = "activity_type",
    minlength = 1,
    outputres = 5,
    outputtype = "character",
    timestamp = FALSE,
    reporttype = "type"
  )
  
  ts = g.part5_initialise_ts(
    IMP = IMP,
    M = M,
    params_247 = params_247,
    params_general = params_general,
    myfun = myfun
  )
  
  type_cols = grep("^ExtFunType_", names(ts), value = TRUE)
  
  expect_equal(
    type_cols,
    c(
      "ExtFunType_activity_type_rest_s",
      "ExtFunType_activity_type_walk_s"
    )
  )
  expect_equal(ts$ExtFunType_activity_type_rest_s, c(5, 0, 0, 0, 5))
  expect_equal(ts$ExtFunType_activity_type_walk_s, c(0, 5, 5, 0, 0))
  
  
  # reporttype = 'type' identifies bout levels in the requested order
  ws3 = 5
  type_ts = data.frame(
    time = seq_len(12),
    ACC = rep(0.05, 12),
    diur = rep(0, 12),
    nonwear = rep(0, 12),
    guider = rep("unknown", 12),
    ExtFunType_activity_type_rest_s =
      c(5, 5, 5, 0, 5, 5, 0, 0, 5, 0, 0, 0),
    ExtFunType_activity_type_walk_s =
      c(0, 0, 0, 5, 0, 0, 5, 5, 0, 5, 5, 5),
    stringsAsFactors = FALSE
  )
  
  myfun = list(
    colnames = "activity_type",
    reporttype = "type",
    tbout.dur = c(1, 5),
    tbout.criter = 1,
    tbout.order = "walk"
  )
  
  type_levels = GGIR:::identify_levels_ExtFunType(
    ts = type_ts,
    myfun = myfun,
    ws3 = ws3
  )
  
  expect_equal(names(type_levels$TLEVELS), c("walk", "rest"))
  expect_equal(
    type_levels$Tnames,
    c(
      "walk_bts_5",
      "walk_bts_1_5",
      "rest_bts_5",
      "rest_bts_1_5"
    )
  )
  expect_equal(
    dim(type_levels$TOLEVELS),
    c(12, 2)
  )
  expect_equal(
    colnames(type_levels$TOLEVELS),
    c("rest", "walk")
  )
  
  
  # aggregateType reports total type time and acceleration strata
  vari = data.frame(
    timestamp = seq_len(8),
    ENMO = c(0.05, 0.05, 0.10, 0.10, 0.20, 0.20, 0.20, 0.05),
    activity_type = c(
      "rest", "rest", "walk", "walk",
      "walk", "rest", "rest", "rest"
    ),
    stringsAsFactors = FALSE
  )
  
  daysummary = matrix(NA_real_, nrow = 1, ncol = 30)
  ds_names = rep("", 30)
  
  myfun = list(
    colnames = "activity_type",
    ilevels = c(0, 80, 200),
    reporttype = "type"
  )
  
  segmentInfo = list(
    anwi_nameindices = "_0-24hr",
    anwi_index = 1
  )
  
  out = GGIR:::aggregateType(
    metric_name = "activity_type",
    epochsize = 5,
    daysummary = daysummary,
    ds_names = ds_names,
    fi = 1,
    di = 1,
    vari = vari,
    segmentInfo = segmentInfo,
    myfun = myfun
  )
  
  expect_equal(
    out$ds_names[1:2],
    c(
      "ExtFunType_tot_activity_type_rest_0-24hr",
      "ExtFunType_tot_activity_type_walk_0-24hr"
    )
  )
  expect_equal(
    out$daysummary[1, 1:2],
    c(25 / 60, 15 / 60),
    tolerance = 1e-10
  )
  
  expected_strata = c(
    "0-80mg_ENMO",
    "80-200mg_ENMO",
    "atleast200mg_ENMO"
  )
  
  for (i in seq_along(expected_strata)) {
    cols = grep(
      paste0("_acc", expected_strata[i], "_0-24hr$"),
      out$ds_names,
      value = FALSE
    )
    expect_equal(length(cols), 2)
  }
  
  rest_cols = grep(
    "^ExtFunType_tot_activity_type_rest_acc.*_0-24hr$",
    out$ds_names,
    value = FALSE
  )
  walk_cols = grep(
    "^ExtFunType_tot_activity_type_walk_acc.*_0-24hr$",
    out$ds_names,
    value = FALSE
  )
  
  expect_equal(length(rest_cols), 3)
  expect_equal(length(walk_cols), 3)
  
  names_rest = out$ds_names[rest_cols]
  names_walk = out$ds_names[walk_cols]
  
  expected_rest = c(
    "ExtFunType_tot_activity_type_rest_acc0-80mg_ENMO_0-24hr",
    "ExtFunType_tot_activity_type_rest_acc80-200mg_ENMO_0-24hr",
    "ExtFunType_tot_activity_type_rest_accatleast200mg_ENMO_0-24hr"
  )
  expected_walk = c(
    "ExtFunType_tot_activity_type_walk_acc0-80mg_ENMO_0-24hr",
    "ExtFunType_tot_activity_type_walk_acc80-200mg_ENMO_0-24hr",
    "ExtFunType_tot_activity_type_walk_accatleast200mg_ENMO_0-24hr"
  )
  
  expect_equal(names_rest, expected_rest)
  expect_equal(names_walk, expected_walk)
  
  expect_equal(
    out$daysummary[1, rest_cols],
    c(15 / 60, 0, 10 / 60),
    tolerance = 1e-10
  )
  expect_equal(
    out$daysummary[1, walk_cols],
    c(0, 10 / 60, 5 / 60),
    tolerance = 1e-10
  )
  
  
  # part 5 external type aggregation calculates SPT and day summaries
  ts = data.frame(
    time = 1:50,
    ACC = 1:50 / 1000,
    diur = c(rep(1, 25), rep(0, 25)),
    ExtFunType_activity_type_rest_s = c(rep(0, 3), rep(5, 25), rep(0, 22)),
    ExtFunType_activity_type_walk_s = c(rep(5, 3), rep(0, 25), rep(5, 22))
  )
  
  myfun = list(
    colnames = "activity_type",
    reporttype = "type",
    tbout.dur = c(1, 2, 5, 10),
    tbout.criter = 0.8,
    tbout.order = c("walk", "rest")
  )
  
  type_levels = identify_levels_ExtFunType(ts, myfun, ws3 = 5)
  
  TLEVELS = type_levels$TLEVELS
  TOLEVELS = type_levels$TOLEVELS
  Tnames = type_levels$Tnames
  
  dsummary = matrix("", nrow = 1, ncol = 100)
  ds_names = rep("", 100)
  
  out = GGIR:::g.part5_analyseSegment_ExtFunType(
    ts = ts,
    sse = 1:nrow(ts),
    TOLEVELS = TOLEVELS,
    TLEVELS = TLEVELS,
    Tnames = Tnames,
    myfun = myfun,
    ws3new = 5,
    dsummary = dsummary,
    ds_names = ds_names,
    si = 1,
    fi = 1
  )
  
  # build-up expected names generated
  prefix = "ExtFunType"
  outcomes = c("dur", "ACC", "Nblocks", "Nbouts")
  windows = c("spt", "day")
  levels = c("walk", "rest")
  boutdurs = c("10", "5_10", "2_5", "1_2")
  periods = c(paste0(levels, "_unbt"),
              paste0(levels, "_bts_", rep(boutdurs, each = length(levels))),
              paste0("total_", levels))
  vars = expand.grid(outcome = outcomes,
                     window = windows,
                     period = periods)
  vars = with(vars, paste(prefix, outcome, window, period, sep = "_"))
  expected_names = ifelse(grepl("^ExtFunType_dur_", vars),
                          paste0(vars, "_min"),
                          ifelse(grepl("^ExtFunType_ACC_", vars),
                                 paste0(vars, "_mg"),
                                 vars))
  
  expect_all_true(out$ds_names[out$ds_names != ""] %in% expected_names)
  
  names = out$ds_names[1:length(expected_names)]
  values = out$dsummary[1, 1:length(expected_names)]
  names(values) = names
  
  # expected rest and walk durations during day
  expected_rest = sum(ts$ExtFunType_activity_type_rest_s[which(ts$diur == 0)]) / 60
  expected_walk = sum(ts$ExtFunType_activity_type_walk_s[which(ts$diur == 0)]) / 60
  
  expect_equal(
    as.numeric(values[c("ExtFunType_dur_day_total_rest_min",
                        "ExtFunType_dur_day_total_walk_min")]),
    c(expected_rest, expected_walk),
    tolerance = 1e-10
  )
  
  # expected rest and walk mean acc during day
  expected_rest = mean(ts$ACC[which(ts$ExtFunType_activity_type_rest_s > 0 & ts$diur == 0)])
  expected_walk = mean(ts$ACC[which(ts$ExtFunType_activity_type_walk_s > 0 & ts$diur == 0)])
  
  expect_equal(
    as.numeric(values[c("ExtFunType_ACC_day_total_rest_mg",
                        "ExtFunType_ACC_day_total_walk_mg")]),
    c(expected_rest, expected_walk),
    tolerance = 1e-10
  )
  
  
  # detectTypeBouts calculates duration, number and mean duration
  myfun = list(
    colnames = "activity_type",
    reporttype = "type",
    tbout.dur = 1,
    tbout.criter = 1
  )
  
  varnum_type = c(rep("rest", 3), rep("walk", 2), rep("rest", 2), rep("walk", 3))
  varnum = seq_along(varnum_type)
  
  daysummary = matrix(NA_real_, nrow = 1, ncol = 50)
  ds_names = rep("", 50)
  
  out = GGIR:::detectTypeBouts(myfun = myfun,
                               varnum_type = varnum_type,
                               varnum = varnum,
                               UnitReScale = 1,
                               daysummary = daysummary,
                               ds_names = ds_names,
                               di = 1,
                               fi = 1,
                               ws3 = 60,
                               boutnameEnding = "0-24hr")
  
  expect_equal(out$ds_names[1:6],
               c("ExtFunType_totdur_B1M100%_rest_0-24hr",
                 "ExtFunType_number_B1M100%_rest_0-24hr",
                 "ExtFunType_meandur_B1M100%_rest_0-24hr",
                 "ExtFunType_totdur_B1M100%_walk_0-24hr",
                 "ExtFunType_number_B1M100%_walk_0-24hr",
                 "ExtFunType_meandur_B1M100%_walk_0-24hr"),
               ignore_attr = TRUE)
  
  expect_equal(
    out$daysummary[1, 1:6], c(5, 2, 2.5, 5, 2, 2.5),
    tolerance = 1e-10
  )
  
  
  # part 2 and part 5 reports contain external type output
  Ndays = 2
  create_test_acc_csv(Nmin = Ndays * 1440, sf = 3)
  
  fn = "123A_testaccfile.csv"
  dn = "output_test"
  if (file.exists(dn)) unlink(dn, recursive = TRUE)
  
  type_fun = function(data, parameters) {
    n = floor(nrow(data) / 15)
    out = rep(c("rest", "walk", "rest", "walk"), length.out = n)
    data.frame(activity_type = out, stringsAsFactors = FALSE)
  }
  
  myfun = list(FUN = type_fun,
               parameters = NULL,
               expected_sample_rate = 3,
               expected_unit = "g",
               colnames = "activity_type",
               minlength = 1,
               outputres = 5,
               outputtype = "character",
               aggfunction = function(x) x[1],
               timestamp = FALSE,
               reporttype = "type",
               ilevels = c(0, 80, 200),
               tbout.dur = c(1, 5),
               tbout.criter = 0.8,
               tbout.order = c("walk", "rest"))
  
  GGIR(mode = 1:4,
       datadir = fn,
       outputdir = getwd(),
       studyname = "test",
       idloc = 2,
       do.report = c(2, 4),
       visualreport = FALSE,
       windowsizes = c(5, 60, 60),
       myfun = myfun,
       includedaycrit = 2,
       verbose = FALSE)
  
  part2_daytype = file.path(dn, "results", "part2_daytypesummary.csv")
  part2_type = file.path(dn, "results", "part2_typesummary.csv")
  
  expect_true(file.exists(part2_daytype))
  expect_true(file.exists(part2_type))
  
  daytype_report = read.csv(part2_daytype)
  type_report = read.csv(part2_type)
  
  expect_true(any(grepl("^tot_activity_type", names(daytype_report))))
  expect_true(any(grepl("^meandur_B1M80", names(daytype_report))))
  expect_true(any(grepl("^number_B5M80", names(daytype_report))))
  expect_true(any(grepl("^totdur_B5M80", names(daytype_report))))
  
  expect_true(any(grepl("^AD_tot_activity_type", names(type_report))))
  expect_true(any(grepl("^WE_meandur_B1M80", names(type_report))))
  expect_true(any(grepl("^WWD_number_B5M80", names(type_report))))
  expect_true(any(grepl("^WWE_totdur_B5M80", names(type_report))))
  
  
  GGIR(mode = 5,
       datadir = fn,
       outputdir = getwd(),
       studyname = "test",
       idloc = 2,
       windowsizes = c(5, 60, 60),
       myfun = myfun,
       timewindow = "MM",
       includedaycrit.part5 = 2/24,
       minimum_MM_length.part5 = 2,
       part5_agg2_60seconds = TRUE,
       verbose = FALSE)
  
  p5_files = list.files(file.path(dn, "results"), 
                        pattern = "^part5_extfuntype_daysummary_.*\\.csv$",
                        full.names = TRUE, recursive = T)
  p5_files_person = list.files(file.path(dn, "results"),
                               pattern = "^part5_extfuntype_personsummary_.*\\.csv$",
                               full.names = TRUE, recursive = T)
  
  expect_true(length(p5_files) > 0)
  expect_true(length(p5_files_person) > 0)
  
  p5_report = read.csv(p5_files[1])
  p5_person = read.csv(p5_files_person[1])
  
  expect_true(any(grepl("^dur_day_", names(p5_report))))
  expect_true(any(grepl("^ACC_day_", names(p5_report))))
  expect_true(any(grepl("^Nblocks_day_", names(p5_report))))
  expect_true(any(grepl("^Nbouts_day_", names(p5_report))))
  
  expect_true(any(grepl("^dur_day_", names(p5_person))))
  expect_true(any(grepl("^Nblocks_day_", names(p5_person))))
  
  if (file.exists(fn)) file.remove(fn)
  if (dir.exists(dn)) unlink(dn, recursive = TRUE)
  
})
