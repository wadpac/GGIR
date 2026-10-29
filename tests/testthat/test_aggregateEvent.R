library(GGIR)
context("aggregateEvent")
test_that("Events from external function are correctly aggregated", {
  skip_on_cran()
  
  # create dummy test data
  # 5 minutes, where minute 2-4 are the minutes of our window
  # minute 2 has no steps and ENMO = 0
  # minute 3 has 60 steps and ENMO = 80
  # minute 4 has 60 steps and ENMO = 120
  metashort = data.frame(ENMO = c(rep(0, 24), rep(0.08, 12), rep(0.12, 12), rep(1, 12)),
                         step_count = c(rep(0, 24), rep(5, 36)))
  cn_metashort = colnames(metashort)
  anwi_index = 1
  # varnum = metashort$step_count[anwindices]
  ws3 = 5
  anwi_nameindices = "_1234hrs"
  daysummary = matrix("", 1, 23)
  fi = 1
  di = 1
  ds_names = ""
  myfun = list(ilevels = c(0, 50, 100),
               clevels = c(0, 30, 50),
               qlevels = c(0.25, 0.5, 0.75),
               ebout.dur = c(1, 5, 10),
               ebout_th.cad = 30,
               ebout.th.acc = 50,
               ebout.criter = 0.8,
               ebout.condition = "AND")
  # run function  
  segmentInfo = list(anwi_nameindices = anwi_nameindices,
                     anwi_index = anwi_index)
  params_247 = load_params("247")$params_247
  eventAgg = aggregateEvent(metric_name = "step_count",
                            epochsize = ws3,
                            daysummary = daysummary,
                            ds_names = ds_names,
                            fi = fi, di = di, vari = metashort,
                            segmentInfo = segmentInfo, myfun, params_247)
  
  daysummary = as.data.frame(eventAgg$daysummary)
  names(daysummary)[1:length(eventAgg$ds_names)] = eventAgg$ds_names
  
  # Dimensions
  expect_equal(ncol(daysummary), 23)
  
  # total steps
  expect_equal(daysummary$ExtFunEvent_tot_step_count_1234hrs, "180")
  
  #mean cadence
  expect_equal(daysummary$ExtFunEvent_mn_cad_1234hrs, "36")
  
  # Percentiles of cadence
  expect_equal(daysummary$ExtFunEvent_cad_p25_1234hrs, "0")
  expect_equal(daysummary$ExtFunEvent_cad_p50_1234hrs, "60")
  expect_equal(daysummary$ExtFunEvent_cad_p75_1234hrs, "60")
  
  #total steps per acc range
  expect_equal(daysummary$`ExtFunEvent_tot_step_count_acc0-50mg_ENMO_1234hrs`, "0")
  expect_equal(daysummary$`ExtFunEvent_tot_step_count_acc50-100mg_ENMO_1234hrs`, "60")
  expect_equal(daysummary$ExtFunEvent_tot_step_count_accatleast100mg_ENMO_1234hrs, "120")
  
  #mean cadence per acc range
  expect_equal(daysummary$`ExtFunEvent_mn_cad_acc0-50mg_ENMO_1234hrs`, "0")
  expect_equal(daysummary$`ExtFunEvent_mn_cad_acc50-100mg_ENMO_1234hrs`, "60")
  expect_equal(daysummary$ExtFunEvent_mn_cad_accatleast100mg_ENMO_1234hrs, "60")
  
  #mean cadence per cadence range
  expect_equal(daysummary$`ExtFunEvent_mn_cad_cad0-30spm_1234hrs`, "0")
  expect_equal(daysummary$`ExtFunEvent_mn_cad_cad30-50spm_1234hrs`, "0")
  expect_equal(daysummary$ExtFunEvent_mn_cad_cadatleast50spm_1234hrs, "60")
  
  #total steps per cadence range
  expect_equal(daysummary$`ExtFunEvent_tot_step_count_cad0-30spm_1234hrs`, "0")
  expect_equal(daysummary$`ExtFunEvent_tot_step_count_cad30-50spm_1234hrs`, "0")
  expect_equal(daysummary$ExtFunEvent_tot_step_count_cadatleast50spm_1234hrs, "180")
  
  # mean acc per cadence range
  expect_equal(daysummary$`ExtFunEvent_mn_ENMO_cad0-30spm_1234hrs`, "0")
  expect_equal(daysummary$`ExtFunEvent_mn_ENMO_cad30-50spm_1234hrs`, "0")
  expect_equal(daysummary$ExtFunEvent_mn_ENMO_cadatleast50spm_1234hrs, "400")
  
  # time per cadence range
  expect_equal(daysummary$`ExtFunEvent_dur_cad0-30spm_1234hrs`, "2")
  expect_equal(daysummary$`ExtFunEvent_dur_cad30-50spm_1234hrs`, "0")
  expect_equal(daysummary$ExtFunEvent_dur_cadatleast50spm_1234hrs, "3")
  
  # MX
  # First create longer time series
  ENMO = seq(1/12, 1440, by = 1/12)
  step_count = rep(5, 1440 * 12)
  
  set.seed(123)
  steps = rnorm(n = length(which(ENMO > 40)), mean = 2, sd = 1)
  step_count[which(ENMO > 40)] = abs(steps)
  metashort = data.frame(ENMO = ENMO,
                         step_count = step_count)
  params_247[["winhr"]] = 2
  segmentInfo = list(anwi_nameindices = anwi_nameindices,
                     anwi_index = anwi_index,
                     anwi_t0 = 1,
                     anwi_t1 = 12 * 60 * 5)
  eventAgg = aggregateEvent(metric_name = "step_count",
                            epochsize = ws3,
                            daysummary = daysummary,
                            ds_names = ds_names,
                            fi = fi, di = di, vari = metashort,
                            segmentInfo = segmentInfo, myfun, params_247)
  daysummary = as.data.frame(eventAgg$daysummary)
  names(daysummary)[1:length(eventAgg$ds_names)] = eventAgg$ds_names
  expect_equal(daysummary$ExtFunEvent_L2hr_cad_1234hrs, 2.5, tolerance = 0.01)
  expect_equal(daysummary$ExtFunEvent_L2_cad_meancad_1234hrs, 24.17291, tolerance = 0.001)
  expect_equal(daysummary$ExtFunEvent_L2_cad_mean_ENMO_mg_1234hrs, 210041.7, tolerance = 0.05)
  expect_equal(daysummary$ExtFunEvent_M2hr_cad_1234hrs, 0)
  expect_equal(daysummary$ExtFunEvent_M2_cad_meancad_1234hrs, 36.26251, tolerance = 0.05)
  expect_equal(daysummary$ExtFunEvent_M2_cad_mean_ENMO_mg_1234hrs, 60041.67, tolerance = 0.01)
  
  # Event aggregation per acceleration level ignores non-acceleration columns
  ws3 = 5
  metashort = data.frame(ENMO = c(rep(0, 24), rep(0.08, 12), rep(0.12, 12), rep(1, 12)),
                         step_count = c(rep(0, 24), rep(5, 36)),
                         activity = factor(rep(c("sit", "walk"), each = 30)))
  myfun = list(ilevels = c(0, 50, 100), clevels = c(0, 30, 50),
               qlevels = c(0.25, 0.5, 0.75))
  segmentInfo = list(anwi_nameindices = "_1234hrs", anwi_index = 1)
  params_247 = load_params("247")$params_247
  eventAgg = aggregateEvent(metric_name = "step_count", epochsize = ws3,
                            daysummary = matrix("", 1, 23), ds_names = "",
                            fi = 1, di = 1, vari = metashort,
                            segmentInfo = segmentInfo, myfun, params_247)
  # no variable is derived from the type column "activity"
  expect_false(any(grepl("activity", eventAgg$ds_names)))
  # same variables as without the type column
  expect_equal(length(eventAgg$ds_names), 23)
  expect_true("ExtFunEvent_tot_step_count_acc50-100mg_ENMO_1234hrs" %in% eventAgg$ds_names)

  # Multiple event outputs receive distinct names for every summary metric.
  multi_vari = data.frame(
    ENMO = c(0.01, 0.02, 0.03, 0.04),
    event_a = c(1, 0, 2, 0),
    event_b = c(0, 3, 0, 4)
  )
  multi_myfun = list(
    colnames = c("event_a", "event_b"),
    reporttype = c("event", "event"),
    ilevels = 0,
    clevels = 0,
    qlevels = 0.5
  )
  multi_params = load_params("247")$params_247
  multi_params[["winhr"]] = numeric()
  multi_summary = matrix(NA_real_, nrow = 1, ncol = 100)
  multi_names = rep("", 100)
  multi_segment = list(anwi_nameindices = "_0-24hr", anwi_index = 1)

  event_a = aggregateEvent(
    metric_name = "event_a", epochsize = 5,
    daysummary = multi_summary, ds_names = multi_names,
    fi = 1, di = 1, vari = multi_vari, segmentInfo = multi_segment,
    myfun = multi_myfun, params_247 = multi_params
  )
  event_b = aggregateEvent(
    metric_name = "event_b", epochsize = 5,
    daysummary = event_a$daysummary, ds_names = event_a$ds_names,
    fi = event_a$fi, di = 1, vari = multi_vari, segmentInfo = multi_segment,
    myfun = multi_myfun, params_247 = multi_params
  )
  used_names = event_b$ds_names[nzchar(event_b$ds_names)]
  expect_false(anyDuplicated(used_names) > 0)
  expect_true(any(grepl("event_a", used_names)))
  expect_true(any(grepl("event_b", used_names)))

  bout_myfun = c(multi_myfun, list(
    ebout.dur = 1,
    ebout.th.cad = 0,
    ebout.th.acc = 0,
    ebout.criter = 1,
    ebout.condition = "AND"
  ))
  bout_a = detectEventBouts(
    myfun = bout_myfun, varnum_event = multi_vari$event_a,
    varnum = multi_vari$ENMO, UnitReScale = 1000,
    daysummary = matrix(NA_real_, 1, 20), ds_names = rep("", 20),
    di = 1, fi = 1, ws3 = 5, boutnameEnding = "ENMO_0-24hr",
    metric_name = "event_a"
  )
  bout_b = detectEventBouts(
    myfun = bout_myfun, varnum_event = multi_vari$event_b,
    varnum = multi_vari$ENMO, UnitReScale = 1000,
    daysummary = bout_a$daysummary, ds_names = bout_a$ds_names,
    di = 1, fi = bout_a$fi, ws3 = 5, boutnameEnding = "ENMO_0-24hr",
    metric_name = "event_b"
  )
  bout_names = bout_b$ds_names[nzchar(bout_b$ds_names)]
  expect_false(anyDuplicated(bout_names) > 0)
})
