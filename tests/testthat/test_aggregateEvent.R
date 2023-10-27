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
})
