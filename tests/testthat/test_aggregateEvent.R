library(GGIR)
context("aggregateEvent")
test_that("Events from external function are correctly aggregated", {
  skip_on_cran()
  
  # create dummy test data
  # 5 minutes, where minute 2-4 are the minutes of our window
  # minute 2 has no steps and ENMO = 0
  # minute 3 has 60 steps and ENMO = 80
  # minute 4 has 60 steps and ENMO = 120
  metashort = data.frame(ENMO = c(rep(0, 12), rep(0, 12), rep(0.08, 12), rep(0.12, 12), rep(1, 12)),
                         step_count = c(rep(0, 24), rep(5, 36)))
  cn_metashort = colnames(metashort)
  anwindices = 13:48
  anwi_index = 1
  varnum = metashort$step_count[anwindices]
  ws3 = 5
  anwi_nameindices = "_1234hrs"
  daysummary = matrix("", 1, 25)
  fi = 1
  di = 1
  ds_names = ""
  myfun = list(ilevels = c(0, 50, 100),
               clevels = c(0, 30, 50),
               qlevels = c(0.25, 0.5, 0.75))
  # run function  
  eventAgg = aggregateEvent(metric_name = "step_count", varnum = varnum,
                            epochsize = ws3, anwi_nameindices = anwi_nameindices,
                            anwi_index = anwi_index, ds_names = ds_names,
                            fi = fi, di = di, daysummary = daysummary,
                            metashort = metashort,  anwindices = anwindices, myfun)
  
  daysummary = as.data.frame(eventAgg$daysummary)
  names(daysummary)[1:length(eventAgg$ds_names)] = eventAgg$ds_names
  
  # total steps
  expect_equal(daysummary$tot_step_count_1234hrs, "120")
  
  #mean cadence
  expect_equal(daysummary$mn_cad_1234hrs, "40")
  
  # Percentiles of cadence
  expect_equal(daysummary$cad_p25_1234hrs, "0")
  expect_equal(daysummary$cad_p50_1234hrs, "60")
  expect_equal(daysummary$cad_p75_1234hrs, "60")
  
  #total steps per acc range
  expect_equal(daysummary$`tot_step_count_acc0-50mg_ENMO_1234hrs`, "0")
  expect_equal(daysummary$`tot_step_count_acc50-100mg_ENMO_1234hrs`, "60")
  expect_equal(daysummary$tot_step_count_accatleast100mg_ENMO_1234hrs, "60")
  
  #mean cadence per acc range
  expect_equal(daysummary$`mn_cad_acc0-50mg_ENMO_1234hrs`, "0")
  expect_equal(daysummary$`mn_cad_acc50-100mg_ENMO_1234hrs`, "60")
  expect_equal(daysummary$mn_cad_accatleast100mg_ENMO_1234hrs, "60")
  
  #mean cadence per cadence range
  expect_equal(daysummary$`mn_cad_cad0-30spm_1234hrs`, "0")
  expect_equal(daysummary$`mn_cad_cad30-50spm_1234hrs`, "0")
  expect_equal(daysummary$mn_cad_cadatleast50spm_1234hrs, "60")
  
  #total steps per cadence range
  expect_equal(daysummary$`tot_step_count_cad0-30spm_1234hrs`, "0")
  expect_equal(daysummary$`tot_step_count_cad30-50spm_1234hrs`, "0")
  expect_equal(daysummary$tot_step_count_cadatleast50spm_1234hrs, "120")
  
  # mean acc per cadence range
  expect_equal(daysummary$`mn_ENMO_cad0-30spm_1234hrs`, "0")
  expect_equal(daysummary$`mn_ENMO_cad30-50spm_1234hrs`, "0")
  expect_equal(daysummary$mn_ENMO_cadatleast50spm_1234hrs, "100")
  
  # time per cadence range
  expect_equal(daysummary$`dur_cad0-30spm_1234hrs`, "1")
  expect_equal(daysummary$`dur_cad30-50spm_1234hrs`, "0")
  expect_equal(daysummary$dur_cadatleast50spm_1234hrs, "2")
})
