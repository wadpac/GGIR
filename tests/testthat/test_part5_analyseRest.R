library(GGIR)
context("g.part5.analyseRest")

tz = "Pacific/Auckland"
ds_names = rep("", 40)
dsummary = matrix("", 1, 40)
startday = as.POSIXct(x = "2022-06-02 08:00:00", tz = tz)
ts = data.frame(time = seq(startday, startday + (16 * 3600), by = 60))
ts$sibdetection = 0
params_general = load_params()$params_general
params_general[["desiredtz"]] = tz

test_that("Overlap 1 nap and 1 sib", {
  fi = 1
  di = 1
  sibreport = data.frame(ID = rep("test123", 2), type = c("nap", "sib"),
                         start = c("2022-06-02 14:00:00", "2022-06-02 14:05:00"),
                         end = c("2022-06-02 14:20:00", "2022-06-02 14:20:00"))
  sibreport$duration = as.numeric(difftime(time1 = sibreport$end,
                                           time2 = sibreport$start, units = "mins", tz = tz))
  params_sleep = load_params()$params_sleep
  params_sleep[["possible_nap_dur"]] =  c(0, 240)
  params_sleep[["possible_nap_window"]] =  c(9, 18)
  
  out_add_nap = g.part5.addNaps(sibreport = sibreport, ts = ts,
                                params_general = params_general,
                                params_sleep = params_sleep)
  ts = out_add_nap$ts
  sibreport = out_add_nap$sibreport
  long_nap_boutsi = out_add_nap$long_nap_boutsi
  
  restAnalyses = g.part5.analyseRest(sibreport = sibreport, dsummary = dsummary,
                                     ds_names = ds_names, fi = fi, di = di,
                                     ts = ts, tz = tz,
                                     params_sleep = params_sleep,
                                     long_nap_boutsi = long_nap_boutsi)
  fi = restAnalyses$fi
  di = restAnalyses$di
  dsummary = restAnalyses$dsummary
  ds_names = restAnalyses$ds_names
  dsummary = as.numeric(dsummary[, which(ds_names != "")])
  ds_names = ds_names[ds_names != ""]
  names(dsummary) = ds_names
  dsummary = as.data.frame(t(dsummary))
  
  expect_equal(dsummary$nbouts_day_denap, 1)
  expect_equal(dsummary$nbouts_day_srnap, 1)
  expect_equal(dsummary$frag_mean_dur_denap_day, 15)
  expect_equal(dsummary$frag_mean_dur_srnap_day, 20)
  expect_equal(dsummary$perc_denap_overl_srnap, 100)
  expect_equal(dsummary$perc_srnap_overl_denap, 75)
  expect_equal(sum(dsummary), 323)
})

test_that("Overlap 1 nonwear and 1 sib", {
  fi = 1
  di = 1
  sibreport = data.frame(ID = rep("test123", 2), type = c("nonwear", "sib"),
                         start = c("2022-06-02 14:00:00", "2022-06-02 14:05:00"),
                         end = c("2022-06-02 14:20:00", "2022-06-02 14:20:00"))
  sibreport$duration = as.numeric(difftime(time1 = sibreport$end, time2 = sibreport$start, units = "mins", tz = tz))
  params_sleep = load_params()$params_sleep
  params_sleep[["possible_nap_dur"]] =  c(0, 240)
  params_sleep[["possible_nap_window"]] =  c(9, 18)
  
  out_add_nap = g.part5.addNaps(sibreport = sibreport, ts = ts,
                                params_general = params_general,
                                params_sleep = params_sleep)
  ts = out_add_nap$ts
  sibreport = out_add_nap$sibreport
  long_nap_boutsi = out_add_nap$long_nap_boutsi
  
  restAnalyses = g.part5.analyseRest(sibreport = sibreport, dsummary = dsummary,
                                     ds_names = ds_names, fi = fi, di = di,
                                     ts = ts, tz = tz,
                                     params_sleep = params_sleep,
                                     long_nap_boutsi = long_nap_boutsi)
  fi = restAnalyses$fi
  di = restAnalyses$di
  dsummary = restAnalyses$dsummary
  ds_names = restAnalyses$ds_names
  dsummary = as.numeric(dsummary[, which(ds_names != "")])
  ds_names = ds_names[ds_names != ""]
  names(dsummary) = ds_names
  dsummary = as.data.frame(t(dsummary))
  
  expect_equal(dsummary$nbouts_day_denap, 1)
  expect_equal(dsummary$nbouts_day_srnonw, 1)
  expect_equal(dsummary$frag_mean_dur_denap_day, 15)
  expect_equal(dsummary$frag_mean_dur_srnonw_day, 20)
  expect_equal(dsummary$perc_denap_overl_srnonw, 100)
  expect_equal(dsummary$perc_srnonw_overl_denap, 75)
  expect_equal(sum(dsummary), 323)
})


test_that("No overlap 1 nonwear, 1 nap, and 1 sib", {
  fi = 1
  di = 1
  sibreport = data.frame(ID = rep("test123", 3), type = c("nonwear", "nap", "sib"),
                         start = c("2022-06-02 12:00:00", "2022-06-02 13:00:00", "2022-06-02 15:00:00"),
                         end = c("2022-06-02 12:20:00", "2022-06-02 13:20:00", "2022-06-02 15:20:00"))
  sibreport$duration = as.numeric(difftime(time1 = sibreport$end, time2 = sibreport$start, units = "mins", tz = tz))
  params_sleep = load_params()$params_sleep
  params_sleep[["possible_nap_dur"]] =  c(0, 240)
  params_sleep[["possible_nap_window"]] =  c(9, 18)
  
  out_add_nap = g.part5.addNaps(sibreport = sibreport, ts = ts,
                                params_general = params_general,
                                params_sleep = params_sleep)
  ts = out_add_nap$ts
  sibreport = out_add_nap$sibreport
  long_nap_boutsi = out_add_nap$long_nap_boutsi
  
  restAnalyses = g.part5.analyseRest(sibreport = sibreport, dsummary = dsummary,
                                     ds_names = ds_names, fi = fi, di = di,
                                     ts = ts, tz = tz,
                                     params_sleep = params_sleep,
                                     long_nap_boutsi = long_nap_boutsi)
  fi = restAnalyses$fi
  di = restAnalyses$di
  dsummary = restAnalyses$dsummary
  expect_equal(sum(as.numeric(restAnalyses$dsummary), na.rm = TRUE), 129)
  
  ds_names = restAnalyses$ds_names
  dsummary = as.numeric(dsummary[, which(ds_names != "")])
  ds_names = ds_names[ds_names != ""]
  names(dsummary) = ds_names
  dsummary = as.data.frame(t(dsummary))
  
  expect_equal(ncol(dsummary), 27)
  expect_equal(nrow(dsummary), 1)
  expect_equal(dsummary$nbouts_day_denap, 1)
  expect_equal(dsummary$nbouts_day_srnap, 1)
  expect_equal(dsummary$nbouts_day_srnonw, 1)
  expect_equal(dsummary$frag_mean_dur_denap_day, 20)
  expect_equal(dsummary$frag_mean_dur_srnap_day, 20)
  expect_equal(dsummary$frag_mean_dur_srnonw_day, 20)
  expect_equal(dsummary$perc_denap_overl_srnap, 0)
  expect_equal(dsummary$perc_denap_overl_srnonw, 0)
  expect_equal(dsummary$perc_srnonw_overl_denap, 0)
  expect_equal(sum(dsummary, na.rm = TRUE), 129)
  
})
