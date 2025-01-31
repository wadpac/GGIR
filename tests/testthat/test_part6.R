library(GGIR)
context("g.part6")

test_that("Part 6 can run co-analysis and CR analysis", {

  # Create test files for household co-analysis
  metadatadir = "./output_testpart6"
  part6_threshold_combi = "40_120_400"
  dn = paste0(metadatadir, "/meta/ms5.outraw/", part6_threshold_combi)
  if (!dir.exists(dn)) dir.create(path = dn, recursive = TRUE)
  dn2 = paste0(metadatadir, "/meta/basic")
  if (!dir.exists(dn2)) dir.create(path = dn2, recursive = TRUE)
  data(data.metalong)
  M = data.metalong
  save(M, file = paste0(dn2, "/meta_800-900-001_left wrist.bin.RData"))
  save(M, file = paste0(dn2, "/meta_800-900-002_left wrist.bin.RData"))
  save(M, file = paste0(dn2, "/meta_800-900-003_left wrist.bin.RData"))
  
  
  # Update parameters to align with datset
  params_general = load_params(topic = "general")$params_general
  params_general[["desiredtz"]] = "America/Curacao"
  params_phyact = load_params(topic = "phyact")$params_phyact
  params_phyact[["part6_threshold_combi"]] = part6_threshold_combi
  thresholds = as.numeric(unlist(strsplit(part6_threshold_combi, "_")))
  params_phyact[["threshold.lig"]] = thresholds[1]
  params_phyact[["threshold.mod"]] = thresholds[2] 
  params_phyact[["threshold.vig"]] = thresholds[3]
  params_247 = load_params(topic = "247")$params_247
  params_247[["part6HCA"]] = TRUE
  
  # Use time shifts to simulate three household members
  data(data.ts)
  mdat = data.ts
  
  Lnames = c("spt_sleep", "spt_wake_IN", "spt_wake_LIG", "spt_wake_MOD",
             "spt_wake_VIG", "day_IN_unbt", "day_LIG_unbt", "day_MOD_unbt",
             "day_VIG_unbt", "day_MVPA_bts_10", "day_IN_bts_30",
             "day_IN_bts_10_30", "day_LIG_bts_10")
  
  mdat$timenum = mdat$timenum - (5 * 60) 
  mdat$timestamp = as.POSIXct(mdat$timenum, tz =  params_general[["desiredtz"]], origin = "1970-01-01")
  filename = "800-900-001_left wrist.bin"
  save(mdat, filename, Lnames, file = paste0(dn, "/800-900-001_left wrist.RData"))
  mdat$timenum = mdat$timenum + (7 * 60)
  mdat$timestamp = as.POSIXct(mdat$timenum, tz =  params_general[["desiredtz"]], origin = "1970-01-01")
  filename = "800-900-002_left wrist.bin"
  save(mdat, filename, Lnames, file = paste0(dn, "/800-900-002_left wrist.RData"))
  mdat$timenum = mdat$timenum + (14 * 60)
  mdat$timestamp = as.POSIXct(mdat$timenum, tz =  params_general[["desiredtz"]], origin = "1970-01-01")
  filename = "800-900-003_left wrist.bin"
  save(mdat, filename, Lnames, file = paste0(dn, "/800-900-003_left wrist.RData"))
  mdat_file3 = mdat
  
  # Run household co-analysis  
  g.part6(metadatadir = metadatadir,
          params_general = params_general,
          params_phyact = params_phyact,
          params_247 = params_247, verbose = FALSE)
  
  # Check aligned time series output file
  path_to_ts = paste0(metadatadir, "/results/part6HouseholdCoAnalysis/alignedTimeseries/timeseries_HID_900.RData")
  expect_true(file.exists(path_to_ts))
  load(path_to_ts)
  TS = alignedTimeseries
  expect_equal(nrow(TS), 10032)
  expect_equal(ncol(TS), 43)
  expect_equal(sum(TS$ACC.001[1000:1200]), 840.91)
  expect_equal(sum(TS$lightmean.001[1000:1200]), 1.166)
  expect_equal(sum(TS$ACC.002[2000:2300]), 7731.019)
  expect_equal(sum(TS$lightmean.002[2000:2300]), 2223.294, tolerance = 0.002)
  
  # Check pairwise comparisons
  path_to_pairwisecomp = paste0(metadatadir, "/results/part6HouseholdCoAnalysis/pairwise_summary_all_housholds.csv")
  expect_true(file.exists(path_to_pairwisecomp))
  PC = read.csv(path_to_pairwisecomp)
  expect_equal(nrow(PC), 18)
  expect_equal(ncol(PC), 41)
  expect_equal(PC$Npairs, rep(3, 18))
  expect_equal(sum(PC$wakeup_acc_1_before_2_mg), 171.863)
  expect_equal(PC$wakeup_time1[c(1, 8, 16)], c("01:35:00", "02:58:00", "03:39:00"))
  expect_equal(sum(PC$day_Kappa_active), 5.091)
  
  #===================================================================
  # For circadian rhythm analysis we set one window to invalid
  mdat_file3$invalid_wakinghours[which(mdat_file3$window == 4)] = 100 # set one window to invalid
  mdat_file3$invalid_sleepperiod[which(mdat_file3$window == 4)] = 100 # set one window to invalid
  mdat_file3$invalid_fullwindow[which(mdat_file3$window == 4)] = 100 # set one window to invalid
  mdat_file3$ACC[which(mdat$window == 3)] = NA # set one window to invalid
  mdat = mdat_file3
  filename = "800-900-003_left wrist.bin"
  save(mdat, filename, Lnames, file = paste0(dn, "/800-900-003_left wrist.RData"))
  
  # Run Circadian rhythm analysis with default window
  params_247[["part6HCA"]] = FALSE
  params_general[["do.parallel"]] = FALSE
  params_general[["overwrite"]] = TRUE
  params_247[["cosinor"]] = TRUE
  params_247[["part6CR"]] = TRUE
  params_247[["part6Window"]] = c("start", "end")
  params_cleaning = load_params(topic = "cleaning")$params_cleaning
  g.part6(metadatadir = metadatadir,
          params_general = params_general,
          params_phyact = params_phyact,
          params_247 = params_247,
          params_cleaning = params_cleaning,
          f0 = 1, f1 = 3,
          verbose = FALSE)
  path_to_ms6 = paste0(metadatadir, "/meta/ms6.out/800-900-003_left wrist.RData")
  expect_true(file.exists(path_to_ms6))
  
  load(path_to_ms6)
  expect_equal(ncol(output_part6), 48)
  expect_equal(output_part6$starttime, "2022-06-02 03:16:00")
  expect_equal(output_part6$cosinor_mes, 2.471082, tolerance = 0.00001)
  expect_equal(output_part6$cosinorExt_minimum, 1.302779, tolerance = 0.00001)
  expect_equal(output_part6$cosinorExt_MESOR, 2.171, tolerance = 0.00001)
  expect_equal(sum(output_part6[5:27]), 620.6144, tolerance = 0.0001)
  
  # Run Circadian rhythm analysis with non-default window
  params_247[["part6Window"]] = c("W2", "W-1") # second wake till last wake
  g.part6(metadatadir = metadatadir,
          params_general = params_general,
          params_phyact = params_phyact,
          params_247 = params_247,
          params_cleaning = params_cleaning,
          f0 = 1, f1 = 3,
          verbose = FALSE)
  path_to_ms6 = paste0(metadatadir, "/meta/ms6.out/800-900-003_left wrist.RData")
  
  expect_true(file.exists(path_to_ms6))
  
  load(path_to_ms6)
  expect_equal(ncol(output_part6), 48)
  expect_equal(output_part6$starttime, "2022-06-03 01:57:00")
  expect_equal(output_part6$cosinor_mes, 2.431978, tolerance = 0.00001)
  expect_equal(output_part6$cosinorExt_minimum, 1.286542, tolerance = 0.00001)
  expect_equal(output_part6$cosinorExt_MESOR, 2.140128, tolerance = 0.00001)
  expect_equal(sum(output_part6[5:27]), 680.9232, tolerance = 0.0001)
  
  output_part6_with_invalid = output_part6
  rm(output_part6)
  
  # Assess impact of storing part 5 output without invalid
  mdat = mdat_file3[-which(mdat$window == 4),] # delete the invalid window
  filename = "800-900-003_left wrist.bin"
  save(mdat, filename, Lnames, file = paste0(dn, "/800-900-003_left wrist.RData"))
  
  g.part6(metadatadir = metadatadir,
          params_general = params_general,
          params_phyact = params_phyact,
          params_247 = params_247,
          params_cleaning = params_cleaning,
          f0 = 1, f1 = 3,
          verbose = FALSE)
  path_to_ms6 = paste0(metadatadir, "/meta/ms6.out/800-900-003_left wrist.RData")
  
  expect_true(file.exists(path_to_ms6))
  
  load(path_to_ms6)
  output_part6_without_invalid = output_part6
  
  # Compare output_part6_with_invalid and output_part6_without_invalid
  expect_equal(ncol(output_part6_with_invalid), ncol(output_part6_without_invalid))
  expect_equal(output_part6_with_invalid$starttime, output_part6_without_invalid$starttime)
  expect_equal(output_part6_with_invalid$cosinor_mes, output_part6_without_invalid$cosinor_mes, tolerance = 0.00001)
  expect_equal(output_part6_with_invalid$cosinorExt_minimum, output_part6_without_invalid$cosinorExt_minimum, tolerance = 0.00001)
  expect_equal(output_part6_with_invalid$cosinorExt_MESOR, output_part6_without_invalid$cosinorExt_MESOR, tolerance = 0.00001)
  expect_equal(sum(output_part6_with_invalid[5:27]), sum(output_part6_without_invalid[5:27]), tolerance = 0.0001)

  # Remove test files
  if (file.exists(metadatadir))  unlink(metadatadir, recursive = TRUE)
}
)
