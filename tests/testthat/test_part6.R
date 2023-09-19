library(GGIR)
context("part6")

test_that("Part 6 with household co-analysis", {
  
  # Create test files for household co-analysis
  metadatadir = "./output_testpart6"
  part6_threshold_combi = "40_120_400"
  dn = paste0(metadatadir, "/meta/ms5.outraw/", part6_threshold_combi)
  if (!dir.exists(dn)) dir.create(path = dn, recursive = TRUE)
  dn2 = paste0(metadatadir, "/meta/basic")
  if (!dir.exists(dn2)) dir.create(path = dn2, recursive = TRUE)
  F2 = system.file("testfiles/part6_metalong.RData", package = "GGIR")[1]
  load(F2)
  save(M, file = paste0(dn2, "/meta_800-900-001_left wrist.bin.RData"))
  save(M, file = paste0(dn2, "/meta_800-900-002_left wrist.bin.RData"))
  save(M, file = paste0(dn2, "/meta_800-900-003_left wrist.bin.RData"))
  
  F1 = system.file("testfiles/part6_ts.RData", package = "GGIR")[1]
  load(F1)
  
  # Use time shifts to simulate three household members
  mdat$timenum = mdat$timenum - (5 * 60) 
  save(mdat, file = paste0(dn, "/800-900-001_left wrist.RData"))
  mdat$timenum = mdat$timenum + (7 * 60)
  save(mdat, file = paste0(dn, "/800-900-002_left wrist.RData"))
  mdat$timenum = mdat$timenum + (14 * 60)
  save(mdat, file = paste0(dn, "/800-900-003_left wrist.RData"))
  
  # Run household co-analysis  
  # Update parameters to align with datset
  params_general = load_params()$params_general
  params_general[["desiredtz"]] = "America/Curacao"
  params_phyact = load_params()$params_phyact
  params_phyact[["part6_threshold_combi"]] = part6_threshold_combi
  g.part6(metadatadir = metadatadir,
          params_general = params_general,
          params_phyact = params_phyact, verbose = FALSE)
  
  # Check aligned time series output file
  path_to_ts = paste0(metadatadir, "/results/household_co_analysis/household_timeseries/timeseries_HID_900.csv")
  expect_true(file.exists(path_to_ts))
  TS = read.csv(path_to_ts)
  expect_equal(nrow(TS), 10032)
  expect_equal(ncol(TS), 43)
  expect_equal(sum(TS$ACC.001[1000:1200]), 840.91)
  expect_equal(sum(TS$lightmean.001[1000:1200]), 1.166)
  expect_equal(sum(TS$ACC.002[2000:2300]), 7731.019)
  expect_equal(sum(TS$lightmean.002[2000:2300]), 2223.294, tolerance = 0.002)
  
  # Check pairwise comparisons
  path_to_pairwisecomp = paste0(metadatadir, "/results/household_co_analysis/summary_pairs/pairwise_summary_all_housholds.csv")
  expect_true(file.exists(path_to_pairwisecomp))
  PC = read.csv(path_to_pairwisecomp)
  expect_equal(nrow(PC), 18)
  expect_equal(ncol(PC), 41)
  expect_equal(PC$Npairs, rep(3, 18))
  expect_equal(sum(PC$wakeup_acc_1_before_2), 171.863)
  expect_equal(PC$wakeup_time1[c(1, 8, 16)], c("01:35:00", "02:58:00", "03:39:00"))
  expect_equal(sum(PC$day_Kappa_active), 5.091)
  
  # Remove test files
  if (file.exists(metadatadir))  unlink(metadatadir, recursive = TRUE)
})