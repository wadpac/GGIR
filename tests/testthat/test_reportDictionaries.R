library(GGIR)
context("report dictionaries")
test_that("Data dictionaries are generated for the part 2, 4, 5, and 6 reports", {
  skip_on_cran()
  metadatadir = file.path(tempdir(), "output_dictionaries")
  if (dir.exists(metadatadir)) unlink(metadatadir, recursive = TRUE)
  dir.create(file.path(metadatadir, "results", "QC"), recursive = TRUE)
  params_output = list(sep_reports = ",", dec_reports = ".")
  write_report = function(cnames, fn) {
    x = as.data.frame(matrix(1, 2, length(cnames)))
    colnames(x) = cnames
    data.table::fwrite(x, file = file.path(metadatadir, "results", fn))
  }
  read_dictionary = function(fn) {
    data.table::fread(file.path(metadatadir, "results", "variableDictionary", fn),
                      data.table = FALSE)
  }
  #---------------------------------------
  # part 2
  write_report(c("ID", "filename", "ENMO_fullRecordingMean", "AD_L5hr_ENMO_mg_0-24hr",
                 "WE_M5_ENMO_mg_0-24hr", "AD_[0,50)_ENMO_mg_0-24hr", "AD_p90_ENMO_mg_0-24hr",
                 "WWD_MVPA_E5S_B10M80%_T100_ENMO_0-24hr", "AD_ig_gradient_ENMO_0-24hr",
                 "cosinor_amp", "IS"), "part2_summary.csv")
  write_report(c("ID", "calendar_date", "N valid hours", "mean_ENMO_mg_0-12hr",
                 "MVPA_E1M_T100_ENMO_0-24hr"), "part2_daysummary.csv")
  write_report(c("ID", "AD_tot_step_count_0-24hr", "AD_mn_cad_0-24hr",
                 "AD_L5_cad_meancad_0-24hr", "AD_number_B1M80%_cad30ANDacc50_ENMO_0-24hr"),
               "part2_eventsummary.csv")
  write_report(c("filename", "scale.x", "cal.error.end"), "QC/data_quality_report.csv")
  write_report(c("ID", "AD_tot_activity_type_sitting_0-24hr", "AD_tot_activity_type_unclassified_0-24hr",
                 "AD_tot_activity_type_invalid_0-24hr", "AD_totdur_B10M80%_sitting_0-24hr",
                 "AD_tot_activity_type_sitting_acc0-50mg_ENMO_0-24hr"), "part2_typesummary.csv")
  g.report.part2_dictionary(metadatadir, params_output)
  D = read_dictionary("part2_dictionary_summary.csv")
  expect_equal(nrow(D), 11)
  expect_true(all(!is.na(D$Definition) & D$Definition != ""))
  expect_equal(D$Definition[D$Variable == "AD_[0,50)_ENMO_mg_0-24hr"],
               paste0("Time accumulated with ENMO from 0 (included) to 50 (excluded) mg",
                      " (minutes) over the full day (0-24h) - average of all valid days"))
  D = read_dictionary("part2_dictionary_daysummary.csv")
  expect_true(all(!is.na(D$Definition) & D$Definition != ""))
  expect_true(grepl("from 0h to 12h", D$Definition[D$Variable == "mean_ENMO_mg_0-12hr"]))
  D = read_dictionary("part2_dictionary_eventsummary.csv")
  expect_true(all(!is.na(D$Definition) & D$Definition != ""))
  expect_true(grepl("cadence", D$Definition[D$Variable == "AD_L5_cad_meancad_0-24hr"]))
  expect_true(file.exists(file.path(metadatadir, "results", "variableDictionary",
                                    "part2_dictionary_data_quality_report.csv")))
  D = read_dictionary("part2_dictionary_typesummary.csv")
  expect_true(all(!is.na(D$Definition) & D$Definition != ""))
  expect_true(grepl("'sitting' by external function output activity_type over the full day",
                    D$Definition[D$Variable == "AD_tot_activity_type_sitting_0-24hr"]))
  expect_true(grepl("ENMO from 0 \\(included\\) to 50",
                    D$Definition[D$Variable == "AD_tot_activity_type_sitting_acc0-50mg_ENMO_0-24hr"]))
  #---------------------------------------
  # part 4
  write_report(c("ID", "night", "sleeponset", "wakeup", "SptDuration", "guider", "cleaningcode"),
               "part4_nightsummary_sleep_cleaned.csv")
  write_report(c("ID", "n_nights_acc", "SptDuration_AD_T5A5_mn", "SptDuration_WE_T5A5_sd",
                 "average_dur_sib_wakinghours_WD_T5A5_mn", "n_days_w_sib_wakinghours_AD_T5A5",
                 "guider_SptDuration_AD_mn"),
               "QC/part4_summary_sleep_full.csv") # only full report available
  g.report.part4_dictionary(metadatadir, params_output)
  D = read_dictionary("part4_dictionary_nightsummary.csv")
  expect_equal(nrow(D), 7)
  expect_true(all(!is.na(D$Definition) & D$Definition != ""))
  D = read_dictionary("part4_dictionary_summary.csv")
  expect_true(all(!is.na(D$Definition) & D$Definition != ""))
  expect_true(grepl("standard deviation across weekend nights",
                    D$Definition[D$Variable == "SptDuration_WE_T5A5_sd"]))
  #---------------------------------------
  # part 5: variables that were not covered before
  write_report(c("ID", "filename", "nap_count", "nbouts_day_denap", "perc_srnap_overl_denap",
                 "Nbouts_day_MVPA_bts_10", "dur_day_nap_min", "GGIRversion"),
               "part5_daysummary_MM_L40M100V400_T5A5.csv")
  write_report(c("ID", "filename", "nap_count_pla", "Nmarkerbutton_used", "boutcriter.in"),
               "part5_personsummary_MM_L40M100V400_T5A5.csv")
  write_report(c("ID", "filename", "activity_type_dur_spt_sitting_unbt_min",
                 "activity_type_Nbouts_day_walking_bts_5_10", "activity_type_ACC_day_total_walking_mg"),
               "part5_extfuntype_activity_type_daysummary_MM_T5A5.csv")
  g.report.part5_dictionary(metadatadir, params_output)
  D = read_dictionary("part5_dictionary_daysummary.csv")
  expect_true(all(!is.na(D$Definition) & D$Definition != ""))
  expect_true(grepl("at least 10 min", D$Definition[D$Variable == "Nbouts_day_MVPA_bts_10"]))
  expect_true(grepl("naps", D$Definition[D$Variable == "dur_day_nap_min"]))
  D = read_dictionary("part5_dictionary_personsummary.csv")
  expect_true(all(!is.na(D$Definition) & D$Definition != ""))
  expect_true(grepl("plain average$", D$Definition[D$Variable == "nap_count_pla"]))
  expect_false(grepl("average", D$Definition[D$Variable == "boutcriter.in"]))
  D = read_dictionary("part5_dictionary_extfuntype_activity_type_daysummary.csv")
  expect_true(all(!is.na(D$Definition) & D$Definition != ""))
  expect_true(grepl("^Number of bouts of 5 to 10 min of time classified as 'walking'",
                    D$Definition[D$Variable == "activity_type_Nbouts_day_walking_bts_5_10"]))
  #---------------------------------------
  # part 6
  write_report(c("ID", "N_valid_days", "cosinor_mes", "IS", "SSP", "L5VALUE", "M5TIME_clock",
                 "FRAG_TP_IN2PA_day", "FRAG_Nfrag_sleep_spt", "SleepRegularityIndex2"),
               "part6_summary.csv")
  g.report.part6_dictionary(metadatadir, params_output)
  D = read_dictionary("part6_dictionary_summary.csv")
  expect_equal(nrow(D), 10)
  expect_true(all(!is.na(D$Definition) & D$Definition != ""))
  if (dir.exists(metadatadir)) unlink(metadatadir, recursive = TRUE)
})
