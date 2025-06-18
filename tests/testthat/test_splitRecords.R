library(GGIR)
context("splitRecords")

test_that("Recording can be split", {
  skip_on_cran()
  #==========================================
  # Create dummy file
  data(data.getmeta)
  data(data.calibrate)
  data(data.inspectfile)
  M = data.getmeta
  tz = "Europe/London"
  
  # duplicate data
  M$metashort =  rbind(M$metashort, M$metashort)
  M$metalong =  rbind(M$metalong, M$metalong)
  starttime = as.POSIXct(M$metashort$timestamp[1], tz = tz)
  M$metashort$timestamp = seq(from = starttime, by = 5, length.out = nrow(M$metashort))
  starttime = as.POSIXct(M$metalong$timestamp[1], tz = tz)
  M$metalong$timestamp = seq(from = starttime, by = 900, length.out = nrow(M$metalong))
  
  # change timestamp format to what it has been since 2017
  M$metashort$timestamp = POSIXtime2iso8601(x = M$metashort$timestamp, tz = tz)
  M$metalong$timestamp = POSIXtime2iso8601(x = M$metalong$timestamp, tz = tz)
  colnames(M$metashort)[which(colnames(M$metashort) == "angle")] = "anglez"
  I = data.inspectfile
  I$filename = "1_1-1-1900.bin"
  C = data.calibrate
  
  if (dir.exists("./output_test"))  unlink("./output_test", recursive = TRUE)
  dn = "./output_test/meta/basic"
  if (!dir.exists(dn)) {
    dir.create(dn, recursive = TRUE)
  }
  result_folder = "./output_test/results/QC"
  if (!dir.exists(result_folder)) {
    dir.create(result_folder, recursive = TRUE)
  }
  
  filefoldername = filename_dir = I$filename
  tail_expansion_log = NULL
  save(M, C, I,
       filefoldername = filefoldername,
       filename_dir = filename_dir,
       tail_expansion_log = tail_expansion_log,
       file = paste0(dn, "/meta_1_1-1-1900.bin.RData"))
  
  times_to_split = data.frame(ID = "1", time1 = "2013-11-15 00:05", time2 = "2013-11-18")
  csv_file = "time_chunks_per_participant.csv"
  write.csv(x = times_to_split, file = csv_file, row.names = FALSE)
  
  # Prepare params object
  params_general = load_params()$params_general
  params_general[["desiredtz"]] = tz
  params_general[["idloc"]] = 2
  params_general[["recording_split_times"]] = "time_chunks_per_participant.csv"
  params_general[["recording_split_overlap"]] = 0
  params_general[["recording_split_timeformat"]] = "%Y-%m-%d %H:%M"
  params_general[["recording_split_ignore_edges"]] == FALSE
  
  splitRecords(metadatadir = "./output_test",
               params_general = params_general)
  
  # File created?
  expect_true(dir.exists("./output_test/meta/basic"))
  expect_equal(length(dir("./output_test/meta/basic")), 2)
  expect_true(file.exists("./output_test/meta/basic/meta_1_1-1-1900_split1_startrecTOtime1.bin.RData"))
  expect_true(file.exists("./output_test/meta/basic/meta_1_1-1-1900_split2_time1TOendrec.bin.RData"))

  # File content correct?
  load("./output_test/meta/basic/meta_1_1-1-1900_split1_startrecTOtime1.bin.RData")
  expect_equal(nrow(M$metashort), 8820)
  expect_equal(nrow(M$metalong), 49)
  expect_equal(M$metashort$timestamp[1], "2013-11-14T11:45:00+0000")
  expect_equal(tail(M$metashort$timestamp, n = 1), "2013-11-14T23:59:55+0000")
  expect_equal(M$metalong$timestamp[1],  "2013-11-14T11:45:00+0000")
  expect_equal(tail(M$metalong$timestamp, n = 1), "2013-11-14T23:45:00+0000")
  load("./output_test/meta/basic/meta_1_1-1-1900_split2_time1TOendrec.bin.RData")
  expect_equal(nrow(M$metashort), 28619)
  expect_equal(nrow(M$metalong), 158)
  expect_equal(M$metashort$timestamp[1], "2013-11-15T00:00:00+0000")
  expect_equal(tail(M$metashort$timestamp, n = 1), "2013-11-16T15:44:50+0000")
  expect_equal(M$metalong$timestamp[1], "2013-11-15T00:00:00+0000")
  expect_equal(tail(M$metalong$timestamp, n = 1), "2013-11-16T15:15:00+0000")
  
  # Test in context of GGIR as a whole:
  GGIR(datadir = "D:/test", outputdir = ".", desiredtz = tz,
       mode = 2:5, do.report = c(2, 4, 5), verbose = FALSE, idloc = 2,
       recording_split_times = "time_chunks_per_participant.csv",
       visualreport = FALSE, recording_split_overlap = 0,
       recording_split_timeformat = "%Y-%m-%d %H:%M")
  
  # We expect split names to be logged in part 2 csv reports
  P2 = read.csv("./output_test/results/part2_summary.csv")
  expect_equal(P2$split1_name, c("startrec", "time1"))
  expect_equal(P2$split2_name, c("time1", "endrec"))
  P2day = read.csv("./output_test/results/part2_daysummary.csv")
  expect_equal(P2day$split1_name, c("startrec", "time1", "time1"))
  expect_equal(P2day$split2_name, c("time1", "endrec", "endrec"))
  QC = read.csv("./output_test/results/QC/data_quality_report.csv")
  expect_equal(QC$split1_name, c("startrec", "time1"))
  expect_equal(QC$split2_name, c("time1", "endrec"))
  # We expect split names to be logged in part 4 csv reports
  P4night = read.csv("./output_test/results/part4_nightsummary_sleep_cleaned.csv")
  expect_equal(P4night$split1_name, "time1")
  expect_equal(P4night$split2_name, "endrec")
  P4 = read.csv("./output_test/results/part4_summary_sleep_cleaned.csv")
  expect_equal(P4$split1_name, "time1")
  expect_equal(P4$split2_name, "endrec")
  # We expect split names to be logged in part 5 csv reports
  P5day = read.csv("./output_test/results/part5_daysummary_MM_L40M100V400_T5A5.csv")
  expect_equal(P5day$split1_name, "time1")
  expect_equal(P5day$split2_name, "endrec")
  P5 = read.csv("./output_test/results/part5_personsummary_MM_L40M100V400_T5A5.csv")
  expect_equal(P5$split1_name, "time1")
  expect_equal(P5$split2_name, "endrec")
  
  if (dir.exists("./output_test"))  unlink("./output_test", recursive = TRUE)
  if (file.exists(csv_file)) unlink(csv_file, recursive = TRUE)
})


test_that("Recording can be split and ignore edges", {
  skip_on_cran()
  
  #==========================================
  # Create dummy file
  data(data.getmeta)
  data(data.calibrate)
  data(data.inspectfile)
  M = data.getmeta
  
  # change timestamp format to what it has been since 2017
  M$metashort$timestamp = POSIXtime2iso8601(x = M$metashort$timestamp, tz = "Europe/London")
  M$metalong$timestamp = POSIXtime2iso8601(x = M$metalong$timestamp, tz = "Europe/London")
  I = data.inspectfile
  I$filename = "1_1-1-1900.bin"
  C = data.calibrate
  
  dn = "./testfolder/meta/basic"
  if (!dir.exists(dn)) {
    dir.create(dn, recursive = TRUE)
  }
  filefoldername = filename_dir = I$filename
  tail_expansion_log = NULL
  save(M, C, I,
       filefoldername = filefoldername,
       filename_dir = filename_dir,
       tail_expansion_log = tail_expansion_log,
       file = paste0(dn, "/meta_1_1-1-1900.bin.RData"))
  
  times_to_split = data.frame(ID = "1", time1 = "2013-11-15 00:05", time2 = "2013-11-16")
  csv_file = "time_chunks_per_participant.csv"
  write.csv(x = times_to_split, file = csv_file, row.names = FALSE)
  
  # Prepare params object
  params_general = load_params()$params_general
  params_general[["desiredtz"]] = "Europe/Amsterdam"
  params_general[["idloc"]] = 2
  params_general[["recording_split_times"]] = "time_chunks_per_participant.csv"
  params_general[["recording_split_overlap"]] = 0
  params_general[["recording_split_timeformat"]] = "%Y-%m-%d %H:%M"
  params_general[["recording_split_ignore_edges"]] = TRUE
  
  splitRecords(metadatadir = "./testfolder",
               params_general = params_general)
  
  # File created?
  expect_true(dir.exists("./testfolder/meta/basic"))
  expect_equal(length(dir("./testfolder/meta/basic")), 1)
  expect_true(file.exists("./testfolder/meta/basic/meta_1_1-1-1900.bin.RData"))
  
  # File content correct?
  load("./testfolder/meta/basic/meta_1_1-1-1900.bin.RData")
  expect_equal(nrow(M$metashort), 18720)
  expect_equal(nrow(M$metalong), 104)
  expect_equal(M$metashort$timestamp[1], "2013-11-14T11:45:00+0000")
  expect_equal(tail(M$metashort$timestamp, n = 1), "2013-11-15T13:44:55+0000")
  expect_equal(M$metalong$timestamp[1],  "2013-11-14T11:45:00+0000")
  expect_equal(tail(M$metalong$timestamp, n = 1), "2013-11-15T13:30:00+0000")
  
  if (dir.exists("./testfolder"))  unlink("./testfolder", recursive = TRUE)
  if (file.exists(csv_file)) unlink(csv_file, recursive = TRUE)
})
