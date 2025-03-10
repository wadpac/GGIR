library(GGIR)
context("splitRecords")

test_that("Recording can be split", {
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
  
  splitRecords(metadatadir = "./testfolder",
               params_general = params_general)
  
  # File created?
  expect_true(dir.exists("./testfolder/meta/basic"))
  expect_equal(length(dir("./testfolder/meta/basic")), 2)
  expect_true(file.exists("./testfolder/meta/basic/meta_1_1-1-1900_split1_startTOtime1.bin.RData"))
  expect_true(file.exists("./testfolder/meta/basic/meta_1_1-1-1900_split2_time1TOend.bin.RData"))

  # File content correct?
  load("./testfolder/meta/basic/meta_1_1-1-1900_split1_startTOtime1.bin.RData")
  expect_equal(nrow(M$metashort), 8100)
  expect_equal(nrow(M$metalong), 45)
  expect_equal(M$metashort$timestamp[1], "2013-11-14T11:45:00+0000")
  expect_equal(tail(M$metashort$timestamp, n = 1), "2013-11-14T22:59:55+0000")
  expect_equal(M$metalong$timestamp[1],  "2013-11-14T11:45:00+0000")
  expect_equal(tail(M$metalong$timestamp, n = 1), "2013-11-14T22:45:00+0000")
  load("./testfolder/meta/basic/meta_1_1-1-1900_split2_time1TOend.bin.RData")
  expect_equal(nrow(M$metashort), 10619)
  expect_equal(nrow(M$metalong), 58)
  expect_equal(M$metashort$timestamp[1], "2013-11-14T23:00:00+0000")
  expect_equal(tail(M$metashort$timestamp, n = 1), "2013-11-15T13:44:50+0000")
  expect_equal(M$metalong$timestamp[1], "2013-11-14T23:00:00+0000")
  expect_equal(tail(M$metalong$timestamp, n = 1), "2013-11-15T13:15:00+0000")
  
  if (dir.exists("./testfolder"))  unlink("./testfolder", recursive = TRUE)
  if (file.exists(csv_file)) unlink(csv_file, recursive = TRUE)
})
