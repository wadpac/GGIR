library(GGIR)
context("appendRecords")
#==========================================
# Create dummy data: 4 files from which the first 3 can be appended
timestamps = matrix("", 10, 4)
# Timstamps for file 1
timestamps[, 1] = c("2020-05-13T14:00:00+0200", "2020-05-13T14:01:00+0200",
                    "2020-05-13T14:02:00+0200", "2020-05-13T14:03:00+0200",
                    "2020-05-13T14:04:00+0200", "2020-05-13T14:05:00+0200",
                    "2020-05-13T14:06:00+0200", "2020-05-13T14:07:00+0200",
                    "2020-05-13T14:08:00+0200", "2020-05-13T14:09:00+0200")
# Timstamps for file 2: overlapping time
timestamps[, 2] = c("2020-05-13T14:05:00+0200", "2020-05-13T14:06:00+0200",
                    "2020-05-13T14:07:00+0200", "2020-05-13T14:08:00+0200",
                    "2020-05-13T14:09:00+0200", "2020-05-13T14:10:00+0200",
                    "2020-05-13T14:11:00+0200", "2020-05-13T14:12:00+0200",
                    "2020-05-13T14:13:00+0200", "2020-05-13T14:14:00+0200")
# Timstamps for file 3: non-overlapping time but small gap
timestamps[, 3] = c("2020-05-13T14:30:00+0200", "2020-05-13T14:31:00+0200",
                    "2020-05-13T14:32:00+0200", "2020-05-13T14:33:00+0200",
                    "2020-05-13T14:34:00+0200", "2020-05-13T14:35:00+0200",
                    "2020-05-13T14:36:00+0200", "2020-05-13T14:37:00+0200",
                    "2020-05-13T14:38:00+0200", "2020-05-13T14:39:00+0200")
# Timstamps for file 4: non-overlapping time but long gap
timestamps[, 4] = c("2020-05-25T14:30:00+0200", "2020-05-25T14:31:00+0200",
                    "2020-05-25T14:32:00+0200", "2020-05-25T14:33:00+0200",
                    "2020-05-25T14:34:00+0200", "2020-05-25T14:35:00+0200",
                    "2020-05-25T14:36:00+0200", "2020-05-25T14:37:00+0200",
                    "2020-05-25T14:38:00+0200", "2020-05-25T14:39:00+0200")
# Calibration, same for all files:
C = list(cal.error.end = 0, cal.error.start = 0)
C$scale = c(1, 1, 1)
C$tempoffset = C$offset = c(0, 0, 0)
C$QCmessage = "Autocalibration not done"
C$npoints = C$nhoursused = 0
C$use.temp = TRUE
# Template M object
M1 = M2 = M3 = M4 = list(filecorrupt = FALSE, filetooshort = FALSE,
                         metalong = data.frame(
                           timestamp = rep("", 2), 
                           nonwearscore = rep(0, 2),
                           clippingscore = rep(0, 2),
                           lightmean = rep(0, 2),
                           lightpeak = rep(0, 2),
                           temperaturemean = rep(0, 2),
                           EN = rep(0, 2)
                         ),
                         metashort = data.frame(timestamp = rep("", 10),
                                                accmetric = rep(0, 10)), 
                         wday = 0,
                         wdayname = "Sunday",
                         windowsizes = c(60, 300, 300),
                         bsc_qc = data.frame(A = 1:3,
                                             B = 1:3))
# File 1, use value 1 as a way to mark the time series
M1$metashort$timestamp = timestamps[, 1]
M1$metashort$accmetric = 1
M1$metalong$timestamp = timestamps[c(1, 6), 1]
M1$metalong$lightmean = 1
# File 2, use value 2 as a way to mark the time series
M2$metashort$timestamp = timestamps[, 2]
M2$metashort$accmetric = 2
M2$metalong$timestamp = timestamps[c(1, 6), 2]
M2$metalong$lightmean = 2
# File 3, use value 3 as a way to mark the time series
M3$metashort$timestamp = timestamps[, 3]
M3$metashort$accmetric = 3
M3$metalong$timestamp = timestamps[c(1, 6), 3]
M3$metalong$lightmean = 3
# File 4, use value 4 as a way to mark the time series
M4$metashort$timestamp = timestamps[, 4]
M4$metashort$accmetric = 4
M4$metalong$timestamp = timestamps[c(1, 6), 4]
M4$metalong$lightmean = 4

dummyheader = data.frame(uniqueSerialCode = 0, 
                         frequency = 100,
                         Subject_Code = 1,
                         start = "1980-01-01 19:08:00",
                         device = "name",
                         firmwareVersion = "unknown",
                         block = 0)
dummyheader = t(dummyheader)
colnames(dummyheader) = "value"
I1 = I2 = I3 = I4 = list(
  header = dummyheader,
  monc = 2,
  monn = "geneactive",
  dformc = 1,
  dformn = "bin",
  sf = 100,
  filename = "",
  deviceSerialNumber = "12345")
# Three file names with same ID
I1$filename = "1_T1test.bin"
I2$filename = "1_T2test.bin"
I3$filename = "1_T3test.bin"
I4$filename = "1_T4test.bin"

tail_expansion_log = NULL
filefoldername = filename_dir = NA
# Save objects to RData files to imitate a real study situation
dn = "./testfolder/meta/basic"
if (!dir.exists(dn)) {
  dir.create(dn, recursive = TRUE)
}
I = I1
M = M1
save(I, M, C, filefoldername, filename_dir, tail_expansion_log, file = paste0(dn, "/meta_1_bin.RData"))
I = I2
M = M2
save(I, M, C, filefoldername, filename_dir, tail_expansion_log, file = paste0(dn, "/meta_2_bin.RData"))
I = I3
M = M3
save(I, M, C, filefoldername, filename_dir, tail_expansion_log, file = paste0(dn, "/meta_3_bin.RData"))
I = I4
M = M4
save(I, M, C, filefoldername, filename_dir, tail_expansion_log, file = paste0(dn, "/meta_4_bin.RData"))

test_that("Neighbouring recordings are correctly appended", {
  
  appendRecords(metadatadir = "./testfolder",
                desiredtz = "Europe/Amsterdam",
                idloc = 2,
                maxRecordingInterval = 120) # extract ID from filename
  
  expect_true(dir.exists("./testfolder/meta/basic"))
  expect_equal(length(dir("./testfolder/meta/basic")), 2)
  expect_true(file.exists("./testfolder/meta/basic/meta_1_bin.RData"))
  expect_true(file.exists("./testfolder/meta/basic/meta_4_bin.RData"))
  
  # File one, should now be merged
  load("./testfolder/meta/basic/meta_1_bin.RData")
  expect_equal(nrow(M$metashort), 40)
  expect_equal(sum(M$metashort$accmetric), 55)
  expect_equal(nrow(M$metalong), 8)
  expect_equal(sum(M$metalong$lightmean), 11)
  expect_equal(length(which(duplicated(M$metalong$timestamp) == TRUE)), 0)
  expect_equal(length(which(duplicated(M$metashort$timestamp) == TRUE)), 0)
  
  expect_equal(length(I_list), 3)
  expect_equal(I_list[[1]]$interval, c(-0.267, 0.067))
  expect_equal(I_list[[2]]$interval, -0.267)
  expect_false("interval" %in% names(I_list[[3]]))

  expect_equal(M$metalong$timestamp, c("2020-05-13T14:00:00+0200",
                                       "2020-05-13T14:05:00+0200",
                                       "2020-05-13T14:10:00+0200",
                                       "2020-05-13T14:15:00+0200",
                                       "2020-05-13T14:20:00+0200",
                                       "2020-05-13T14:25:00+0200",
                                       "2020-05-13T14:30:00+0200",
                                       "2020-05-13T14:35:00+0200"))
  
  expect_equal(M$metashort$timestamp[c(1, 12, 24, 36)], c( "2020-05-13T14:00:00+0200",
                                                           "2020-05-13T14:11:00+0200",
                                                           "2020-05-13T14:23:00+0200",
                                                           "2020-05-13T14:35:00+0200"))

  expect_equal(unique(M$metashort$accmetric), c(1, 2, 0, 3))
  
  # File four, should be unmerged
  rm(I_list)
  load("./testfolder/meta/basic/meta_4_bin.RData")
  expect_false(exists("I_list"))
  expect_equal(nrow(M$metashort), 10)
  expect_equal(nrow(M$metalong), 2)
  expect_equal(unique(M$metashort$accmetric), 4)
  if (file.exists(dn))  unlink(dn, recursive = TRUE)
})
