library(GGIR)
context("convertEpochData")
test_that("External epoch data is correctly converted", {
  skip_on_cran()
  params_general = load_params()$params_general
  params_general[["overwrite"]] = TRUE
  params_general[["extEpochData_timeformat"]] = "%d/%m/%Y %H:%M:%S"
  # Set smaller than usual windowsizes, because this test recordings are clipped short
  params_general[["windowsizes"]][2] = 60
  params_general[["windowsizes"]][3] = 120
  
  # Function to create a new directory (dn) and copies the test file there
  dn = "./tmp_testdata"
  move2folder = function(fn, dn) {
    if (!dir.exists(dn)) {
      dir.create(dn)
    }
    file.copy(from = fn, to = dn)
  }
  # Create output directory
  QCout = "./output_tmp_testdata/results/QC"
  QCbasis = "./output_tmp_testdata/meta/basic"
  if (!dir.exists(QCout)) dir.create(QCout, recursive = TRUE)
  if (!dir.exists(QCbasis)) dir.create(QCbasis, recursive = TRUE)
  outputdir = "./output_tmp_testdata"
  
  # Tidy up by deleting output folder
  if (file.exists(outputdir)) unlink(outputdir, recursive = TRUE)
  
  # AWD
  cat("\nActiwatch AWD")
  move2folder(system.file("testfiles/Actiwatch.AWD", package = "GGIR")[1], dn)
  params_general[["windowsizes"]][1] = 60
  params_general[["dataFormat"]] = "actiwatch_awd"
  params_general[["extEpochData_timeformat"]] = "%d-%b-%Y %H:%M:%S"
  convertEpochData(datadir = dn, metadatadir = "./output_tmp_testdata",
                   params_general = params_general)
  
  if (dir.exists(dn))  unlink(dn, recursive = TRUE)
  load(paste0(QCbasis, "/meta_Actiwatch.AWD.RData"))
  expect_equal(sum(M$metalong$nonwearscore), 63)
  expect_equal(nrow(M$metashort), 329)
  # # Next two lines commented out because these change pending the upcoming update to GGIRread
  # expect_equal(ncol(M$metashort), 3)
  # expect_equal(colnames(M$metashort), c("timestamp", "ZCY", "marker"))
  
  # Tidy up by deleting output folder
  if (file.exists(outputdir)) unlink(outputdir, recursive = TRUE)
  
  # Actiwatch CSV
  cat("\nActiwatch CSV")
  move2folder(system.file("testfiles/Actiwatch.csv", package = "GGIR")[1], dn)
  params_general[["windowsizes"]][1] = 15
  params_general[["dataFormat"]] = "actiwatch_csv"
  params_general[["extEpochData_timeformat"]] = "%d-%m-%Y %H:%M:%S"
  expect_error(convertEpochData(datadir = dn, metadatadir = "./output_tmp_testdata",
               params_general = params_general))
  params_general[["extEpochData_timeformat"]] = "%d/%m/%Y %H:%M:%S"
  convertEpochData(datadir = dn, metadatadir = "./output_tmp_testdata",
                   params_general = params_general)
  if (dir.exists(dn))  unlink(dn, recursive = TRUE)
  load(paste0(QCbasis, "/meta_Actiwatch.csv.RData"))
  expect_equal(sum(M$metalong$nonwearscore), 600)
  expect_equal(nrow(M$metashort), 860)
  # expect_equal(ncol(M$metashort), 4)
  # # Note: It says markering in next file because test file was Dutch
  # # this is an open issue https://github.com/wadpac/GGIRread/issues/75
  # expect_equal(colnames(M$metashort)[1:3], c("timestamp", "ZCY", "markering", "ExtSleep"))

  # Tidy up by deleting output folder
  if (file.exists(outputdir)) unlink(outputdir, recursive = TRUE)
  
  # ukbiobank CSV
  cat("\nukbiobank CSV")
  move2folder(system.file("testfiles/ukbiobank.csv", package = "GGIR")[1], dn)
  params_general[["windowsizes"]][1] = 5
  params_general[["dataFormat"]] = "ukbiobank_csv"
  convertEpochData(datadir = dn, metadatadir = "./output_tmp_testdata",
                   params_general = params_general)
  if (dir.exists(dn))  unlink(dn, recursive = TRUE)
  load(paste0(QCbasis, "/meta_ukbiobank.csv.RData"))
  expect_equal(sum(M$metalong$nonwearscore), 0)
  expect_equal(nrow(M$metashort), 492)
  expect_equal(ncol(M$metashort), 2)
  expect_equal(colnames(M$metashort), c("timestamp", "LFENMO"))

  # Tidy up by deleting output folder
  if (file.exists(outputdir)) unlink(outputdir, recursive = TRUE)
  
  # ActiGraph mode 61
  cat("\nActiGraph mode 61")
  move2folder(system.file("testfiles/ActiGraph61.csv", package = "GGIR")[1], dn)
  params_general[["windowsizes"]][1] = 5
  params_general[["dataFormat"]] = "actigraph_csv"
  params_general[["extEpochData_timeformat"]] = "%m/%d/%Y %H:%M:%S"
  convertEpochData(datadir = dn, metadatadir = "./output_tmp_testdata",
                   params_general = params_general)
  if (dir.exists(dn))  unlink(dn, recursive = TRUE)
  load(paste0(QCbasis, "/meta_ActiGraph61.csv.RData"))
  expect_equal(sum(M$metalong$nonwearscore), 165)
  expect_equal(nrow(M$metashort), 984)
  expect_equal(ncol(M$metashort), 6)
  expect_true(all(c("timestamp", "NeishabouriCount_x", "NeishabouriCount_y", 
                    "NeishabouriCount_z", "ExtStep") %in% colnames(M$metashort)))
  
  # Tidy up by deleting output folder
  if (file.exists(outputdir)) unlink(outputdir, recursive = TRUE)
  
  # ActiGraph mode 13
  cat("\nActiGraph mode 13")
  move2folder(system.file("testfiles/ActiGraph13.csv", package = "GGIR")[1], dn)
  params_general[["windowsizes"]][1] = 15
  params_general[["dataFormat"]] = "actigraph_csv"
  params_general[["extEpochData_timeformat"]] = "%m/%d/%Y %H:%M:%S"
  convertEpochData(datadir = dn, metadatadir = "./output_tmp_testdata",
                   params_general = params_general)
  if (dir.exists(dn))  unlink(dn, recursive = TRUE)
  load(paste0(QCbasis, "/meta_ActiGraph13.csv.RData"))
  expect_equal(sum(M$metalong$nonwearscore), 291)
  expect_equal(nrow(M$metashort), 988)
  expect_equal(ncol(M$metashort), 6)
  expect_true(all(c("timestamp", "NeishabouriCount_x", "NeishabouriCount_y",  "ExtStep",
                    "NeishabouriCount_z") %in% colnames(M$metashort)))
  
  # Tidy up by deleting output folder
  if (file.exists(outputdir)) unlink(outputdir, recursive = TRUE)

  # ActiGraph with headers
  cat("\nActiGraph with headers")
  move2folder(system.file("testfiles/ActiGraph13_timestamps_headers.csv", package = "GGIR")[1], dn)
  params_general[["windowsizes"]][1] = 1
  params_general[["dataFormat"]] = "actigraph_csv"
  params_general[["extEpochData_timeformat"]] = "%d-%m-%Y %H:%M:%S"
  convertEpochData(datadir = dn, metadatadir = "./output_tmp_testdata",
                   params_general = params_general)
  if (dir.exists(dn))  unlink(dn, recursive = TRUE)
  load(paste0(QCbasis, "/meta_ActiGraph13_timestamps_headers.csv.RData"))
  expect_equal(sum(M$metalong$nonwearscore), 0)
  expect_equal(nrow(M$metashort), 960)
  expect_equal(ncol(M$metashort), 6)
  expect_true(all(c("timestamp", "NeishabouriCount_x", "NeishabouriCount_y", 
                    "NeishabouriCount_z", "ExtStep", "NeishabouriCount_vm") %in% colnames(M$metashort)))
  
  # Tidy up by deleting output folder
  if (file.exists(outputdir)) unlink(outputdir, recursive = TRUE)
  

  # Sensewear
  cat("\nSensewear")
  move2folder(system.file("testfiles/sensewear.xls", package = "GGIR")[1], dn)
  params_general[["windowsizes"]][1] = 60
  params_general[["windowsizes"]][2] = 60
  params_general[["windowsizes"]][3] = 120
  params_general[["dataFormat"]] = "sensewear_xls"
  params_general[["extEpochData_timeformat"]] = "%d-%b-%Y %H:%M:%S"
  convertEpochData(datadir = dn, metadatadir = "./output_tmp_testdata",
                   params_general = params_general)
  if (dir.exists(dn))  unlink(dn, recursive = TRUE)
  load(paste0(QCbasis, "/meta_sensewear.xls.RData"))
  expect_equal(sum(M$metalong$nonwearscore), 0)
  expect_equal(nrow(M$metashort), 60)
  expect_equal(ncol(M$metashort), 4)
  expect_true(all(c("timestamp", "ExtAct", "ExtStep",
                    "ExtSleep") %in% colnames(M$metashort)))
  
  # Tidy up by deleting output folder
  if (file.exists(outputdir)) unlink(outputdir, recursive = TRUE)
  
  
})
