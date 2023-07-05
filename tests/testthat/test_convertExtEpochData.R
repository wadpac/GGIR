library(GGIR)
context("convertEpochData")
test_that("External epoch data is correctly converted", {
  skip_on_cran()
  # library(testthat)
  # dirR = dir("~/GGIR/R", full.names = TRUE)
  # for (i in dirR) source(i)
  params_general = load_params()$params_general
  params_general[["overwrite"]] = TRUE
  params_general[["extEpochData_dateformat"]] = "%d/%m/%Y"
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
  move2folder(system.file("testfiles/Actiwatch.AWD", package = "GGIR")[1], dn)
  params_general[["windowsizes"]][1] = 60
  params_general[["dataFormat"]] = "actiwatch_awd"
  params_general[["extEpochData_dateformat"]] = "%d-%b-%Y"
  convertEpochData(datadir = dn, studyname = "tmp_testdata", outputdir = ".",
                              params_general = params_general)
  
  if (dir.exists(dn))  unlink(dn, recursive = TRUE)
  load(paste0(QCbasis, "/meta_Actiwatch.AWD.RData"))
  expect_equal(nrow(M$metashort), 329)
  expect_equal(ncol(M$metashort), 2)
  expect_equal(colnames(M$metashort), c("timestamp", "ZCY"))
  
  # Tidy up by deleting output folder
  if (file.exists(outputdir)) unlink(outputdir, recursive = TRUE)
  
  # Actiwatch CSV
  move2folder(system.file("testfiles/Actiwatch.csv", package = "GGIR")[1], dn)
  params_general[["windowsizes"]][1] = 15
  params_general[["dataFormat"]] = "actiwatch_csv"
  params_general[["extEpochData_dateformat"]] = "%d-%m-%Y"
  expect_error(convertEpochData(datadir = dn, studyname = "tmp_testdata", outputdir = ".",
                   params_general = params_general))
  params_general[["extEpochData_dateformat"]] = "%d/%m/%Y"
  convertEpochData(datadir = dn, studyname = "tmp_testdata", outputdir = ".",
                   params_general = params_general)
  if (dir.exists(dn))  unlink(dn, recursive = TRUE)
  load(paste0(QCbasis, "/meta_Actiwatch.csv.RData"))
  expect_equal(nrow(M$metashort), 860)
  expect_equal(ncol(M$metashort), 2)
  expect_equal(colnames(M$metashort), c("timestamp", "ZCY"))

  # Tidy up by deleting output folder
  if (file.exists(outputdir)) unlink(outputdir, recursive = TRUE)
  
  # ukbiobank CSV
  move2folder(system.file("testfiles/ukbiobank.csv", package = "GGIR")[1], dn)
  params_general[["windowsizes"]][1] = 5
  params_general[["dataFormat"]] = "ukbiobank_csv"
  convertEpochData(datadir = dn, studyname = "tmp_testdata", outputdir = ".",
                   params_general = params_general)
  if (dir.exists(dn))  unlink(dn, recursive = TRUE)
  load(paste0(QCbasis, "/meta_ukbiobank.csv.RData"))
  expect_equal(nrow(M$metashort), 492)
  expect_equal(ncol(M$metashort), 2)
  expect_equal(colnames(M$metashort), c("timestamp", "LFENMO"))

  # Tidy up by deleting output folder
  if (file.exists(outputdir)) unlink(outputdir, recursive = TRUE)
  
  # ActiGraph mode 61
  move2folder(system.file("testfiles/ActiGraph61.csv", package = "GGIR")[1], dn)
  params_general[["windowsizes"]][1] = 5
  params_general[["dataFormat"]] = "actigraph_csv"
  params_general[["extEpochData_dateformat"]] = "%m/%d/%Y"
  convertEpochData(datadir = dn, studyname = "tmp_testdata", outputdir = ".",
                   params_general = params_general)
  if (dir.exists(dn))  unlink(dn, recursive = TRUE)
  load(paste0(QCbasis, "/meta_ActiGraph61.csv.RData"))
  expect_equal(nrow(M$metashort), 984)
  expect_equal(ncol(M$metashort), 4)
  expect_equal(colnames(M$metashort), c("timestamp", "NeishabouriCount_x",
                                        "NeishabouriCount_y", "NeishabouriCount_z"))
  
  # Tidy up by deleting output folder
  if (file.exists(outputdir)) unlink(outputdir, recursive = TRUE)
  
  # ActiGraph mode 61
  move2folder(system.file("testfiles/ActiGraph13.csv", package = "GGIR")[1], dn)
  params_general[["windowsizes"]][1] = 15
  params_general[["dataFormat"]] = "actigraph_csv"
  params_general[["extEpochData_dateformat"]] = "%m/%d/%Y"
  convertEpochData(datadir = dn, studyname = "tmp_testdata", outputdir = ".",
                   params_general = params_general)
  if (dir.exists(dn))  unlink(dn, recursive = TRUE)
  load(paste0(QCbasis, "/meta_ActiGraph13.csv.RData"))
  expect_equal(nrow(M$metashort), 988)
  expect_equal(ncol(M$metashort), 4)
  expect_equal(colnames(M$metashort), c("timestamp", "NeishabouriCount_x",
                                        "NeishabouriCount_y", "NeishabouriCount_z"))
  
  # Tidy up by deleting output folder
  if (file.exists(outputdir)) unlink(outputdir, recursive = TRUE)
  
  
})
