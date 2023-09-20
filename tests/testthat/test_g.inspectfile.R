library(GGIR)
context("g.inspectfile")
test_that("File extention and monitor type are correctly detected", {
  skip_on_cran()
  params_general = load_params()$params_general
  params_general[["overwrite"]] = TRUE
  params_rawdata = load_params()$params_rawdata
  
  # Function to create a new directory (dn) and copies the test file there
  dn = "./tmp_testdata"
  move2folder = function(fn, dn) {
    if (!dir.exists(dn)) {
      dir.create(dn)
    }
    file.copy(from = fn, to = dn)
  }

  if (dir.exists(dn))  unlink(dn, recursive = TRUE)
  
  # ActiGraph mode 61
  move2folder(system.file("testfiles/ActiGraph61.csv", package = "GGIR")[1], dn)
  I = g.inspectfile(paste0(dn, "/ActiGraph61.csv"),
                    desiredtz = params_general[["desiredtz"]], params_rawdata = params_rawdata)
  expect_equal(I$monc, MONITOR$ACTIGRAPH)
  expect_equal(I$dformc, FORMAT$CSV)

  if (dir.exists(dn))  unlink(dn, recursive = TRUE)

  # ActiGraph mode 13
  move2folder(system.file("testfiles/ActiGraph13.csv", package = "GGIR")[1], dn)
  I = g.inspectfile(paste0(dn, "/ActiGraph13.csv"),
                    desiredtz = params_general[["desiredtz"]], params_rawdata = params_rawdata)
  expect_equal(I$monc, MONITOR$ACTIGRAPH)
  expect_equal(I$dformc, FORMAT$CSV)

  if (dir.exists(dn))  unlink(dn, recursive = TRUE)  
})
