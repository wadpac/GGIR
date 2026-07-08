library(GGIR)
context("g.part1")
test_that("Part 1 can run with all metrics", {
  skip_on_cran()
  #=======================
  # Part 1 with all metrics
  Ndays = 2
  # Using sf = 30 Hertz  because metric zc filters at 4 Hertz and brondcounts requires 30 Hertz
  create_test_acc_csv(Nmin = Ndays * 1440, sf = 30)
  fn = "123A_testaccfile.csv"
  metadatadir = paste0(getwd(), "/output_test")
  desiredtz = "Europe/London"
  dn = "output_test"
  
  # external function,  which adds a index per second
  exampleExtFunction = function(data, parameters) {
    output = data.frame(secondInDay = seq(from = 0, to = floor(nrow(data) / 30), by = 1))
    output$copy1 = output$copy2 = output$secondInDay
    return(output)
  }
  myfun =  list(FUN = exampleExtFunction,
                parameters = 1,
                expected_sample_rate =  30, # resample data to 30 Hertz before applying function
                expected_unit = "mg",
                minlength = 1,
                outputres = 1,
                colnames = c("secondInDay", "copy1", "copy2"),
                outputtype = "numeric", #"numeric" (averaging is possible), "category" (majority vote)
                aggfunction = mean,
                timestamp = F,
                reporttype = rep("scalar", 3)) # for unit test only
  
  if (file.exists(dn))  unlink(dn, recursive = TRUE)
  g.part1(datadir = fn, metadatadir = metadatadir, f0 = 1, f1 = 1, overwrite = TRUE, desiredtz = desiredtz,
          do.cal = FALSE, do.anglex = TRUE,
          # We are not doing all the metrics, because Travis-CI cannot allocate enough memory
          do.enmo = TRUE,do.lfenmo = TRUE,
          do.bfen = TRUE, do.hfenplus = TRUE,
          do.mad = TRUE, do.zcx = TRUE,
          windowsizes = c(15, 3600, 3600), do.parallel = FALSE,
          minimumFileSizeMB = 0, verbose = FALSE, myfun = myfun)
  
  rn = dir("output_test/meta/basic/",full.names = TRUE)
  load(rn[1])
  expect_equal(ncol(M$metashort), 12)
  expect_true(nrow(M$metashort) == 11280)
  expect_equal(mean(M$metashort$BFEN),  0.0458, tolerance = 4)
  expect_equal(mean(M$metashort$LFENMO),  0.0447, tolerance = 4)
  expect_equal(mean(M$metashort$HFENplus),  0.0914, tolerance = 4)
  expect_equal(mean(M$metashort$MAD),  0.0073, tolerance = 4)
  expect_equal(mean(M$metashort$anglex),  57.4683, tolerance = 4)
  expect_equal(mean(M$metashort$anglez),  0.3522, tolerance = 4)
  expect_equal(mean(M$metashort$ZCX),  14.94, tolerance = 2)
  expect_equal(mean(M$metashort$secondInDay),  42318.65, tolerance = 2)
  expect_equal(M$metashort$copy1, M$metashort$copy2)
  if (file.exists(fn)) file.remove(fn)
})
