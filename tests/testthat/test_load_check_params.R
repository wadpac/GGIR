library(GGIR)
context("loading and checking params")
test_that("load_params can load parameters", {
  skip_on_cran()
  
  params = load_params()
  expect_equal(params$params_sleep[[1]], 5)
  expect_equal(params$params_sleep[[3]], TRUE)
  expect_equal(params$params_cleaning[[5]], 0)
  expect_equal(params$params_sleep[[6]], "Y")
  
  # Test length of objects
  expect_equal(length(params), 8)
  expect_equal(length(params$params_sleep), 27)
  expect_equal(length(params$params_metrics), 41)
  expect_equal(length(params$params_rawdata), 39)
  expect_equal(length(params$params_247), 24)
  expect_equal(length(params$params_cleaning), 28)
  expect_equal(length(params$params_phyact), 14)
  expect_equal(length(params$params_output), 27)
  expect_equal(length(params$params_general), 17)

  params_sleep = params$params_sleep
  params_metrics = params$params_metrics
  params_rawdata = params$params_rawdata
  params_247 = params$params_247
  params_phyact = params$params_phyact
  params_cleaning = params$params_cleaning
  params_output = params$params_output
  params_general = params$params_general
  
  # Test that parameter check does not generate warnings:
  expect_warning(check_params(params_sleep), regexp = NA)
  # Test that parameter check does not generate errors:
  expect_error(check_params(params_sleep), regexp = NA)
  # Test that parameter check produces error when numeric is given a character value
  params_sleep$timethreshold = "ABC"
  expect_error(check_params(params_sleep), regexp = "Sleep parameter timethreshold is not numeric")
  params_sleep = params$params_sleep
  # Test that parameter check produces error when character is given a numeric value
  params_sleep$Sadeh_axis = 123
  expect_error(check_params(params_sleep), regexp = "Sleep parameter Sadeh_axis is not character")
  params_sleep = params$params_sleep
  # Test that parameter check produces error when boolean is given a numeric value
  params_general$overwrite = 123
  expect_error(check_params(params_general = params_general), regexp = "general parameter overwrite is not boolean")
  params_general = params$params_general
  
  # Error if brondcounts calculated
  params_metrics$do.brondcounts = TRUE
  expect_error(check_params(params_metrics = params_metrics))
  params_metrics = params$params_metrics
  
  # change HASPT algo to "notused" if length(def.noc.sleep) == 2
  params_sleep$def.noc.sleep = c(21, 8)
  check = check_params(params_sleep = params_sleep)
  expect_equal(check$params_sleep$HASPT.algo, "notused")
  params_sleep = params$params_sleep
  
  # if sensor.location = "hip", then HASPT should turn to "HorAngle"
  # also, three angles metrics should be turned to TRUE
  # also, sleepwindowType should be turned to TimeInBed
  params_general$sensor.location = "hip"
  check = expect_warning(check_params(params_general = params_general, 
                                      params_metrics = params_metrics, 
                                      params_sleep = params_sleep))
  expect_equal(check$params_sleep$HASPT.algo, "HorAngle")
  expect_true(check$params_metrics$do.anglex)
  expect_true(check$params_metrics$do.angley)
  expect_true(check$params_metrics$do.anglez)
  expect_equal(check$params_sleep$sleepwindowType, "TimeInBed")
  params_general = params$params_general
  
  # if HASIB.algo is "Sadeh1994", "Galland2012", "ColeKripke1992"
  # then Sadeh_axis should be one of "X", "Y", "Z"
  # and zero-crossing counts should be calculated
  # otherwise, Sadeh_axis should be empty
  params_sleep$HASIB.algo = "Sadeh1994"
  params_sleep$Sadeh_axis = "y"
  expect_warning(check_params(params_metrics = params_metrics, 
                              params_sleep = params_sleep),
                 regexp = "Parameter Sadeh_axis does not have meaningful value")
  params_sleep$Sadeh_axis = "Y"
  check = check_params(params_metrics = params_metrics, 
                       params_sleep = params_sleep)
  expect_true(check$params_metrics$do.zcy)
  params_sleep = params$params_sleep
  check = check_params(params_metrics = params_metrics, 
                       params_sleep = params_sleep)
  expect_equal(check$params_sleep$Sadeh_axis, "")
  params_sleep = params$params_sleep
  params_metrics = params$params_metrics
  
  # loglocation = "" should be turned to c()
  # loglocation = "C:\\mystudy\\mylog.csv" should turn the \ into /
  params_sleep$loglocation = ""
  check = check_params(params_sleep = params_sleep)
  expect_null(check$params_sleep$loglocation)
  params_sleep$loglocation = "C:\\mystudy\\mylog.csv"
  check = check_params(params_sleep = params_sleep)
  expect_true(grepl("/", check$params_sleep$loglocation))
  params_sleep = params$params_sleep
  
  # loglocation = "C:/mystudy/mylog.csv" and def.noc.sleep of length != 1 are not compatible
  params_sleep$loglocation = "C:/mystudy/mylog.csv"
  params_sleep$def.noc.sleep = c()
  expect_warning(check_params(params_sleep = params_sleep),
                 regexp = "loglocation was specified and def.noc.sleep")
  params_sleep = params$params_sleep
  
  # if no sleeplog and HASPT != "HorAngle", then sleepwindowType should be "SPT"
  params_sleep$sleepwindowType = "TimeInBed"
  expect_warning(check_params(params_sleep = params_sleep),
                 regexp = "Auto-updating sleepwindowType to SPT")
  params_sleep = params$params_sleep
  
  # if data_masking_strategy != 1, hrs.del.start and hrs.del.end != 0 do not make sense
  params_cleaning$data_masking_strategy = 2
  params_cleaning$hrs.del.start = 1
  expect_warning(check_params(params_cleaning = params_cleaning),
                 regexp = "Setting parameter hrs.del.start")
  params_cleaning = params$params_cleaning
  params_cleaning$data_masking_strategy = 2
  params_cleaning$hrs.del.end = 1
  expect_warning(check_params(params_cleaning = params_cleaning),
                 regexp = "Setting parameter hrs.del.end")
  params_cleaning = params$params_cleaning
  
  # if data_masking_strategy != c(3,5), ndayswindow != 7 does not make sense
  params_cleaning$ndayswindow = 14
  expect_warning(check_params(params_cleaning = params_cleaning),
                 regexp = "Setting parameter ndayswindow in combination")
  params_cleaning = params$params_cleaning
  
  # if data_masking_strategy == 5, ndayswindow should be integer
  params_cleaning$data_masking_strategy = 5
  params_cleaning$ndayswindow = 7.5
  check = expect_warning(check_params(params_cleaning = params_cleaning),
                         regexp = "Parameter ndayswindow has been rounded")
  expect_equal(check$params_cleaning$ndayswindow, 8)
  params_cleaning = params$params_cleaning
  
  # data_cleaning_file = "C:\\mystudy\\mycleaningfile.csv" should turn the \ into /
  params_cleaning$data_cleaning_file = "C:\\mystudy\\mycleaningfile.csv"
  check = check_params(params_cleaning = params_cleaning)
  expect_true(grepl("/", check$params_cleaning$data_cleaning_file))
  params_cleaning = params$params_cleaning
  
  # bout.metric should not be used anymore
  params_phyact$bout.metric = 6
  expect_warning(check_params(params_phyact = params_phyact),
                 regexp = "Parameters bout.metric and closedbout are no longer")
  params_phyact = params$params_phyact
  
  # length of mvpadur should be 3
  params_phyact$mvpadur = c(1, 2, 3, 5, 10, 15)
  check =  expect_warning(check_params(params_phyact = params_phyact),
                          regexp = "mvpadur needs to be a vector")
  expect_equal(length(check$params_phyact$mvpadur), 3)
  params_phyact = params$params_phyact
  
  # last value in LUX_day_segments should be 24
  params_247$LUX_day_segments = c(9, 18)
  check = check_params(params_247 = params_247)
  expect_equal(check$params_247$LUX_day_segments[length(check$params_247$LUX_day_segments)], 24)
  params_247 = params$params_247
  
  # if dataFormat = "actiwatch_awd", then acc.metric = "ZCY"
  params_general$dataFormat = "actiwatch_awd"
  check = expect_warning(check_params(params_general = params_general,
                                      params_metrics = params_metrics),
                         regexp = "When dataFormat is set to actiwatch_awd")
  expect_true(check$params_metrics$do.zcy)
  expect_equal(check$params_general$acc.metric, "ZCY")
  # also, no other metrics should be extracted
  params_metrics$do.anglex = TRUE
  params_general$acc.metric = "ZCY"
  check = expect_warning(check_params(params_general = params_general,
                                      params_metrics = params_metrics),
                         regexp = "When dataFormat is set to actiwatch_awd")
  expect_true(check$params_metrics$do.zcy)
  expect_false(check$params_metrics$do.anglex)
  params_general = params$params_general
  params_metrics = params$params_metrics
  
  # if dataFormat = "ukbiobank", then acc.metric = "LFENMO"
  params_general$dataFormat = "ukbiobank"
  check = expect_warning(check_params(params_general = params_general,
                                      params_metrics = params_metrics),
                         regexp = "When dataFormat is set to ukbiobank")
  expect_equal(check$params_general$acc.metric, "LFENMO")
  # also, no other metrics should be extracted
  params_metrics$do.anglex = TRUE
  params_general$acc.metric = "LFENMO"
  check = expect_warning(check_params(params_general = params_general,
                                      params_metrics = params_metrics),
                         regexp = "When dataFormat is set to ukbiobank")
  expect_false(check$params_metrics$do.anglex)
  params_general = params$params_general
  params_metrics = params$params_metrics
  
  # maxRecordingInterval cannot be higher than 21 days (21*24)
  params_general$maxRecordingInterval = 30*24
  expect_error(check_params(params_general = params_general),
               regexp = "A maxRecordingInterval value higher than 21 days")
  
  # segmentWEARcrit should be a number between 0 and 1
  params_cleaning$segmentWEARcrit.part5 = NULL
  expect_warning(check_params(params_cleaning = params_cleaning),
                 regexp = "segmentWEARcrit.part5 is expected to be")
  params_cleaning$segmentWEARcrit.part5 = 16
  expect_error(check_params(params_cleaning = params_cleaning),
                 regexp = "Incorrect value of segmentWEARcrit.part5")
  params_cleaning$segmentWEARcrit.part5 = 0.5
  
  # segmentDAYSPTcrit.part5 should be two numbers between 0 and 1
  params_cleaning$segmentDAYSPTcrit.part5 = NULL
  expect_error(check_params(params_cleaning = params_cleaning),
                 regexp = "Parameter segmentDAYSPTcrit.part5 is expected to be a numeric vector of length 2")
  params_cleaning$segmentDAYSPTcrit.part5 = c(-1, 0)
  expect_error(check_params(params_cleaning = params_cleaning),
               regexp = paste0("Incorrect values of segmentDAYSPTcrit.part5,",
                               " these should be a fractions between zero",
                               " and one, please change."))
  
})
