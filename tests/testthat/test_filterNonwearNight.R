library(GGIR)
context("filterNonwearNight")
test_that("filters nonwear", {
  skip_on_cran()
  
  # Function filterNonwearNight is called via g.weardec
  # and here we test it via calls to g.weardec
  
  data(data.metalong)
  metalong = data.metalong$metalong
  params_cleaning = load_params()$params_cleaning
  params_cleaning[["nonwearFiltermaxHours"]] = 3
  # add add short lasting nonwear that will be filtered
  # it needs to be too short for nonwear detection
  N = length(metalong$nonwearscore)
  metalong$nonwearscore = rep(c(0, 0, 0, 3), times = ceiling(N / 4))[1:N]
  
  #========================================
  # No log, no default
  expected_error_message = paste0("Please specify parameter nonwearFilterWindow ",
                                  "or qwindow as diary with columns to define the ",
                                  "window for filtering short nonwear. See documentation ",
                                  "for parameter nonwearFiltermaxHours")
  expect_error(g.weardec(metalong = metalong,
                         wearthreshold = 2,
                         ws2 = 900,
                         params_cleaning = params_cleaning,
                         desiredtz = "Europe/London",
                         qwindowImp = NULL), regexp = expected_error_message)

  #========================================
  # No log, with default
  params_cleaning[["nonwearFilterWindow"]] = c(0, 6)
  out_nolog_withdef = g.weardec(metalong = metalong,
                         wearthreshold = 2,
                         ws2 = 900,
                         params_cleaning = params_cleaning,
                         desiredtz = "Europe/Amsterdam",
                         qwindowImp = NULL)
  
  
  expect_equal(out_nolog_withdef$nonwearHoursFiltered, 13.5)
  expect_equal(out_nolog_withdef$nonwearEventsFiltered, 54)
  
  
  params_cleaning[["nonwearFilterWindow"]] = NULL
  #========================================
  # With log, no default
  
  # create synthetic qwindowImp object that is 
  # normally produced by function g.conv.actlog based on a activity diary
  qwindowImp = data.frame(ID = rep("101", 10),
                          date = format(seq(as.Date("2022-06-01"), as.Date("2022-06-10"))),
                          qwindow_times = rep("", 10),
                          qwindow_names = rep("", 10))
  for (i in 1:10) {
    qwindowImp$qwindow_names[i] = list(paste0(c("daystart", "outbed", "inbed", "dayend"), i))
    qwindowImp$qwindow_times[i] = list(paste0(c("00:00", "09:00", "21:00", "24:00"), i))
    qwindowImp$qwindow_values[i] = list(c(0, 9, 21, 24))
  }
  
  out_withlog_nodef = g.weardec(metalong = metalong,
                  wearthreshold = 2,
                  ws2 = 900,
                  params_cleaning = params_cleaning,
                  desiredtz = "Europe/Amsterdam",
                  qwindowImp = qwindowImp)
  expect_equal(out_withlog_nodef$nonwearHoursFiltered, 27)
  expect_equal(out_withlog_nodef$nonwearEventsFiltered, 108)
  
  # double check that timing of un-filtered nonwear is intuitive
  N_nonwear_night = length(grep("T21|T22|T23|T01|T02|T03|T04|T05|T06|T07|T08", 
                                metalong$timestamp[which(out_withlog_nodef$r1 == 1)]))
  N_nonwear_day = length(grep("T09|T10|T11|T12|T13|T14|T15|T16|T17|T18|T19|T20", 
                              metalong$timestamp[which(out_withlog_nodef$r1 == 1)]))
  expect_equal(N_nonwear_night, 0)
  expect_equal(N_nonwear_day, 107)
})
