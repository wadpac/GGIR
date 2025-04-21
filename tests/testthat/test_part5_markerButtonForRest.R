library(GGIR)
context("markerButtonForRest")

tz = "Pacific/Auckland"
startday = as.POSIXct(x = "2022-06-02 08:00:00", tz = tz)
ts = data.frame(time = seq(startday, startday + (16 * 3600), by = 60))
ts$marker = 0
ts$sibdetection = 0

test_that("Marker buttons influence sib adjustment as expected", {
  sib_start_original = as.POSIXct(c("2022-06-02 14:00:00", "2022-06-02 15:05:00"), tz = tz)
  sib_end_original = as.POSIXct(c("2022-06-02 14:20:00", "2022-06-02 15:25:00"), tz = tz)
  marker_times = as.POSIXct(c("2022-06-02 13:50:00", "2022-06-02 14:30:00"), tz = tz)
  
  sibreport = data.frame(ID = rep("test123", 2), type = c("sib", "sib"),
                         start = sib_start_original,
                         end = sib_end_original)
  ts$marker[which(ts$time %in% marker_times)] = 1

  sibreport$duration = as.numeric(difftime(time1 = sibreport$end,
                                           time2 = sibreport$start, units = "mins", tz = tz))
  params_sleep = load_params()$params_sleep
  params_sleep[["possible_nap_dur"]] =  c(15, 240)
  params_sleep[["possible_nap_window"]] =  c(9, 18)
  params_sleep[["nap_markerbutton_max_distance"]] = 30
  
  #==========================================================
  # ( 1 ) Method 1
  params_sleep[["nap_markerbutton_method"]] = 1
  test1 = markerButtonForRest(sibreport = sibreport, ts = ts,
                                  params_sleep = params_sleep)
  
  # updated timing for first sib because it has marker buttons nearby
  # but not for second sib
  expect_equal(test1$start, as.POSIXct(c("2022-06-02 13:50:00", "2022-06-02 15:05:00"), tz =  tz))
  expect_equal(test1$end, as.POSIXct(c("2022-06-02 14:30:00", "2022-06-02 15:25:00"), tz =  tz))
  # no sib is ignored in method 1
  expect_equal(test1$ignore, c(FALSE, FALSE))
  
  #==========================================================
  # ( 2 ) Method 2
  params_sleep[["nap_markerbutton_method"]] = 2
  test2 = markerButtonForRest(sibreport = sibreport, ts = ts,
                             params_sleep = params_sleep)
  
  # timing for sibs are not updated
  expect_equal(test2$start, sib_start_original)
  expect_equal(test2$end, sib_end_original)
  # second sib is ignored because it does not have marker button nearby
  expect_equal(test2$ignore, c(FALSE, TRUE))
  
  #==========================================================
  # ( 3 ) Method 3
  params_sleep[["nap_markerbutton_method"]] = 3
  test3 = markerButtonForRest(sibreport = sibreport, ts = ts,
                             params_sleep = params_sleep)
  
  # updated timing for first sib because it has marker buttons nearby
  # but not for second sib
  expect_equal(test3$start, test1$start)
  expect_equal(test3$end, test1$end)
  # second sib is ignored because it does not have marker button nearby
  expect_equal(test3$ignore, c(FALSE, TRUE))
  
  
  #==========================================================
  # ( 4 )  Method 3 - Distance 1
  params_sleep[["nap_markerbutton_max_distance"]] = 1
  params_sleep[["nap_markerbutton_method"]] = 3
  test4 = markerButtonForRest(sibreport = sibreport, ts = ts,
                             params_sleep = params_sleep)
  
  # timings are not updated because both sibs do not have marker button within less than 1 minute
  expect_equal(test4$start, sib_start_original)
  expect_equal(test4$end, sib_end_original)
  # both sibs are ignored because they do not have marker button within less than 1 minute
  expect_equal(test4$ignore, c(TRUE, TRUE))
  
  #==========================================================
  # ( 5 )  Method 3 - Distance 1 - Add 1 marker buttons inside the first sib
  marker_times = c(marker_times, as.POSIXct("2022-06-02 14:05:00", tz = tz))
  ts$marker[which(ts$time %in% marker_times)] = 1
  params_sleep[["nap_markerbutton_max_distance"]] = 1
  params_sleep[["nap_markerbutton_method"]] = 3
  test5 = markerButtonForRest(sibreport = sibreport, ts = ts,
                             params_sleep = params_sleep)
  
  # timings are not updated because both sibs do not have marker button within less than 1 minute
  # on both sides of the middle
  expect_equal(test5$start, sib_start_original)
  expect_equal(test5$end, sib_end_original)
  # both sibs are ignored because they do not have marker button within less than 1 minute
  expect_equal(test5$ignore, c(TRUE, TRUE))
  
  #==========================================================
  # ( 6 )  Method 3 - Distance 1 - Add 2nd marker button inside the first sib
  marker_times = c(marker_times, as.POSIXct("2022-06-02 14:15:00", tz = tz))
  ts$marker[which(ts$time %in% marker_times)] = 1
  params_sleep[["nap_markerbutton_max_distance"]] = 1
  params_sleep[["nap_markerbutton_method"]] = 3
  test6 = markerButtonForRest(sibreport = sibreport, ts = ts,
                             params_sleep = params_sleep)
  
  # updated timing for first sib because it has marker buttons nearby
  # but not for second sib
  expect_equal(test6$start, as.POSIXct(c("2022-06-02 14:05:00", "2022-06-02 15:05:00"), tz =  tz))
  expect_equal(test6$end, as.POSIXct(c("2022-06-02 14:15:00", "2022-06-02 15:25:00"), tz =  tz))
  # both sibs are ignored because they do not have marker button within less than 1 minute
  expect_equal(test6$ignore, c(FALSE, TRUE))
})
