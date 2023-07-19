library(GGIR)
context("Lux handling in part 5")
test_that("lux_per_segment is correctly calculated", {
  skip_on_cran()
  
  # create test data (part 1 meta data with lux variables)
  # metashort
  Nshort = 2 * (1440*60/5)
  t0 = as.POSIXlt("2021-05-27 22:45:00", tz="Europe/Amsterdam", origin="1970-01-01")
  time = seq.POSIXt(t0, length.out = Nshort, by = 5) # the values of time are not used in the function, we only care about the length the object time
  timestamp = POSIXtime2iso8601(time, tz = "")
  ACC = rnorm(n = Nshort,mean=0,sd=40)
  anglez = rnorm(n = Nshort,mean=0,sd=40)
  metashort = data.frame(timestamp = timestamp, anglez = anglez, ENMO = ACC)
  
  # metalong
  Nlong = 2 * (1440*60/900)
  t0 = as.POSIXlt("2021-05-27 22:45:00", tz="Europe/Amsterdam", origin="1970-01-01")
  time = seq.POSIXt(t0, length.out = Nlong, by = 900) # the values of time are not used in the function, we only care about the length the object time
  timestamp = POSIXtime2iso8601(time, tz = "")
  lightmean = rnorm(n = Nlong,mean=1000,sd=4000)
  lightmean[which(lightmean < 0)] = 0
  lightpeak = lightmean + 1000
  temperaturemean = rnorm(n = Nlong,mean=25,sd=2)
  EN = rnorm(n = Nlong,mean=1,sd=40)
  metalong = data.frame(timestamp = timestamp, 
                        nonwearscore = 0, clippingscore = 0,
                        lightmean = lightmean, lightpeak = lightpeak,
                        temperaturemean = temperaturemean,
                        EN = EN)
  
  # create folder structure
  dir.create("output_test")
  dir.create("output_test/meta")
  dir.create("output_test/meta/basic")
  dir.create("output_test/results")
  dir.create("output_test/results/QC")
  
  
})



