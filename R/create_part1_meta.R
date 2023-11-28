create_part1_meta = function(desired_outputdir = "output_test",
                             Ndays = 2, windowsizes = c(5, 900, 3600),
                             lux = FALSE) {
  # function to create output folder structure and fill it with part 1
  # milestone data. These data come from the objects in the data folder,
  # and it is repeated to fill in the desired duration as defined with Ndays.
  # ========================================
  # folder structure
  if (!dir.exists(desired_outputdir)) {
    dir.create("output_test/meta/basic", recursive = TRUE)
    dir.create("output_test/results/QC", recursive = TRUE)
  }
  data(data.calibrate); data(data.inspectfile); data(data.getmeta)
  M = data.getmeta; C = data.calibrate; I = data.inspectfile
  print(head(M$metashort))
  # extend M$metashort
  from = as.POSIXct(M$metashort$timestamp[1], tz = "")
  timestamp = seq.POSIXt(from = from, to = from + Ndays*24*60*60, by = windowsizes[1])
  anglez = rep(M$metashort$angle, length.out = length(timestamp))
  enmo = rep(M$metashort$ENMO, length.out = length(timestamp))
  M$metashort = data.frame(timestamp = POSIXtime2iso8601(timestamp, tz = ""),
                           anglez = anglez, ENMO = enmo)
  # extend M$metalong
  timestamp = seq.POSIXt(from = from, to = from + Ndays*24*60*60, by = windowsizes[2])
  nonwearscore = rep(M$metalong$nonwearscore, length.out = length(timestamp))
  clippingscore = rep(M$metalong$clippingscore, length.out = length(timestamp))
  lightmean = rep(M$metalong$lightmean, length.out = length(timestamp))
  lightpeak = rep(M$metalong$lightpeak, length.out = length(timestamp))
  temperaturemean = rep(M$metalong$temperaturemean, length.out = length(timestamp))
  EN = rep(M$metalong$EN, length.out = length(timestamp))
  M$metalong = data.frame(timestamp = POSIXtime2iso8601(timestamp, tz = ""),
                          nonwearscore = nonwearscore, clippingscore = clippingscore,
                          lightmean = lightmean, lightpeak, lightpeak, 
                          temperaturemean = temperaturemean, EN = EN)
  # other info
  filefoldername = "123A_testaccfile.csv"
  filename_dir = "123A_testaccfile.csv"
  tail_expansion_log = NULL
  # add lightmean and lightpeak to metalong
  if (lux == TRUE) {
    set.seed(400)
    M$metalong$lightmean = rnorm(n = nrow(M$metalong), mean = 1000, sd = 2000)
    M$metalong$lightmean[which(M$metalong$lightmean < 0)] = 0
    M$metalong$lightmean = M$metalong$lightmean / 1000
    M$metalong$lightpeak = M$metalong$lightmean + 0.2
  }
  # save part 1 metadata
  meta_fn = paste(desired_outputdir, "meta", "basic", 
                  "meta_123A_testaccfile.csv.RData", sep = .Platform$file.sep)
  save(C, I, M, filefoldername, filename_dir, tail_expansion_log,
       file = meta_fn)
}