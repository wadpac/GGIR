library(GGIR)
context("read.myacc.csv")
test_that("read.myacc.csv can handle files without header, no decimal places in timestamps, temperature", {
  skip_on_cran()
  old_options = options()
  # create test files
  N = 30
  sf = 30
  options(digits.secs = 4)
  
  # NOTE THIS UNIT TEST STILL USES POSIXlt A LOT
  # BECAUSE OLDER R RELEASE CANNOT HANDLE POSIXct for this purpose
  t0 = as.POSIXlt(x = "2022-11-02 14:01:16.00", tz = "Europe/Amsterdam") 
  timeseq = t0 + ((0:(N - 1))/sf)
  time = as.POSIXlt(timeseq, origin = "1970-1-1", tz = "Europe/London")
  testfile = matrix("", 4, 1)
  set.seed(100)
  accx = rnorm(N)
  set.seed(200)
  accy = rnorm(N)
  set.seed(300)
  accz = rnorm(N)
  set.seed(400)
  temp = rnorm(N)
  wear = c(rep(TRUE,N/3),rep(FALSE,N/6),rep(TRUE,N/3),rep(TRUE,N/6))
  
  # No header, but otherwise normal
  S1 = data.frame(x = accx, time = time, 
                  y = accy, z = accz, temp = temp + 20, 
                  stringsAsFactors = TRUE)
  
  testfile[1] = "testcsv1.csv"
  write.csv(S1, file = testfile[1], row.names = FALSE)
  
  # With decimal places in seconds removed
  testfile[2] = "testcsv2.csv"
  S1$time = round(S1$time)
  write.csv(S1, file = testfile[2], row.names = FALSE)
  
  # Without temperature
  S2 = data.frame(x = accx, time = time, y = accy, z = accz, stringsAsFactors = TRUE)
  testfile[3] = "testcsv3.csv"
  write.csv(S2, file = testfile[3], row.names = FALSE)
  
  # Without temperature, without time, with a wear channel
  S3 = data.frame(x = accx, y = accy, z = accz, wear = wear, stringsAsFactors = TRUE)
  testfile[4] = "testcsv4.csv"
  write.csv(S3, file = testfile[4], row.names = FALSE)
  
  # UNIXsec
  testfile[5] = "testcsv5.csv"
  S5 = S1
  S5$time = as.numeric(S5$time)
  write.csv(S5, file = testfile[5], row.names = FALSE)
  
  # UNIXmsec
  testfile[6] = "testcsv6.csv"
  S6 = S5
  S6$time = S6$time * 1000
  write.csv(S6, file = testfile[6], row.names = FALSE)
  
  # attempt to load
  D1 = read.myacc.csv(rmc.file = testfile[1], rmc.nrow = 20, rmc.dec = ".",
                      rmc.firstrow.acc = 1, rmc.firstrow.header = c(),
                      rmc.col.acc = c(1,3,4), rmc.col.temp = 5, rmc.col.time = 2,
                      rmc.unit.acc = "g", rmc.unit.temp = "C",
                      rmc.format.time = "%Y-%m-%d %H:%M:%OS", rmc.origin = "1970-01-01",
                      desiredtz = "Europe/London", rmc.sf = sf,
                      rmc.headername.sf = "sample_frequency",
                      rmc.headername.sn = "serial_number",
                      rmc.headername.recordingid = "ID")
  
  expect_equal(mean(D1$data$x), 0.1078671, tol = 0.0001)
  expect_equal(mean(D1$data$y), -0.06675716, tol = 0.0001)
  
  # attempt to load as if values are in mg and scale by 100
  D1B = read.myacc.csv(rmc.file = testfile[1], rmc.nrow = 20, rmc.dec = ".",
                      rmc.firstrow.acc = 1, rmc.firstrow.header = c(),
                      rmc.col.acc = c(1,3,4), rmc.col.temp = 5, rmc.col.time = 2,
                      rmc.unit.acc = "mg", rmc.unit.temp = "C",
                      rmc.format.time = "%Y-%m-%d %H:%M:%OS", rmc.origin = "1970-01-01",
                      rmc.scalefactor.acc = 100,
                      desiredtz = "Europe/London", rmc.sf = sf,
                      rmc.headername.sf = "sample_frequency",
                      rmc.headername.sn = "serial_number",
                      rmc.headername.recordingid = "ID")
  
  expect_equal(mean(D1B$data$x), 0.01078671, tol = 0.0001)
  expect_equal(mean(D1B$data$y), -0.006675716, tol = 0.0001)
  
  #------------------------
  # Warnings and errors related to desiredtz and configtz
  # warning if desiredtz not provided but rmc.desiredtz provided:
  expect_warning(read.myacc.csv(rmc.file = testfile[1], rmc.nrow = 20, rmc.dec = ".",
                                rmc.firstrow.acc = 1, rmc.firstrow.header = c(),
                                rmc.col.acc = c(1,3,4), rmc.col.temp = 5, rmc.col.time = 2,
                                rmc.unit.acc = "g", rmc.unit.temp = "C", 
                                rmc.format.time = "%Y-%m-%d %H:%M:%OS",
                                rmc.origin = "1970-01-01",
                                rmc.desiredtz = "Europe/London", rmc.sf = sf,
                                rmc.headername.sf = "sample_frequency",
                                rmc.headername.sn = "serial_number",
                                rmc.headername.recordingid = "ID"))
  
  #  now also with configtz
  expect_warning(read.myacc.csv(rmc.file = testfile[1], rmc.nrow = 20, rmc.dec = ".",
                                rmc.firstrow.acc = 1, rmc.firstrow.header = c(),
                                rmc.col.acc = c(1,3,4), rmc.col.temp = 5, rmc.col.time = 2,
                                rmc.unit.acc = "g", rmc.unit.temp = "C", 
                                rmc.format.time = "%Y-%m-%d %H:%M:%OS",
                                rmc.origin = "1970-01-01",
                                desiredtz = "Europe/London", 
                                rmc.configtz = "Europe/Madrid",
                                rmc.sf = sf,
                                rmc.headername.sf = "sample_frequency",
                                rmc.headername.sn = "serial_number",
                                rmc.headername.recordingid = "ID"))
  
  # error if none of rmc.desiredtz and desiredtz are provided:
  expect_error(read.myacc.csv(rmc.file = testfile[1], rmc.nrow = 20, rmc.dec = ".",
                              rmc.firstrow.acc = 1, rmc.firstrow.header = c(),
                              rmc.col.acc = c(1,3,4), rmc.col.temp = 5, rmc.col.time = 2,
                              rmc.unit.acc = "g", rmc.unit.temp = "C", 
                              rmc.format.time = "%Y-%m-%d %H:%M:%OS",
                              rmc.origin = "1970-01-01",
                              rmc.sf = sf,
                              rmc.headername.sf = "sample_frequency",
                              rmc.headername.sn = "serial_number",
                              rmc.headername.recordingid = "ID"))
  
  # error if both rmc.desiredtz and desiredtz are provided:
  expect_error(read.myacc.csv(rmc.file = testfile[1], rmc.nrow = 20, rmc.dec = ".",
                              rmc.firstrow.acc = 1, rmc.firstrow.header = c(),
                              rmc.col.acc = c(1,3,4), rmc.col.temp = 5, rmc.col.time = 2,
                              rmc.unit.acc = "g", rmc.unit.temp = "C", 
                              rmc.format.time = "%Y-%m-%d %H:%M:%OS",
                              rmc.origin = "1970-01-01",
                              desiredtz = "Europe/London", rmc.desiredtz = "Europe/Madrid",
                              rmc.sf = sf,
                              rmc.headername.sf = "sample_frequency",
                              rmc.headername.sn = "serial_number",
                              rmc.headername.recordingid = "ID"))
  
  
  # Evaluate with decimal places in seconds
  expect_equal(nrow(D1$data), 20)
  expect_equal(ncol(D1$data), 5)
  expect_equal(strftime(as.POSIXct(D1$data$time[1:5], tz = "Europe/London", origin = "1970-01-01"),
                        format = '%Y-%m-%d %H:%M:%OS2', tz = "Europe/London"), 
               c("2022-11-02 13:01:16.00",
                 "2022-11-02 13:01:16.03",
                 "2022-11-02 13:01:16.06",
                 "2022-11-02 13:01:16.09",
                 "2022-11-02 13:01:16.13"))
  expect_that(D1$header,equals("no header"))
  
  # Evaluate without decimal places decimal places in seconds and
  # different config timezone
  D2 = read.myacc.csv(rmc.file = testfile[2], rmc.nrow = 20, rmc.dec = ".",
                      rmc.firstrow.acc = 1, rmc.firstrow.header = c(),
                      rmc.col.acc = c(1,3,4), rmc.col.temp = 5, rmc.col.time = 2,
                      rmc.unit.acc = "g", rmc.unit.temp = "C", rmc.format.time = "%Y-%m-%d %H:%M:%OS",
                      rmc.origin = "1970-01-01",
                      desiredtz = "Europe/London", rmc.sf = sf,
                      configtz = "America/Chicago",
                      rmc.headername.sf = "sample_frequency",
                      rmc.headername.sn = "serial_number",
                      rmc.headername.recordingid = "ID")
  expect_equal(nrow(D2$data), 20)
  expect_equal(ncol(D2$data), 5)
  expect_equal(strftime(as.POSIXct(D2$data$time[1:5], tz = "Europe/London", origin = "1970-01-01"), 
                        format = '%Y-%m-%d %H:%M:%OS2', tz = "Europe/London"),
               c("2022-11-02 18:01:16.50", "2022-11-02 18:01:16.53",
                 "2022-11-02 18:01:16.56", "2022-11-02 18:01:16.59",
                 "2022-11-02 18:01:16.63"))
  expect_that(D2$header,equals("no header"))
  
  D3 = read.myacc.csv(rmc.file = testfile[3], rmc.nrow = 20, rmc.dec = ".",
                      rmc.firstrow.acc = 1, rmc.firstrow.header = c(),
                      rmc.col.acc = c(1,3,4), rmc.col.temp = c(),
                      rmc.col.time = 2,
                      rmc.unit.acc = "g", rmc.unit.temp = "C",
                      rmc.format.time = "%Y-%m-%d %H:%M:%OS",
                      rmc.origin = "1970-01-01",
                      desiredtz = "Europe/London", 
                      rmc.sf = sf,
                      rmc.headername.sf = "sample_frequency",
                      rmc.headername.sn = "serial_number",
                      rmc.headername.recordingid = "ID")
  expect_that(nrow(D3$data),equals(20))
  expect_that(ncol(D3$data),equals(4))
  expect_that(D3$header,equals("no header"))
  
  D4 = read.myacc.csv(rmc.file = testfile[4], rmc.nrow = 20, 
                      rmc.dec = ".",
                      rmc.firstrow.acc = 1, rmc.firstrow.header = c(),
                      rmc.col.acc = 1:3, rmc.col.temp = c(), rmc.col.time = c(),
                      rmc.unit.acc = "g", rmc.unit.temp = "C", 
                      rmc.format.time = "%Y-%m-%d %H:%M:%OS",
                      rmc.origin = "1970-01-01",
                      desiredtz = "Europe/London", rmc.sf = sf,
                      rmc.headername.sf = "sample_frequency",
                      rmc.headername.sn = "serial_number",
                      rmc.headername.recordingid = "ID",
                      rmc.col.wear = 4)
  expect_that(nrow(D4$data), equals(20))
  expect_true(D4$data[1, 4])
  expect_that(ncol(D4$data), equals(4))
  expect_that(D4$header, equals("no header"))
  
  # Timestamps in UNIXsec
  D5 = read.myacc.csv(rmc.file = testfile[5], rmc.nrow = 20, rmc.dec = ".",
                      rmc.firstrow.acc = 1, rmc.firstrow.header = c(),
                      rmc.col.acc = c(1,3,4), rmc.col.temp = 5, rmc.col.time = 2,
                      rmc.unit.acc = "g", 
                      rmc.unit.time = "UNIXsec",
                      desiredtz = "Europe/London", rmc.sf = sf)
  
  expect_equal(mean(D5$data$x), 0.1078671, tol = 0.0001)
  expect_equal(mean(D5$data$y), -0.06675716, tol = 0.0001)
  expect_equal(D5$data$time[1], 1667394076, tol = 0.1)
  
  # Timestamps in UNIXmsec
  D6 = read.myacc.csv(rmc.file = testfile[6], rmc.nrow = 20, rmc.dec = ".",
                      rmc.firstrow.acc = 1, rmc.firstrow.header = c(),
                      rmc.col.acc = c(1,3,4), rmc.col.temp = 5, rmc.col.time = 2,
                      rmc.unit.acc = "g", 
                      rmc.unit.time = "UNIXmsec",
                      desiredtz = "Europe/London", rmc.sf = sf)
  expect_equal(mean(D6$data$x), 0.1078671, tol = 0.0001)
  expect_equal(mean(D6$data$y), -0.06675716, tol = 0.0001)
  expect_equal(D6$data$time[1], 1667394076, tol = 0.1)
  
  for (i in 1:length(testfile)) {
    expect_true(file.exists(testfile[i]))
    if (file.exists(testfile[i])) file.remove(testfile[i])
  }
  options(old_options)
})

test_that("read.myacc.csv can handle header and bit-value acceleration", {
  skip_on_cran()
  old_options = options()
  # create test files
  N = 30
  sf = 30
  options(digits.secs = 4)
  t0 = as.POSIXlt(x = "2022-11-02 14:01:16.00", tz = "Europe/Amsterdam")
  timeseq = t0 + ((0:(N - 1))/sf)
  time = as.POSIXlt(timeseq, origin = "1970-1-1", tz = "Europe/London")
  testfile = matrix("", 3, 1)
  set.seed(100)
  accx = rnorm(N)
  set.seed(200)
  accy = rnorm(N)
  set.seed(300)
  accz = rnorm(N)
  set.seed(400)
  temp = rnorm(N)
  wear = c(rep(TRUE, N / 3), rep(FALSE, N / 6),
           rep(TRUE, N / 3), rep(TRUE, N / 6))

  # 1: With 2-column header, with temperature, with time
  S1 = as.matrix(data.frame(x = accx, time = time, y = accy,
                            z = accz, temp = temp + 20, stringsAsFactors = TRUE))
  hd_NR = 10
  hd = matrix("", hd_NR + 1, ncol(S1))
  hd[1, 1:2] = c("ID","12345")
  hd[2, 1:2] = c("sample_rate","30")
  hd[3, 1:2] = c("serial_number","30")
  hd[4, 1:2] = c("bit","8")
  hd[5, 1:2] = c("dynamic_range","6")
  S1 = rbind(hd, S1)
  S1[hd_NR + 1,] = colnames(S1)
  colnames(S1) = NULL
  testfile[1] = "testcsv1.csv"
  write.table(S1, file = testfile[1], col.names = FALSE, row.names = FALSE)

  # 2: With 2-column header, with temperature, with time, bit-value acceleration unit
  bits = 8
  set.seed(100)
  xb = sample(x = 1:(2^bits),size = N,replace = TRUE)
  set.seed(200)
  yb = sample(x = 1:(2^bits),size = N,replace = TRUE)
  set.seed(300)
  zb = sample(x = 1:(2^bits),size = N,replace = TRUE)
  set.seed(400)
  temp3 = rnorm(N)
  S3 = S2 = as.matrix(data.frame(x = xb, time = time, y = yb,
                            z = zb, temp = temp3 + 20, stringsAsFactors = TRUE))
  S2 = rbind(hd, S2)
  S2[hd_NR + 1, ] = colnames(S2)
  colnames(S2) = NULL
  testfile[2] = "testcsv2.csv"
  write.table(S2, file = testfile[2], col.names = FALSE, row.names = FALSE)

  # 2. A header in 1 column:
  hd2 = matrix("", hd_NR + 1, ncol(S3))
  hd2[1, 1:2] = c("ID: 12345", "")
  hd2[2, 1:2] = c("sample_rate: 30", "")
  hd2[3, 1:2] = c("serial_number: 30", "")
  hd2[4, 1:2] = c("bit: 8", "")
  hd2[5, 1:2] = c("dynamic_range: 6", "")
  S3 = rbind(hd2, S3)
  S3[hd_NR + 1,] = colnames(S3)
  colnames(S3) = NULL
  testfile[3] = "testcsv3.csv"
  write.table(S3, file = testfile[3], col.names = FALSE, row.names = FALSE)



  #------------------------
  # Test 1 - 2 column header
  D1 = read.myacc.csv(rmc.file = testfile[1], rmc.nrow = 20, rmc.dec = ".",
                      rmc.firstrow.acc = 11, rmc.firstrow.header = 1,
                      rmc.col.acc = c(1, 3, 4), rmc.col.temp = 5, rmc.col.time = 2,
                      rmc.unit.acc = "g", rmc.unit.temp = "C",
                      rmc.format.time = "%Y-%m-%d %H:%M:%OS",
                      rmc.origin = "1970-01-01",
                      desiredtz = "Europe/London",
                      rmc.headername.sf = "sample_frequency",
                      rmc.headername.sn = "serial_number",
                      rmc.headername.recordingid = "ID")
  expect_that(nrow(D1$data), equals(20))
  expect_that(ncol(D1$data), equals(5))
  expect_that(nrow(D1$header), equals(5))
  expect_that(ncol(D1$header), equals(1))
  expect_equal(as.numeric(D1$header["sample_rate",1]), 30)
  
  # Test 2 - 2 column header, bit-valued acceleration
  D2 = read.myacc.csv(rmc.file = testfile[2], rmc.nrow = 20, rmc.dec = ".",
                      rmc.firstrow.acc = 11, rmc.firstrow.header = 1,
                      rmc.col.acc = c(1,3,4), rmc.col.temp = 5,
                      rmc.col.time = 2,
                      rmc.unit.acc = "bit", rmc.unit.temp = "C",
                      rmc.format.time = "%Y-%m-%d %H:%M:%OS",
                      rmc.origin = "1970-01-01",
                      desiredtz = "Europe/London", rmc.sf = sf,
                      rmc.headername.sf = "sample_rate",
                      rmc.headername.sn = "serial_number",
                      rmc.headername.recordingid = "ID", rmc.bitrate = "bit",
                      rmc.dynamic_range = "dynamic_range",
                      rmc.header.structure = c())
  expect_that(nrow(D2$data),equals(20))
  expect_that(ncol(D2$data),equals(5))
  expect_that(nrow(D2$header),equals(5))
  expect_that(ncol(D2$header),equals(1))
  
  # Test 3 - 1 column header, bit-valued acceleration
  D3 = read.myacc.csv(rmc.file = testfile[3], rmc.nrow = 20, rmc.dec = ".",
                      rmc.firstrow.acc = 11, rmc.firstrow.header = 1,
                      rmc.header.length = 5,
                      rmc.col.acc = c(1,3,4), rmc.col.temp = 5,
                      rmc.col.time = 2,
                      rmc.unit.acc = "bit", rmc.unit.temp = "C",
                      rmc.format.time = "%Y-%m-%d %H:%M:%OS",
                      rmc.origin = "1970-01-01",
                      desiredtz = "Europe/London", rmc.sf = sf,
                      rmc.headername.sf = "sample_rate",
                      rmc.headername.sn = "serial_number",
                      rmc.headername.recordingid = "ID",
                      rmc.bitrate = "bit", rmc.dynamic_range = "dynamic_range",
                      rmc.header.structure = ": ", rmc.check4timegaps = TRUE)
  # 
  expect_equal(mean(D3$data[,2]), 0.761, tolerance = 3)
  expect_equal(mean(D3$data[,3]), -0.62, tolerance = 2)
  expect_equal(mean(D3$data[,4]), -0.36, tolerance = 1)
  expect_equal(mean(D3$data[,5]), 20.1, tolerance = 1)
  expect_equal(D3$data[2,2], 0.9768219, tolerance  = 3)
  expect_equal(ncol(D3$data), 5)
  expect_equal(nrow(D3$header),5)
  expect_equal(ncol(D3$header), 1)
  expect_equal(as.numeric(D3$header[1,]), 12345)
  expect_equal(as.numeric(D3$header[2,]), 30)

  for (i in 1:length(testfile)) {
    expect_true(file.exists(testfile[i]))
    if (file.exists(testfile[i])) file.remove(testfile[i])
  }
   options(old_options)
})

test_that("read.myacc.csv can handle gaps in time and irregular sample rate", {
  skip_on_cran()
  old_options = options()
  # create test files
  N = 30
  sf = 30
  options(digits.secs = 4)
  t0 = as.POSIXlt(x = "2022-11-02 14:01:16.00", tz = "Europe/Amsterdam")
  timeseq = t0 + ((0:(N - 1))/sf)
  time = as.POSIXlt(timeseq, origin = "1970-1-1", tz = "Europe/London")
  testfile = matrix("", 1, 1)
  set.seed(100)
  accx = rnorm(N)
  set.seed(200)
  accy = rnorm(N)
  set.seed(300)
  accz = rnorm(N)
  set.seed(400)
  temp = rnorm(N)
  wear = c(rep(TRUE, N / 3), rep(FALSE, N / 6),
           rep(TRUE, N / 3), rep(TRUE, N / 6))
  
  # 1: With 2-column header, with temperature, with gap in time, variation in sample rate
  time_gap = time
  time_gap[10:length(time_gap)] = time_gap[10:length(time_gap)] + 5 # add gap of 5 seconds
  time_gap = time_gap + rnorm(n = length(time_gap), mean = 0, sd = 0.0001)
  S1 = as.matrix(data.frame(x = accx, time = time_gap, y = accy,
                            z = accz, temp = temp + 20, stringsAsFactors = TRUE))
  hd_NR = 10
  hd = matrix("", hd_NR + 1, ncol(S1))
  hd[1, 1:2] = c("ID","12345")
  hd[2, 1:2] = c("sample_rate","30")
  hd[3, 1:2] = c("serial_number","30")
  S1 = rbind(hd, S1)
  S1[hd_NR + 1,] = colnames(S1)
  colnames(S1) = NULL
  testfile[1] = "testcsv1.csv"
  write.table(S1, file = testfile[1], col.names = FALSE, row.names = FALSE)
  
  
  #------------------------
  # Test 1 - time gaps but otherwise regular sample rate
  D1 = read.myacc.csv(rmc.file = testfile[1], rmc.nrow = 20, rmc.dec = ".",
                      rmc.firstrow.acc = 11, rmc.firstrow.header = 1,
                      rmc.col.acc = c(1, 3, 4), rmc.col.temp = 5, rmc.col.time = 2,
                      rmc.unit.acc = "g", rmc.unit.temp = "C",
                      rmc.format.time = "%Y-%m-%d %H:%M:%OS",
                      rmc.origin = "1970-01-01",
                      desiredtz = "Europe/London",
                      rmc.sf = sf,
                      rmc.headername.sf = "sample_rate",
                      rmc.headername.sn = "serial_number",
                      rmc.headername.recordingid = "ID",
                      rmc.check4timegaps = TRUE, rmc.doresample = FALSE)
  expect_that(nrow(D1$data), equals(170)) # because data expands with 5 seconds that are now imputed
  expect_that(ncol(D1$data), equals(5))
  
  #------------------------
  # Test 2 - time gaps and irregular sample rate
  D2 = read.myacc.csv(rmc.file = testfile[1], rmc.nrow = 20, rmc.dec = ".",
                      rmc.firstrow.acc = 11, rmc.firstrow.header = 1,
                      rmc.col.acc = c(1, 3, 4), rmc.col.temp = 5, rmc.col.time = 2,
                      rmc.unit.acc = "g", rmc.unit.temp = "C",
                      rmc.format.time = "%Y-%m-%d %H:%M:%OS",
                      rmc.origin = "1970-01-01",
                      desiredtz = "Europe/London",
                      rmc.sf = sf,
                      rmc.headername.sf = "sample_rate",
                      rmc.headername.sn = "serial_number",
                      rmc.headername.recordingid = "ID",
                      rmc.check4timegaps = TRUE, rmc.doresample = TRUE)
  expect_that(nrow(D2$data), equals(170)) # because data expands with 5 seconds that are now imputed
  expect_that(ncol(D2$data), equals(5))
  
  
  for (i in 1:length(testfile)) {
    expect_true(file.exists(testfile[i]))
    if (file.exists(testfile[i])) file.remove(testfile[i])
  }
  options(old_options)
})