library(GGIR)
context("read.myacc.csv")
test_that("read.myacc.csv can read a variety of csv file formats", {
  skip_on_cran()
  
  # create test files
  N = 30
  sf = 30
  timestamps = as.POSIXlt(Sys.time() + ((0:(N - 1))/sf), origin = "1970-1-1", tz = "Europe/London")
  testfile = matrix("", 6, 1)
  set.seed(100)
  accx = rnorm(N)
  set.seed(200)
  accy = rnorm(N)
  set.seed(300)
  accz = rnorm(N)
  set.seed(400)
  temp = rnorm(N)
  wear = c(rep(TRUE,N/3),rep(FALSE,N/6),rep(TRUE,N/3),rep(TRUE,N/6))
  # create test file 1: No header, with temperature, with time
  S1 = data.frame(x = accx, time = timestamps, 
                  y = accy, z = accz, temp = temp + 20, 
                  stringsAsFactors = TRUE)
  testfile[1] = "testcsv1.csv"
  write.csv(S1, file = testfile[1], row.names = FALSE)
  # create test file 2: No header, without temperature, with time
  S2 = data.frame(x = accx, time = timestamps, y = accy, z = accz, stringsAsFactors = TRUE)
  testfile[2] = "testcsv2.csv"
  write.csv(S2, file = testfile[2], row.names = FALSE)
  # create test file 3: No header, without temperature, without time, with wear channel
  S3 = data.frame(x = accx, y = accy, z = accz, wear = wear, stringsAsFactors = TRUE)
  testfile[3] = "testcsv3.csv"
  write.csv(S3, file = testfile[3], row.names = FALSE)
  # create test file 4: With header, with temperature, with time
  S4 = as.matrix(data.frame(x = accx, time = timestamps, y = accy,
                            z = accz, temp = temp + 20, stringsAsFactors = TRUE))
  hd_NR = 10
  hd = matrix("", hd_NR + 1, ncol(S4))
  hd[1, 1:2] = c("ID","12345")
  hd[2, 1:2] = c("sample_rate","30")
  hd[3, 1:2] = c("serial_number","30")
  hd[4, 1:2] = c("bit","8")
  hd[5, 1:2] = c("dynamic_range","6")
  S4 = rbind(hd, S4)
  S4[hd_NR + 1,] = colnames(S4)
  colnames(S4) = NULL
  testfile[4] = "testcsv4.csv"
  write.table(S4, file = testfile[4], col.names = FALSE, row.names = FALSE)
  
  
  # create test file 5
  # With header, with temperature, with time, but bit-value acceleration unit
  bits = 8
  set.seed(100)
  xb = sample(x = 1:(2^bits),size = N,replace = TRUE)
  set.seed(200)
  yb = sample(x = 1:(2^bits),size = N,replace = TRUE)
  set.seed(300)
  zb = sample(x = 1:(2^bits),size = N,replace = TRUE)
  set.seed(400)
  temp3 = rnorm(N)
  S5 = as.matrix(data.frame(x = xb, time = timestamps, y = yb,
                            z = zb, temp = temp3 + 20, stringsAsFactors = TRUE))
  hd_NR = 10
  hd = matrix("", hd_NR + 1, ncol(S5))
  hd[1, 1:2] = c("ID", "12345")
  hd[2, 1:2] = c("sample_rate", "30")
  hd[3, 1:2] = c("serial_number", "30")
  hd[4, 1:2] = c("bit","8")
  hd[5, 1:2] = c("dynamic_range","6")
  S5 = rbind(hd, S5)
  S5[hd_NR + 1, ] = colnames(S5)
  colnames(S5) = NULL
  testfile[5] = "testcsv5.csv"
  write.table(S5, file = testfile[5], col.names = FALSE, row.names = FALSE)
  
  # create test file 7
  # With header, with temperature, with time, but bit-value acceleration unit, and gap in time
  bits = 8
  timestamps_gap = timestamps
  timestamps_gap[10:length(timestamps_gap)] = timestamps_gap[10:length(timestamps_gap)] + 5 # add gap of 5 seconds
  N_withgap = length(timestamps_gap)
  set.seed(100)
  xb = sample(x = 1:(2^bits), size = N_withgap, replace = TRUE)
  set.seed(200)
  yb = sample(x = 1:(2^bits), size = N_withgap, replace = TRUE)
  set.seed(300)
  zb = sample(x = 1:(2^bits), size = N_withgap, replace = TRUE)
  set.seed(400)
  temp2 = rnorm(N_withgap)
  S7 = as.matrix(data.frame(x = xb, time = timestamps_gap, y = yb, z = zb, 
                            temp = temp2 + 20, stringsAsFactors = TRUE))
  hd_NR = 10
  hd = matrix("",hd_NR + 1,ncol(S5))
  hd[1, 1:2] = c("ID", "12345")
  hd[2, 1:2] = c("sample_rate", "30")
  hd[3, 1:2] = c("serial_number", "30")
  hd[4, 1:2] = c("bit", "8")
  hd[5, 1:2] = c("dynamic_range", "6")
  S7 = rbind(hd,S7)
  S7[hd_NR + 1,] = colnames(S7)
  colnames(S7) = NULL
  testfile[6] = "testcsv7.csv"
  write.table(S7, file = testfile[6], col.names = FALSE, row.names = FALSE)
  
  #------------------------
  # Try to read each of these files
  D1 = read.myacc.csv(rmc.file = testfile[1], rmc.nrow = 20, rmc.dec = ".",
                      rmc.firstrow.acc = 1, rmc.firstrow.header = c(),
                      rmc.col.acc = c(1,3,4), rmc.col.temp = 5, rmc.col.time = 2,
                      rmc.unit.acc = "g", rmc.unit.temp = "C", rmc.format.time = "%Y-%m-%d %H:%M:%OS",
                      rmc.origin = "1970-01-01",
                      rmc.desiredtz = "Europe/London", rmc.sf = 100,
                      rmc.headername.sf = "sample_frequency",
                      rmc.headername.sn = "serial_number",
                      rmc.headername.recordingid = "ID")
  expect_that(nrow(D1$data),equals(20))
  expect_that(ncol(D1$data),equals(5))
  expect_that(D1$header,equals("no header"))
  D2 = read.myacc.csv(rmc.file = testfile[2], rmc.nrow = 20, rmc.dec = ".",
                      rmc.firstrow.acc = 1, rmc.firstrow.header = c(),
                      rmc.col.acc = c(1,3,4), rmc.col.temp = c(),
                      rmc.col.time = 2,
                      rmc.unit.acc = "g", rmc.unit.temp = "C",
                      rmc.format.time = "%Y-%m-%d %H:%M:%OS",
                      rmc.origin = "1970-01-01",
                      rmc.desiredtz = "Europe/London", 
                      rmc.sf = 100,
                      rmc.headername.sf = "sample_frequency",
                      rmc.headername.sn = "serial_number",
                      rmc.headername.recordingid = "ID")
  expect_that(nrow(D2$data),equals(20))
  expect_that(ncol(D2$data),equals(4))
  expect_that(D2$header,equals("no header"))
  D3 = read.myacc.csv(rmc.file = testfile[3], rmc.nrow = 20, 
                      rmc.dec = ".",
                      rmc.firstrow.acc = 1, rmc.firstrow.header = c(),
                      rmc.col.acc = 1:3, rmc.col.temp = c(), rmc.col.time = c(),
                      rmc.unit.acc = "g", rmc.unit.temp = "C", 
                      rmc.format.time = "%Y-%m-%d %H:%M:%OS",
                      rmc.origin = "1970-01-01",
                      rmc.desiredtz = "Europe/London", rmc.sf = 100,
                      rmc.headername.sf = "sample_frequency",
                      rmc.headername.sn = "serial_number",
                      rmc.headername.recordingid = "ID",
                      rmc.col.wear = 4)
  expect_that(nrow(D3$data), equals(20))
  expect_true(D3$data[1, 4])
  expect_that(ncol(D3$data), equals(4))
  expect_that(D3$header, equals("no header"))
  D4 = read.myacc.csv(rmc.file = testfile[4], rmc.nrow = 20, rmc.dec = ".",
                      rmc.firstrow.acc = 11, rmc.firstrow.header = 1,
                      rmc.col.acc = c(1, 3, 4), rmc.col.temp = 5, rmc.col.time = 2,
                      rmc.unit.acc = "g", rmc.unit.temp = "C",
                      rmc.format.time = "%Y-%m-%d %H:%M:%OS",
                      rmc.origin = "1970-01-01",
                      rmc.desiredtz = "Europe/London", 
                      rmc.sf = 100,
                      rmc.headername.sf = "sample_frequency",
                      rmc.headername.sn = "serial_number",
                      rmc.headername.recordingid = "ID")
  expect_that(nrow(D4$data), equals(20))
  expect_that(ncol(D4$data), equals(5))
  expect_that(nrow(D4$header), equals(5))
  expect_that(ncol(D4$header), equals(1))

  D5 = read.myacc.csv(rmc.file = testfile[5], rmc.nrow = 20, rmc.dec = ".",
                      rmc.firstrow.acc = 11, rmc.firstrow.header = 1,
                      rmc.col.acc = c(1,3,4), rmc.col.temp = 5, 
                      rmc.col.time = 2,
                      rmc.unit.acc = "bit", rmc.unit.temp = "C", 
                      rmc.format.time = "%Y-%m-%d %H:%M:%OS",
                      rmc.origin = "1970-01-01",
                      rmc.desiredtz = "Europe/London", rmc.sf = 100,
                      rmc.headername.sf = "sample_rate",
                      rmc.headername.sn = "serial_number",
                      rmc.headername.recordingid = "ID", rmc.bit = "bit",
                      rmc.dynamic_range = "dynamic_range",
                      rmc.header.structure = c())
  expect_that(nrow(D5$data),equals(20))
  expect_that(ncol(D5$data),equals(5))
  expect_that(nrow(D5$header),equals(5))
  expect_that(ncol(D5$header),equals(1))
  D7 = read.myacc.csv(rmc.file = testfile[6], rmc.nrow = 20, rmc.dec = ".",
                      rmc.firstrow.acc = 11, rmc.firstrow.header = 1,
                      rmc.col.acc = c(1,3,4), rmc.col.temp = 5, 
                      rmc.col.time = 2,
                      rmc.unit.acc = "bit", rmc.unit.temp = "C",
                      rmc.format.time = "%Y-%m-%d %H:%M:%OS",
                      rmc.origin = "1970-01-01",
                      rmc.desiredtz = "Europe/London", rmc.sf = 100,
                      rmc.headername.sf = "sample_rate",
                      rmc.headername.sn = "serial_number",
                      rmc.headername.recordingid = "ID", 
                      rmc.bit = "bit", rmc.dynamic_range = "dynamic_range",
                      rmc.header.structure = c(), rmc.check4timegaps = TRUE)
  
  expect_that(round(mean(D7$data[,2]), digits = 3), equals(0.955))
  expect_that(round(mean(D7$data[,3]), digits = 2), equals(0.27))
  expect_that(round(mean(D7$data[,4]), digits = 2), equals(-0.35))
  expect_that(round(mean(D7$data[,5]), digits = 1), equals(20))
  expect_that(round(D7$data[2,2], digits = 3), equals(5.578))
  expect_that(ncol(D7$data), equals(5))
  expect_that(nrow(D7$header), equals(5))
  expect_that(ncol(D7$header), equals(1))
    
  for (i in 1:length(testfile)) {
    expect_true(file.exists(testfile[i]))
    if (file.exists(testfile[i])) file.remove(testfile[i])
  }
})