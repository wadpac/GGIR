library(GGIR)
context("read.myacc.csv")
test_that("read.myacc.csv can read a variety of csv file formats", {
  
  # create test files
  N = 30
  sf = 30
  timestamps = as.POSIXlt(Sys.time()+((0:(N-1))/sf),origin="1970-1-1",tz = "Europe/London")
  testfile = matrix("",4,1)
  # create test file 1: No header, with temperature, with time
  S1 = data.frame(x=rnorm(N), time=timestamps,y=rnorm(N),z=rnorm(N),temp=rnorm(N)+20)
  testfile[1] = "testcsv1.csv"
  write.csv(S1, file= testfile[1], row.names = FALSE)
  # create test file 2: No header, without temperature, with time
  S2 = data.frame(x=rnorm(N), time=timestamps,y=rnorm(N),z=rnorm(N))
  testfile[2] = "testcsv2.csv"
  write.csv(S2, file= testfile[2], row.names = FALSE)
  # create test file 3: No header, without temperature, without time
  S3 = data.frame(x=rnorm(N), y=rnorm(N), z=rnorm(N))
  testfile[3] = "testcsv3.csv"
  write.csv(S3, file= testfile[3], row.names = FALSE)
  # create test file 4: With header, with temperature, with time
  S4 = as.matrix(data.frame(x=rnorm(N), time=timestamps,y=rnorm(N),z=rnorm(N),temp=rnorm(N)+20))
  hd_NR = 10
  hd = matrix("",hd_NR + 1,ncol(S4))
  hd[1,1:2] = c("ID","12345")
  hd[2,1:2] = c("sample_rate","30")
  hd[3,1:2] = c("serial_number","30")
  hd[4,1:2] = c("bit","8")
  hd[5,1:2] = c("dynamic_range","6")
  S4 = rbind(hd,S4)
  S4[hd_NR+1,] = colnames(S4)
  colnames(S4) = NULL
  testfile[4] = "~/testcsv4.csv"
  write.table(S4, file= testfile[4], col.names=FALSE, row.names = FALSE)
  #------------------------
  # Try to read each of these files
  D1 = read.myacc.csv(file=testfile[1], nrow=20, dec=".",
                      firstraw.acc = 1, firstrow.header=c(),
                      col.acc = c(1,3,4), col.temp = 5, col.time=2,
                      unit.acc = "g", unit.temp = "C", format.time = "%Y-%m-%d %H:%M:%OS",
                      origin = "1970-01-01",
                      desiredtz = "Europe/London", samplefrequency = 100,
                      headername.samplefrequency = "sample_frequency",
                      headername.deviceserialnumber = "serial_number",
                      headername.recordingid = "ID")
  expect_that(nrow(D1$data),equals(20))
  expect_that(ncol(D1$data),equals(5))
  expect_that(D1$header,equals("no header"))
  D2 = read.myacc.csv(file=testfile[2], nrow=20, dec=".",
                      firstraw.acc = 1, firstrow.header=c(),
                      col.acc = c(1,3,4), col.temp = c(), col.time=2,
                      unit.acc = "g", unit.temp = "C", format.time = "%Y-%m-%d %H:%M:%OS",
                      origin = "1970-01-01",
                      desiredtz = "Europe/London", samplefrequency = 100,
                      headername.samplefrequency = "sample_frequency",
                      headername.deviceserialnumber = "serial_number",
                      headername.recordingid = "ID")
  expect_that(nrow(D2$data),equals(20))
  expect_that(ncol(D2$data),equals(4))
  expect_that(D2$header,equals("no header"))
  D3 = read.myacc.csv(file=testfile[3], nrow=20, dec=".",
                      firstraw.acc = 1, firstrow.header=c(),
                      col.acc = 1:3, col.temp = c(), col.time=c(),
                      unit.acc = "g", unit.temp = "C", format.time = "%Y-%m-%d %H:%M:%OS",
                      origin = "1970-01-01",
                      desiredtz = "Europe/London", samplefrequency = 100,
                      headername.samplefrequency = "sample_frequency",
                      headername.deviceserialnumber = "serial_number",
                      headername.recordingid = "ID")
  expect_that(nrow(D3$data),equals(20))
  expect_that(ncol(D3$data),equals(3))
  expect_that(D3$header,equals("no header"))
  D4 = read.myacc.csv(file=testfile[4], nrow=20, dec=".",
                      firstraw.acc = 11, firstrow.header=1,
                      col.acc = c(1,3,4), col.temp = 5, col.time=2,
                      unit.acc = "g", unit.temp = "C", format.time = "%Y-%m-%d %H:%M:%OS",
                      origin = "1970-01-01",
                      desiredtz = "Europe/London", samplefrequency = 100,
                      headername.samplefrequency = "sample_frequency",
                      headername.deviceserialnumber = "serial_number",
                      headername.recordingid = "ID")
  
  expect_that(nrow(D4$data),equals(20))
  expect_that(ncol(D4$data),equals(5))
  expect_that(nrow(D4$header),equals(5))
  expect_that(ncol(D4$header),equals(1))
  
  for (i in 1:length(testfile)) {
    expect_true(file.exists(testfile[i]))
    if (file.exists(testfile[i])) file.remove(testfile[i])
  }
})