create_test_acc_csv = function(sf=3,Nmin=2000,storagelocation=c()) {
  # function to create a test Actigraph csv file needed for testing GGIR
  # adds variation in angle to enable autocalibration
  # adds some activity periods
  # rm(list=ls())
  # sf = 3
  # Nmin = 2000
  # storagelocation = c()
  #===============================================
  gensequence = function(t0,t1) {
    x = round(t0*3600*sf):(round(t1*3600*sf)-1)
    return(x)
  }
  genrotationmatrix = function(phi) {   # create rotation matrix
    Rx = Ry = matrix(0,3,3) # no z rotation needed, all we need is to populate sphere on all sides
    Ry[1,] = c(cos(phi),0,sin(phi))
    Ry[2,] = c(0,1,0)
    Ry[3,] = c(-sin(phi),0,cos(phi))
    Rx[1,] = c(cos(phi),-sin(phi),0)
    Rx[2,] = c(sin(phi),cos(phi),0)
    Rx[3,] = c(0,0,1)
    invisible(list(Rx=Rx,Ry=Ry))
  }
  #==================================
  
  if (length(storagelocation) == 0) storagelocation = getwd()
  if (Nmin < 2000) Nmin = 2000 # only make this file for tests with at least 2k minutes of data
  header = c(paste0("------------ Data File Created By ActiGraph GT3X+ ActiLife v6.13.3 Firmware v1.8.0 date format M/d/yyyy at ",sf," Hz  Filter Normal -----------"),
             "Serial Number: MOS2D12345678",
             "Start Time 08:55:30",
             "Start Date 6/23/2016",
             "Epoch Period (hh:mm:ss) 00:00:00",
             "Download Time 10:28:51",
             "Download Date 6/25/2016",
             "Current Memory Address: 0",
             "Current Battery Voltage: 3.97     Mode = 12",
             "--------------------------------------------------")
  variablenames = c("Accelerometer X","Accelerometer Y","Accelerometer Z")
  Nrows = Nmin*sf*60
  # default values, including some calibration error such that auto-calibration can do its work
  accx = 1.015
  accy = 0.005
  accz = -0.005
  testdata = matrix(0,Nrows,3)
  # add enough noise to not trigger the non-wear detection
  set.seed(300)
  testdata[,1] = accx + round(rnorm(n = Nrows,mean = 0.03,sd=0.02),digits=4) #gravity
  set.seed(400)
  testdata[,2] = accy + round(rnorm(n = Nrows,mean = 0.03,sd=0.1),digits=4) #no gravity
  Nsamplesinday = 24*3600*sf
  set.seed(500)
  testdata[,3] = accz + round(rnorm(n = Nrows,mean = 0.03,sd=0.02),digits=4) + #no gravity
                  sin(((1:Nrows)/((Nrows)/(Nsamplesinday/0.1))))* 0.05 +
                  sin(((1:Nrows)/((Nrows)/(Nsamplesinday/2))))* 0.1
  
  
  
  
  #======================================
  # insert sleep blocks from the 15th till the 21st hour
  sleepL1 = seq((12*3600*sf)+1,Nrows,by=24*3600*sf) #first sleep bout
  sleepL2 = rep(sleepL1,each=2.5*3600*sf) # 2.5 hours
  sleepL3 = sleepL2 + rep(0:((length(sleepL2)/length(sleepL1))-1), time = length(sleepL1))
  sleepS1 = seq((15*3600*sf)+1,Nrows,by=24*3600*sf) #second sleep bout
  sleepS2 = rep(sleepS1,each=3*3600*sf) # 3 hours
  sleepS3 = sleepS2 + rep(0:((length(sleepS2)/length(sleepS1))-1), time = length(sleepS1))
  # ACC = rep(0,Nrows) #empty ACC pattern
  sleep_periods = sort(unique(c(sleepL3,sleepS3)))
  set.seed(300)
  testdata[sleep_periods,1] = accx + round(rnorm(n = length(sleep_periods),mean = 0,sd=0.015) ,digits = 4)
  set.seed(400)
  testdata[sleep_periods,2] = accy + round(rnorm(n = length(sleep_periods),mean = 0,sd=0.015) ,digits = 4)
  set.seed(500)
  testdata[sleep_periods,3] = accz + round(rnorm(n = length(sleep_periods),mean = 0,sd=0.015) ,digits = 4)
  # add some rotations to the acceleormeter, to avoid non-wear detection
  for (j in 1:(Nrows/(sf*3600))) { # loop to add angle changes to every night
    t0v = seq(12+((j-1)*24),22+((j-1)*24),by=0.25) # start times of angle positions
    phiv = 1:length(t0v)
    phiv = scale(x = phiv,center = 0,scale = diff(range(phiv))/(2*pi))  # rescale such that angle goes 180 degrees
    gammav = scale(x = phiv,center = 0,scale = diff(range(phiv))/(2*pi))  # rescale such that angle goes 180 degrees
    for (i in 1:length(t0v)) {
      B = gensequence(t0=t0v[i],t1=t0v[i]+0.245) #long activity starts at the 3rd hour of every 24 hours
      Rm_phi = genrotationmatrix(phiv[i])
      Rm_gamma = genrotationmatrix(gammav[i])
      Ry = Rm_phi$Ry; Rx = Rm_gamma$Rx
      B = B[which(B < nrow(testdata))]
      if (length(B) > 0) {
        testdata[B,1] = accx # set constant such that it is detected as a period without movement
        testdata[B,2] = accy
        testdata[B,3] = accz
        testdata[B,] = t(Ry %*% t(testdata[B,]))
        testdata[B,] = t(Rx %*% t(testdata[B,]))
      }
    }
  }
  #============================================================
  # insert 2 minute blocks of activity at the beginning of every 15 minutes
  actP1 = seq(1,Nrows,by=0.25*3600*sf) # starts at the 1st minutes and then everry 15 minutes
  actP2 = rep(actP1,each=2*60*sf) # and it lasts 2 minutes
  actP3 = actP2 + rep(0:((length(actP2)/length(actP1))-1), time = length(actP1))
  ACC = rep(0,Nrows) #empty ACC pattern
  set.seed(300)
  ACC[actP3] = 0.08 + round(rnorm(n = length(actP3),mean = 0,sd=0.01) ,digits = 4) # add 0.08g + 10 mg noise
  testdata[,1] = testdata[,1] + ACC
  #======================================
  # insert 2 hour blocks of accelermeter non-wear time
  nw1 = seq((300*sf)+1,Nrows,by=24*3600*sf) #non wear starts at the 5th minute of every 24 hours
  nw2 =  rep(nw1,each=7200*sf) # and lasts 2 hours
  nw3 = nw2 + rep(0:((length(nw2)/length(nw1))-1), time = length(nw1))
  testdata[nw3,1] = accx # constant values to trigger non-wear detection
  testdata[nw3,2] = accy
  testdata[nw3,3] = accz
  #======================================
  # insert 20 and 4 minute blocks of activity
  actL1 = seq((3*3600*sf)+1,Nrows,by=24*3600*sf) #long activity starts at the 3rd hour of every 24 hours
  actL2 = rep(actL1,each=20*60*sf) # and lasts 20 minutes
  actL3 = actL2 + rep(0:((length(actL2)/length(actL1))-1), time = length(actL1))
  actS1 = seq((3.5*3600*sf)+1,Nrows,by=24*3600*sf) #short activity starts at the 3.5th hour of every 24 hours
  actS2 = rep(actS1,each=4*60*sf) # and lasts 4 minutes
  actS3 = actS2 + rep(0:((length(actS2)/length(actS1))-1), time = length(actS1))
  ACC = rep(0,Nrows) #empty ACC pattern
  act_periods = sort(unique(c(actL3,actS3)))
  set.seed(300)
  ACC[act_periods] = 0.5 + round(rnorm(n = length(act_periods),mean = 0,sd=0.04) ,digits = 4) # add 0.5g + 20 mg noise
  testdata[,1] = testdata[,1] + ACC
  #======================================
  # insert variations in angle to facilitate autocalibration
  t0v = seq(4.5,9,by=1/120) # start times of angle positions, and 30 seconds each
  phiv = 1:length(t0v)
  phiv = scale(x = phiv,center = 0,scale = diff(range(phiv))/(20*pi))  # rescale such that angle go 4 times 360 degrees
  gammav = scale(x = phiv,center = 0,scale = diff(range(phiv))/(2*pi))  # rescale such that angle goes 180 degrees
  for (i in 1:length(t0v)) {
    B = gensequence(t0=t0v[i],t1=t0v[i]+0.24) #long activity starts at the 3rd hour of every 24 hours
    Rm_phi = genrotationmatrix(phiv[i])
    Rm_gamma = genrotationmatrix(gammav[i])
    Ry = Rm_phi$Ry; Rx = Rm_gamma$Rx
    testdata[B,1] = accx # set constant such that it is detected as a period without movement
    testdata[B,2] = accy
    testdata[B,3] = accz
    testdata[B,] = t(Ry %*% t(testdata[B,]))
    testdata[B,] = t(Rx %*% t(testdata[B,]))
  }
  testfile = matrix(" ",Nrows+11,3)
  testfile[1:10,1] = header
  testfile[11,1:3] = variablenames
  testfile[12:(Nrows+11),1:3] = testdata
  write.table(testfile,file=paste0(storagelocation,"/123A_testaccfile.csv"),row.names = FALSE,col.names = FALSE,sep=",",fileEncoding="UTF-8")
}
