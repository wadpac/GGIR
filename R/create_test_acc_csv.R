create_test_acc_csv = function(sf = 3, Nmin = 2000, storagelocation = c(),
                               start_time = NULL, starts_at_midnight = FALSE) {
  # function to create a test Actigraph csv file needed for testing GGIR
  # adds variation in angle to enable autocalibration
  # adds some activity periods
  # rm(list=ls())
  # sf = 3
  # Nmin = 2000
  # storagelocation = c()
  #===============================================
  gensequence = function(t0,t1) {
    x = round(t0 * 3600 * sf):(round(t1 * 3600 * sf) - 1)
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
    invisible(list(Rx = Rx, Ry = Ry))
  }
  
  generate_repeated_timestamp_blocks = function(first_onset, block_duration, period_length, sf, max_timestamp) {
    # Generate a series of timestamp blocks: the first one at first_onset, then one more each period. 
    if (max_timestamp <= first_onset) {
      return(c())
    }
    block_onsets = seq(first_onset + 1, max_timestamp, by = period_length) # one block onset for each period
    block_timestamps = rep(block_onsets, each = block_duration)
    block_timestamps = block_timestamps + rep(0:(block_duration - 1), times = length(block_onsets))
    
    # Make sure the entire duration of the last block fits within the time period for which we have data.
    # Trim it if necessary.
    block_timestamps = block_timestamps[which(block_timestamps <= max_timestamp)]
    
    return(block_timestamps)
  }
  
  generate_daily_timestamp_blocks = function(first_onset, block_duration, sf, max_timestamp) {
    # Generate a series of timestamp blocks: the first one at first_onset, then one more every 24 hours. 
    day_length = 24*3600*sf
    block_timestamps = generate_repeated_timestamp_blocks(first_onset, block_duration, day_length, sf, max_timestamp)
    return(block_timestamps)
  }
  
  #==================================
  
  if (length(storagelocation) == 0) storagelocation = getwd()
  if (Nmin < 720) Nmin = 720 # only make this file for tests with at least 12 hours of data
  if (length(start_time) == 0 || start_time == "") {
    if (starts_at_midnight) {
      start_time = "00:00:00"
    } else {
      start_time = "08:55:30"
    }
  } else {
    if (is.na(as.POSIXct(start_time, format = "%H:%M:%S"))) {
      stop(paste0("start_time \"", start_time, "\" not in the right format, should be \"%H:%M:%S\", ex. \"08:55:30\""))
    }
  }
  header = c(paste0("------------ Data File Created By ActiGraph GT3X+ ActiLife v6.13.3 Firmware v1.8.0 date format M/d/yyyy at ",sf," Hz  Filter Normal -----------"),
             "Serial Number: MOS2D12345678",
             paste0("Start Time ", start_time),
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
  testdata[,1] = accx + round(rnorm(n = Nrows, mean = 0.03, sd = 0.02), digits = 4) #gravity
  set.seed(400)
  testdata[,2] = accy + round(rnorm(n = Nrows, mean = 0.03, sd = 0.1), digits = 4) #no gravity
  Nsamplesinday = 24*3600*sf
  set.seed(500)
  testdata[,3] = accz + round(rnorm(n = Nrows, mean = 0.03, sd = 0.02), digits = 4) + #no gravity
    sin(((1:Nrows)/((Nrows)/(Nsamplesinday/0.1)))) * 0.05 +
    sin(((1:Nrows)/((Nrows)/(Nsamplesinday/2)))) * 0.1
  
  #======================================
  # generate 2.5-hour sleep blocks: at 12 hours from the start of the recording, and then every 24 hours
  sleep1 = generate_daily_timestamp_blocks(first_onset = 12 * 3600 * sf,
                                           block_duration = 2.5 * 3600 * sf, sf = sf,
                                           max_timestamp = Nrows)
  
  # generate 3-hour sleep blocks: at 15 hours from the start of the recording, and then every 24 hours
  sleep2 = generate_daily_timestamp_blocks(first_onset = 15 * 3600 * sf,
                                           block_duration = 3 * 3600 * sf,
                                           sf = sf, max_timestamp = Nrows)
  
  sleep_periods = sort(unique(c(sleep1,sleep2)))
  
  set.seed(300)
  testdata[sleep_periods, 1] = accx + round(rnorm(n = length(sleep_periods),mean = 0, sd = 0.015), digits = 4)
  set.seed(400)
  testdata[sleep_periods, 2] = accy + round(rnorm(n = length(sleep_periods),mean = 0, sd = 0.015), digits = 4)
  set.seed(500)
  testdata[sleep_periods, 3] = accz + round(rnorm(n = length(sleep_periods),mean = 0, sd = 0.015), digits = 4)
  
  # add some rotations to the acceleormeter, to avoid non-wear detection
  for (j in 1:(Nrows/(sf*3600))) { # loop to add angle changes to every night
    t0v = seq(12 + ((j - 1) * 24), 22 + ((j - 1) * 24), by = 0.25) # start times of angle positions
    phiv = 1:length(t0v)
    phiv = scale(x = phiv,center = 0,scale = diff(range(phiv))/(2*pi))  # rescale such that angle goes 180 degrees
    gammav = scale(x = phiv,center = 0,scale = diff(range(phiv))/(2*pi))  # rescale such that angle goes 180 degrees
    for (i in 1:length(t0v)) {
      B = gensequence(t0 = t0v[i], t1 = t0v[i] + 0.245) #long activity starts at the 3rd hour of every 24 hours
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
  # insert 2-minute blocks of activity at the beginning of every 15 minutes
  act2_15 = generate_repeated_timestamp_blocks(first_onset = 0, block_duration = 2 * 60 * sf,
                                               period_length = 15 * 60 * sf,
                                               sf = sf,
                                               max_timestamp = Nrows)
  
  ACC = rep(0,Nrows) #empty ACC pattern
  set.seed(300)
  ACC[act2_15] = 0.08 + round(rnorm(n = length(act2_15),
                                    mean = 0, sd = 0.01) , digits = 4) # add 0.08g + 10 mg noise
  testdata[,1] = testdata[,1] + ACC
  #======================================
  # generate one 2-hour block of non-wear starting at the 5th minute of every 24 hours
  nw = generate_daily_timestamp_blocks(first_onset = 5 * 60 * sf,
                                       block_duration = 2 * 3600 * sf,
                                       sf = sf, max_timestamp = Nrows)
  
  testdata[nw,1] = accx # constant values to trigger non-wear detection
  testdata[nw,2] = accy
  testdata[nw,3] = accz
  #======================================
  # insert 20 and 4 minute blocks of activity
  
  # generate one 20-minute block of activity starting at the 3rd hour of every 24 hours
  act1 = generate_daily_timestamp_blocks(first_onset = 3 * 3600 * sf,
                                         block_duration = 20 * 60 * sf,
                                         sf = sf, max_timestamp = Nrows)
  
  # generate one 4-minute block of activity starting at the 3.5th hour of every 24 hours
  act2 = generate_daily_timestamp_blocks(first_onset = 3.5 * 3600 * sf,
                                         block_duration = 4 * 60 * sf,
                                         sf = sf, max_timestamp = Nrows)
  
  act_periods = sort(unique(c(act1,act2)))
  
  ACC = rep(0,Nrows) #empty ACC pattern
  set.seed(300)
  ACC[act_periods] = 0.5 + round(rnorm(n = length(act_periods),
                                       mean = 0, sd = 0.04), digits = 4) # add 0.5g + 20 mg noise
  testdata[,1] = testdata[,1] + ACC
  #======================================
  # insert variations in angle to facilitate autocalibration
  t0v = seq(4.5, 9, by = 1/120) # start times of angle positions, and 30 seconds each
  phiv = 1:length(t0v)
  phiv = scale(x = phiv, center = 0, scale = diff(range(phiv)) / (20 * pi))  # rescale such that angle go 4 times 360 degrees
  gammav = scale(x = phiv, center = 0, scale = diff(range(phiv)) / (2 * pi))  # rescale such that angle goes 180 degrees
  for (i in 1:length(t0v)) {
    B = gensequence(t0 = t0v[i], t1 = t0v[i] + 0.24) #long activity starts at the 3rd hour of every 24 hours
    Rm_phi = genrotationmatrix(phiv[i])
    Rm_gamma = genrotationmatrix(gammav[i])
    Ry = Rm_phi$Ry; Rx = Rm_gamma$Rx
    testdata[B,1] = accx # set constant such that it is detected as a period without movement
    testdata[B,2] = accy
    testdata[B,3] = accz
    testdata[B,] = t(Ry %*% t(testdata[B,]))
    testdata[B,] = t(Rx %*% t(testdata[B,]))
  }
  testfile = matrix(" ", Nrows + 11, 3)
  testfile[1:10,1] = header
  testfile[11,1:3] = variablenames
  testfile[12:(Nrows + 11), 1:3] = testdata
  data.table::fwrite(x = testfile, file = paste0(storagelocation,"/123A_testaccfile.csv"),
              row.names = FALSE, col.names = FALSE)
}
