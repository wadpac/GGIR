g.getmeta = function(datafile,desiredtz = "",windowsizes = c(5,900,3600),
                     daylimit=FALSE,offset=c(0,0,0),scale=c(1,1,1),tempoffset = c(0,0,0),
                     do.bfen=FALSE, do.enmo=TRUE, do.lfenmo=FALSE,
                     do.en=FALSE, do.hfen=FALSE,
                     do.hfenplus=FALSE, do.mad=FALSE,
                     do.anglex=FALSE, do.angley=FALSE, do.anglez=FALSE,
                     do.roll_med_acc_x=FALSE, do.roll_med_acc_y=FALSE, do.roll_med_acc_z=FALSE,
                     do.dev_roll_med_acc_x=FALSE, do.dev_roll_med_acc_y=FALSE,
                     do.dev_roll_med_acc_z=FALSE, do.enmoa=FALSE,
                     do.lfen=FALSE, do.lfx=FALSE, do.lfy=FALSE, do.lfz=FALSE,
                     do.hfx=FALSE, do.hfy=FALSE, do.hfz=FALSE,
                     do.bfx=FALSE, do.bfy=FALSE, do.bfz=FALSE,
                     lb = 0.2, hb = 15,  n = 4,meantempcal=c(), chunksize=c(), selectdaysfile=c(),
                     dayborder=0,dynrange=c(),configtz=c(),myfun=c(),...) {
  #get input variables
  input = list(...)
  if (length(input) > 0) {
    for (i in 1:length(names(input))) {
      txt = paste(names(input)[i],"=",input[i],sep="")
      if (class(unlist(input[i])) == "character") {
        txt = paste(names(input)[i],"='",unlist(input[i]),"'",sep="")
      }
      eval(parse(text=txt))
    }
  }
  if (length(which(ls() == "outputdir")) != 0) outputdir = input$outputdir
  if (length(which(ls() == "outputfolder")) != 0) outputfolder = input$outputfolder
  if (length(which(ls() == "rmc.dec")) == 0) rmc.dec="."
  if (length(which(ls() == "rmc.firstrow.acc")) == 0) rmc.firstrow.acc = c()
  if (length(which(ls() == "rmc.firstrow.header")) == 0) rmc.firstrow.header=c()
  if (length(which(ls() == "rmc.header.length")) == 0)  rmc.header.length= c()
  if (length(which(ls() == "rmc.col.acc")) == 0) rmc.col.acc = 1:3
  if (length(which(ls() == "rmc.col.temp")) == 0) rmc.col.temp = c()
  if (length(which(ls() == "rmc.col.time")) == 0) rmc.col.time=c()
  if (length(which(ls() == "rmc.unit.acc")) == 0) rmc.unit.acc = "g"
  if (length(which(ls() == "rmc.unit.temp")) == 0) rmc.unit.temp = "C"
  if (length(which(ls() == "rmc.unit.time")) == 0) rmc.unit.time = "POSIX"
  if (length(which(ls() == "rmc.format.time")) == 0) rmc.format.time = "%Y-%m-%d %H:%M:%OS"
  if (length(which(ls() == "rmc.bitrate")) == 0) rmc.bitrate = c()
  if (length(which(ls() == "rmc.dynamic_range")) == 0) rmc.dynamic_range = c()
  if (length(which(ls() == "rmc.unsignedbit")) == 0) rmc.unsignedbit = TRUE
  if (length(which(ls() == "rmc.origin")) == 0) rmc.origin = "1970-01-01"
  if (length(which(ls() == "rmc.desiredtz")) == 0) rmc.desiredtz= "Europe/London"
  if (length(which(ls() == "rmc.sf")) == 0) rmc.sf  = c()
  if (length(which(ls() == "rmc.headername.sf")) == 0) rmc.headername.sf = c()
  if (length(which(ls() == "rmc.headername.sn")) == 0) rmc.headername.sn = c()
  if (length(which(ls() == "rmc.headername.recordingid")) == 0) rmc.headername.recordingid = c()
  if (length(which(ls() == "rmc.header.structure")) == 0) rmc.header.structure = c()
  if (length(which(ls() == "rmc.check4timegaps")) == 0) rmc.check4timegaps = FALSE
  if (length(which(ls() == "rmc.noise")) == 0) rmc.noise = c()
  if (length(which(ls() == "rmc.col.wear")) == 0) rmc.col.wear = c()
  if (length(which(ls() == "rmc.doresample")) == 0) rmc.doresample = FALSE
  metrics2do = data.frame(do.bfen, do.enmo,do.lfenmo,do.en,do.hfen,
                          do.hfenplus, do.mad,do.anglex,do.angley,do.anglez,
                          do.roll_med_acc_x, do.roll_med_acc_y,do.roll_med_acc_z,
                          do.dev_roll_med_acc_x, do.dev_roll_med_acc_y,
                          do.dev_roll_med_acc_z, do.enmoa,do.lfen,
                          do.lfx, do.lfy, do.lfz,
                          do.hfx, do.hfy, do.hfz,
                          do.bfx, do.bfy, do.bfz, stringsAsFactors = TRUE)
  if (length(chunksize) == 0) chunksize = 1
  if (chunksize > 1.5) chunksize = 1.5
  if (chunksize < 0.2) chunksize = 0.2
  nmetrics = sum(c(do.bfen,do.enmo,do.lfenmo,do.en,do.hfen,do.hfenplus,do.mad,
                   do.anglex,do.angley,do.anglez,
                   do.roll_med_acc_x,do.roll_med_acc_y,do.roll_med_acc_z,
                   do.dev_roll_med_acc_x, do.dev_roll_med_acc_y, do.dev_roll_med_acc_z,do.enmoa, do.lfen,
                   do.lfx, do.lfy, do.lfz,  do.hfx, do.hfy, do.hfz,  do.bfx, do.bfy, do.bfz))
  if (length(myfun) != 0) {
    nmetrics = nmetrics + length(myfun$colnames)
    # check myfun object already, because we do not want to discover
    # bugs after waiting for the data to be load
    check_myfun(myfun, windowsizes)
  }

  if (length(nmetrics) == 0) {
    cat("\nWARNING: No metrics selected\n")
  }
  filename = unlist(strsplit(as.character(datafile),"/"))
  filename = filename[length(filename)]
  #   #parameters
  ws3 = windowsizes[1] ; ws2 = windowsizes[2]; ws = windowsizes[3]  #window sizes
  if ((ws2/60) != round(ws2/60)) {
    ws2 = as.numeric(60 * round(ws2/60))
    cat("\nWARNING: The long windowsize needs to be a multitude of five minutes periods. The\n")
    cat(paste("\nlong windowsize has now been automatically adjusted to: ",ws2," seconds in order to meet this criteria.\n",sep=""))
  }
  if ((ws2/ws3) != round(ws2/ws3)) {
    def = c(1,5,10,15,20,30,60)
    def2 = abs(def - ws3)
    ws3 = as.numeric(def[which(def2 == min(def2))])
    cat("\nWARNING: The long windowsize needs to be a multitude of short windowsize. The \n")
    cat(paste("\nshort windowsize has now been automatically adjusted to: ",ws3," seconds in order to meet this criteria.\n",sep=""))
  }
  windowsizes = c(ws3,ws2,ws)
  data = PreviousEndPage = starttime = wday = weekdays = wdayname = c()

  monnames = c("genea","geneactive","actigraph","axivity","movisens") #monitor names
  filequality = data.frame(filetooshort=FALSE,filecorrupt=FALSE,
                           filedoesnotholdday = FALSE,NFilePagesSkipped = 0, stringsAsFactors = TRUE)
  i = 1 #counter to keep track of which binary block is being read
  count = 1 #counter to keep track of the number of seconds that have been read
  count2 = 1 #count number of blocks read with length "ws2" (long epoch, 15 minutes by default)
  LD = 2 #dummy variable used to identify end of file and to make the process stop
  bsc_qc = data.frame(time=c(),size=c(), stringsAsFactors = FALSE)
  # inspect file
  if (length(unlist(strsplit(datafile,"[.]RD"))) > 1) {
    useRDA = TRUE
  } else {
    useRDA = FALSE
  }
  options(warn=-1)
  if (useRDA == FALSE) {
    INFI = g.inspectfile(datafile, desiredtz=desiredtz,
                         rmc.dec=rmc.dec,configtz=configtz,
                         rmc.firstrow.acc = rmc.firstrow.acc,
                         rmc.firstrow.header = rmc.firstrow.header,
                         rmc.header.length = rmc.header.length,
                         rmc.col.acc = rmc.col.acc,
                         rmc.col.temp = rmc.col.temp, rmc.col.time=rmc.col.time,
                         rmc.unit.acc = rmc.unit.acc, rmc.unit.temp = rmc.unit.temp,
                         rmc.unit.time = rmc.unit.time,
                         rmc.format.time = rmc.format.time,
                         rmc.bitrate = rmc.bitrate, rmc.dynamic_range = rmc.dynamic_range,
                         rmc.unsignedbit = rmc.unsignedbit,
                         rmc.origin = rmc.origin,
                         rmc.desiredtz = rmc.desiredtz, rmc.sf = rmc.sf,
                         rmc.headername.sf = rmc.headername.sf,
                         rmc.headername.sn = rmc.headername.sn,
                         rmc.headername.recordingid = rmc.headername.sn,
                         rmc.header.structure = rmc.header.structure,
                         rmc.check4timegaps = rmc.check4timegaps)  # Check which file type and monitor brand it is
  } else {
    load(datafile)
    INFI = I
    sf = INFI$sf #= sf #new line
  }
  options(warn=0)
  mon = INFI$monc
  dformat = INFI$dformc
  sf = INFI$sf
  if (length(sf) == 0) { # if sf is not available then try to retrieve sf from rmc.sf
    if (length(rmc.sf) == 0) {
      stop("Could not identify sample frequency")
    } else {
      if (rmc.sf == 0) {
        stop("Could not identify sample frequency")
      } else {
        sf = rmc.sf
      }
    }
  }
  if (sf == 0) stop("Sample frequency not recognised") #assume 80Hertz in the absense of any other info
  header = INFI$header
  options(warn=-1)
  if (useRDA == FALSE) decn =g.dotorcomma(datafile,dformat,mon=mon, desiredtz=desiredtz, rmc.dec = rmc.dec)
  options(warn=0)

  ID = g.getidfromheaderobject(filename=filename,header=header,dformat=dformat,mon=mon)
  # get now-wear, clip, and blocksize parameters (thresholds)
  ncb_params = get_nw_clip_block_params(chunksize, dynrange, mon, rmc.noise, sf, dformat)
  clipthres = ncb_params$clipthres
  blocksize = ncb_params$blocksize
  sdcriter = ncb_params$sdcriter
  racriter = ncb_params$racriter

  #creating matrixes for storing output
  S = matrix(0,0,4) #dummy variable needed to cope with head-tailing succeeding blocks of data
  nev = 80*10^7 # number expected values
  # NR = ceiling((90*10^6) / (sf*ws3)) + 1000 #NR = number of 'ws3' second rows (this is for 10 days at 80 Hz)
  NR = ceiling(nev / (sf*ws3)) + 1000 #NR = number of 'ws3' second rows (this is for 10 days at 80 Hz)
  metashort = matrix(" ",NR,(1+nmetrics)) #generating output matrix for acceleration signal
  if (mon == 1 | mon == 3 | (mon == 4 & dformat == 3) | (mon == 4 & dformat == 2) | (mon == 0 & length(rmc.col.temp) == 0)) {
    temp.available = FALSE
  } else if (mon == 2 | (mon == 4 & dformat == 4)  | mon == 5 | (mon == 0 & length(rmc.col.temp) > 0)){
    temp.available = TRUE
  }
  if (temp.available == FALSE) {
    metalong = matrix(" ",((nev/(sf*ws2))+100),4) #generating output matrix for 15 minutes summaries
  } else if (temp.available == TRUE & mon != 5){
    metalong = matrix(" ",((nev/(sf*ws2))+100),7) #generating output matrix for 15 minutes summaries
  } else if (temp.available == TRUE & mon == 5){
    metalong = matrix(" ",((nev/(sf*ws2))+100),5) #generating output matrix for 15 minutes summaries
  }
  #------------------------------------------
  if (length(unlist(strsplit(datafile,"[.]RD"))) > 1) {
    useRDA = TRUE
  } else {
    useRDA = FALSE
  }
  #===============================================
  # Read file
  switchoffLD = 0 #dummy variable part "end of loop mechanism"
  sforiginal = sf
  while (LD > 1) {
    P = c()
    if (i  == 1) {
      cat(paste("\nLoading chunk: ",i,sep=""))
    } else {
      cat(paste(" ",i,sep=""))
    }
    options(warn=-1) #turn off warnings (code complains about unequal rowlengths
    if (useRDA == FALSE) {
      accread = g.readaccfile(filename=datafile,blocksize=blocksize,blocknumber=i,
                              selectdaysfile = selectdaysfile,
                              filequality=filequality,decn=decn,
                              dayborder=dayborder,ws=ws,desiredtz=desiredtz,
                              PreviousEndPage=PreviousEndPage,
                              inspectfileobject=INFI,
                              configtz=configtz,
                              rmc.dec=rmc.dec,
                              rmc.firstrow.acc = rmc.firstrow.acc,
                              rmc.firstrow.header = rmc.firstrow.header,
                              rmc.header.length = rmc.header.length,
                              rmc.col.acc = rmc.col.acc,
                              rmc.col.temp = rmc.col.temp, rmc.col.time=rmc.col.time,
                              rmc.unit.acc = rmc.unit.acc, rmc.unit.temp = rmc.unit.temp,
                              rmc.unit.time = rmc.unit.time,
                              rmc.format.time = rmc.format.time,
                              rmc.bitrate = rmc.bitrate, rmc.dynamic_range = rmc.dynamic_range,
                              rmc.unsignedbit = rmc.unsignedbit,
                              rmc.origin = rmc.origin,
                              rmc.desiredtz = rmc.desiredtz, rmc.sf = rmc.sf,
                              rmc.headername.sf = rmc.headername.sf,
                              rmc.headername.sn = rmc.headername.sn,
                              rmc.headername.recordingid = rmc.headername.sn,
                              rmc.header.structure = rmc.header.structure,
                              rmc.check4timegaps = rmc.check4timegaps,
                              rmc.col.wear=rmc.col.wear,
                              rmc.doresample=rmc.doresample)
      P = accread$P
      filequality = accread$filequality
      filetooshort = filequality$filetooshort
      filecorrupt = filequality$filecorrupt
      filedoesnotholdday = filequality$filedoesnotholdday
      NFilePagesSkipped = filequality$NFilePagesSkipped
      switchoffLD = accread$switchoffLD
      PreviousEndPage = accread$endpage
      PreviousStartPage = accread$startpage
      rm(accread); gc()
      if(mon == 5) { # if movisens, then read temperature
        temperature = g.readtemp_movisens(datafile, desiredtz, PreviousStartPage, PreviousEndPage)
        P = cbind(P, temperature[1:nrow(P)])
        colnames(P)[4] = "temp"
        }
    } else {
      filetooshort = FALSE
      filecorrupt = FALSE
      filedoesnotholdday = FALSE
      NFilePagesSkipped = 0
    }
    options(warn=0) #turn on warnings
    #============
    #process data as read from binary file
    if (useRDA == TRUE) {
      if (i == 1) {
        P = 1
      } else {
        P = c()
      }
    }
    if (length(P) > 0) { #would have been set to zero if file was corrupt or empty
      if (useRDA == FALSE) {
        if (mon == 1 & dformat == 1) {
          data = P$rawxyz / 1000 #convert mg output to g for genea
        } else if (mon == 2  & dformat == 1) {
          data = P$data.out
        } else if (dformat == 2) {
          data = P #as.matrix(P,dimnames = list(rownames(P),colnames(P)))
        } else if (dformat == 3) {
          data = P$rawxyz
        } else if (dformat == 4) {
          if (P$header$hardwareType == "AX6") { # cwa AX6
            # Note 18-Feb-2020: For the moment GGIR ignores the AX6 gyroscope signals until robust sensor
            # fusion algorithms and gyroscope metrics have been prepared
            data = P$data[,-c(2:4)]
          } else { # cwa AX3
            data = P$data
          }
        } else if (dformat == 5) {
          data = P$data
        } else if (mon == 5) {
          data = as.matrix(P)
        }
        #add left over data from last time
        if (nrow(S) > 0) {
          data = rbind(S,data)
        }
        SWMT = get_starttime_weekday_meantemp_truncdata(temp.available, mon, dformat,
                                                        data, selectdaysfile,
                                                        P, header, desiredtz,
                                                        sf, i, datafile,  ws2,
                                                        starttime, wday, weekdays, wdayname)
        starttime=SWMT$starttime
        meantemp=SWMT$meantemp
        use.temp=SWMT$use.temp
        wday=SWMT$wday; weekdays=SWMT$SWMT$weekdays; wdayname=SWMT$wdayname
        desiredtz=SWMT$desiredtz; data=SWMT$data
        rm(SWMT)
        if (exists("P")) rm(P); gc()
        if (i != 0 & length(selectdaysfile) == 0 & exists("P")) rm(P); gc()
        LD = nrow(data)
        if (LD < (ws*sf) & i == 1) {
          warning('\nWarning data too short for doing non-wear detection 3\n')
          switchoffLD = 1
          LD = 0 #ignore rest of the data and store what has been loaded so far.
        }
      } else { # if useRDA == TRUE
        LD = nrow(data)
      }
      #store data that could not be used for this block, but will be added to next block
      if (LD >= (ws*sf)) {
        if (useRDA == FALSE) {
          use = (floor(LD / (ws2*sf))) * (ws2*sf) #number of datapoint to use # changes from ws to ws2 Vvh 23/4/2017
          if (length(myfun) != 0) { # if using external function, then check that use is a multitude of the expected windowlength
            Nminlength = use / myfun$minlength
            if (Nminlength != floor(Nminlength)) { # it is not a multitude
              use = floor(Nminlength) * myfun$minlength # correct use accordingly
            }
          }
          if ((LD - use) > 1) {
            # reading csv files
            S = as.matrix(data[(use+1):LD,]) #store left over (included as.matrix)
            if (ncol(S) == 1) {
              S = t(S)
            }
          } else { #use all data
            S = matrix(0,0,ncol(data))
          }
          data = as.matrix(data[1:use,])
          LD = nrow(data) #redefine LD because there is less data
          ##==================================================
          # Feature calculation
          dur = nrow(data)	#duration of experiment in data points
          durexp = nrow(data) / (sf*ws)	#duration of experiment in hrs
          data = as.matrix(data)
          #--------------------------------------------
          if (mon == 2 | (mon == 4 & dformat == 4) | mon == 5 | (mon == 0 & length(rmc.col.temp) > 0)) {
            if (mon == 2) {
              temperaturecolumn = 7; lightcolumn = 5
            } else if (mon ==4) {
              temperaturecolumn = 5; lightcolumn = 7
            } else if (mon == 5) {
              temperaturecolumn = 4
            } else if (mon ==0) {
              temperaturecolumn = 5
            }
            if (mon != 0 & mon != 5) {
              light = as.numeric(data[,lightcolumn])
            }
            if (mon == 0 & length(rmc.col.wear) > 0) {
              wearcol = as.character(data[, which(colnames(data) == "wear")])
              suppressWarnings(storage.mode(wearcola) <- "logical")
            }
            temperature = as.numeric(data[,temperaturecolumn])
          }
          # Initialization of variables
          if (mon == 1) {
            data = data[,1:3]
            data[,1:3] = scale(data[,1:3],center = -offset, scale = 1/scale) #rescale data
          } else if (mon == 4 & dformat == 3) {
            data = data[,1:3]
            data[,1:3] = scale(data[,1:3],center = -offset, scale = 1/scale) #rescale data
          } else if (mon == 4 & (dformat == 4 |  dformat == 2)) {
            data = data[,2:4]
            data[,1:3] = scale(data[,1:3],center = -offset, scale = 1/scale) #rescale data
          } else if (mon == 2 & dformat == 1) {
            yy = as.matrix(cbind(as.numeric(data[,7]),as.numeric(data[,7]),as.numeric(data[,7])))
            data = data[,2:4]
            data[,1:3] = scale(as.matrix(data[,1:3]),center = -offset, scale = 1/scale) +
              scale(yy, center = rep(meantemp,3), scale = 1/tempoffset)  #rescale data
            rm(yy); gc()
          } else if(mon == 5) {
            yy = as.matrix(cbind(as.numeric(data[,4]),as.numeric(data[,4]),as.numeric(data[,4])))
            data = data[,1:3]
            data[,1:3] = scale(as.matrix(data[,1:3]),center = -offset, scale = 1/scale) +
              scale(yy, center = rep(meantemp,3), scale = 1/tempoffset)  #rescale data
            rm(yy); gc()
          } else if ((dformat == 2 | dformat == 5) & (mon != 4)) {
            if (mon == 2 | (mon == 0 & use.temp == TRUE)) {
              tempcolumnvalues = as.numeric(as.character(data[,temperaturecolumn]))
              yy = as.matrix(cbind(tempcolumnvalues, tempcolumnvalues, tempcolumnvalues))
              meantemp = mean(as.numeric(data[,temperaturecolumn]))
              if (length(meantempcal) == 0) meantempcal = meantemp
            }
            if (ncol(data) == 3) data = data[,1:3]
            if (ncol(data) >= 4) {
              data = data[,2:4]
              if (class(data[,1]) == "character") {
                data = apply(data, 2,as.numeric)
              }
            }
            if ((mon == 3 | mon == 0) & use.temp == FALSE) {
              data[,1:3] = scale(data[,1:3],center = -offset, scale = 1/scale)  #rescale data
            } else if ((mon == 2 | mon == 0) & use.temp == TRUE) {
              # meantemp replaced by meantempcal # 19-12-2013
              data[,1:3] = scale(data[,1:3],center = -offset, scale = 1/scale) +
                scale(yy, center = rep(meantempcal,3), scale = 1/tempoffset)  #rescale data
              rm(yy); gc()
            }
          }
          suppressWarnings(storage.mode(data) <- "numeric")
          ## resample experiment to see whehter processing time can be much improved if data is resampled
          sfold = sforiginal # keep sf, because light, temperature are not resampled at the moment
          # STORE THE RAW DATA
          # data[,1], data[,2], data[,3], starttime, (temperature, light)
          if (length(selectdaysfile) > 0) {
            path3 = paste(outputdir,outputfolder,sep="") #where is output stored?
            # calculate extra timestamp in a more complete format
            # i am doing this here and not at the top of the code, because at this point the starttime has already be adjusted
            # to the starttime of the first epoch in the data
            # starttime_aschar_tz = strftime(as.POSIXlt(as.POSIXct(starttime),tz=desiredtz),format="%Y-%m-%d %H:%M:%S %z")
            if (mon == 2 | (mon == 4 & dformat == 4) | (dformat == 5 & mon == 0)) {
              I = INFI
              save(I,sf,wday,wdayname,decn,data,starttime,temperature,light,
                   file = paste(path3,"/meta/raw/",filename,"_day",i,".RData",sep=""))
            } else if (mon == 5) {
              save(I,sf,wday,wdayname,decn,data,starttime,temperature,
                   file = paste(path3,"/meta/raw/",filename,"_day",i,".RData",sep=""))
            } else {
              save(I,sf,wday,wdayname,decn,data,starttime,
                   file = paste(path3,"/meta/raw/",filename,"_day",i,".RData",sep=""))
            }
          }
        } else {
          #           load(datafile) # turned off because datafile will already be loaded (earlier on in script)
          data = cbind(rep(0,nrow(data)),data)
          LD = nrow(data)
        }
        EN = sqrt(data[,1]^2 + data[,2]^2 + data[,3]^2) # Do not delete Used for long epoch calculation
        allmetrics = g.applymetrics(data = data,n=n,sf=sf,ws3=ws3,metrics2do=metrics2do, lb=lb,hb=hb)
        BFEN = allmetrics$BFEN
        ENMO = allmetrics$ENMO
        ENMOa = allmetrics$ENMOa
        LFENMO = allmetrics$LFENMO
        EN_shortepoch = allmetrics$EN
        HFEN = allmetrics$HFEN
        HFENplus = allmetrics$HFENplus
        MAD = allmetrics$MAD
        angle_x = allmetrics$angle_x
        angle_y = allmetrics$angle_y
        angle_z = allmetrics$angle_z
        roll_med_acc_x = allmetrics$roll_med_acc_x
        roll_med_acc_y = allmetrics$roll_med_acc_y
        roll_med_acc_z = allmetrics$roll_med_acc_z
        dev_roll_med_acc_x = allmetrics$dev_roll_med_acc_x
        dev_roll_med_acc_y = allmetrics$dev_roll_med_acc_y
        dev_roll_med_acc_z = allmetrics$dev_roll_med_acc_z
        LFEN = allmetrics$LFEN
        HFX =  allmetrics$HFX
        HFY =  allmetrics$HFY
        HFZ =  allmetrics$HFZ
        LFX =  allmetrics$LFX
        LFY =  allmetrics$LFY
        LFZ =  allmetrics$LFZ
        BFX =  allmetrics$BFX
        BFY =  allmetrics$BFY
        BFZ =  allmetrics$BFZ
        #--------------------------------------------------------------------
        if (length(myfun) != 0) { # apply external function to the data to extract extra features
          #starttime
          if (is.logical(myfun$timestamp) == T) {
            if (myfun$timestamp == TRUE) {
              myfun$timestamp = starttime
            } else {
              myfun$timestamp = c()
            }
          }
          OutputExternalFunction = applyExtFunction(data, myfun, sf, ws3)
        }
      }
      if (LD >= (ws*sf)) { #LD != 0
        #-----------------------------------------------------
        #extend metashort and metalong if it is expected to be too short
        if (count > (nrow(metashort) - (2.5*(3600/ws3) *24))) {
          extension = matrix(" ",((3600/ws3) *24),ncol(metashort)) #add another day to metashort once you reach the end of it
          metashort = rbind(metashort,extension)
          extension2 = matrix(" ",((3600/ws2) *24),ncol(metalong)) #add another day to metashort once you reach the end of it
          metalong = rbind(metalong,extension2)
          cat("\nvariable metashort extended\n")
        }
        col_msi = 2
        if (do.bfen == TRUE) {
          metashort[count:(count-1+length(BFEN)),col_msi] = BFEN; col_msi = col_msi + 1
        }
        if (do.enmo == TRUE) {
          metashort[count:(count-1+length(ENMO)),col_msi] = ENMO; col_msi = col_msi + 1
        }
        if (do.lfenmo == TRUE) {
          metashort[count:(count-1+length(LFENMO)),col_msi] = LFENMO; col_msi = col_msi + 1
        }
        if (do.en == TRUE) {
          metashort[count:(count-1+length(EN_shortepoch)),col_msi] = EN_shortepoch; col_msi = col_msi + 1
        }
        if (do.hfen == TRUE) {
          metashort[count:(count-1+length(HFEN)),col_msi] = HFEN; col_msi = col_msi + 1
        }
        if (do.hfenplus == TRUE) {
          metashort[count:(count-1+length(HFENplus)),col_msi] = HFENplus; col_msi = col_msi + 1
        }
        if (do.mad == TRUE) {
          metashort[count:(count-1+length(MAD)),col_msi] = MAD; col_msi = col_msi + 1
        }
        if (do.anglex == TRUE) {
          metashort[count:(count-1+length(angle_x)),col_msi] = angle_x; col_msi = col_msi + 1
        }
        if (do.angley == TRUE) {
          metashort[count:(count-1+length(angle_y)),col_msi] = angle_y; col_msi = col_msi + 1
        }
        if (do.anglez == TRUE) {
          metashort[count:(count-1+length(angle_z)),col_msi] = angle_z; col_msi = col_msi + 1
        }
        if (do.roll_med_acc_x == TRUE) {
          metashort[count:(count-1+length(roll_med_acc_x)),col_msi] = roll_med_acc_x; col_msi = col_msi + 1
        }
        if (do.roll_med_acc_y == TRUE) {
          metashort[count:(count-1+length(roll_med_acc_y)),col_msi] = roll_med_acc_y; col_msi = col_msi + 1
        }
        if (do.roll_med_acc_z == TRUE) {
          metashort[count:(count-1+length(roll_med_acc_z)),col_msi] = roll_med_acc_z; col_msi = col_msi + 1
        }
        if (do.dev_roll_med_acc_x == TRUE) {
          metashort[count:(count-1+length(dev_roll_med_acc_x)),col_msi] = dev_roll_med_acc_x; col_msi = col_msi + 1
        }
        if (do.dev_roll_med_acc_y == TRUE) {
          metashort[count:(count-1+length(dev_roll_med_acc_y)),col_msi] = dev_roll_med_acc_y; col_msi = col_msi + 1
        }
        if (do.dev_roll_med_acc_z == TRUE) {
          metashort[count:(count-1+length(dev_roll_med_acc_z)),col_msi] = dev_roll_med_acc_z; col_msi = col_msi + 1
        }
        if (do.enmoa == TRUE) {
          metashort[count:(count-1+length(ENMOa)),col_msi] = ENMOa; col_msi = col_msi + 1
        }
        if (do.lfen == TRUE) {
          metashort[count:(count-1+length(LFEN)),col_msi] = LFEN; col_msi = col_msi + 1
        }
        if (do.lfx == TRUE) {
          metashort[count:(count-1+length(LFX)),col_msi] = LFX; col_msi = col_msi + 1
        }
        if (do.lfy == TRUE) {
          metashort[count:(count-1+length(LFY)),col_msi] = LFY; col_msi = col_msi + 1
        }
        if (do.lfz == TRUE) {
          metashort[count:(count-1+length(LFZ)),col_msi] = LFZ; col_msi = col_msi + 1
        }
        if (do.hfx == TRUE) {
          metashort[count:(count-1+length(HFX)),col_msi] = HFX; col_msi = col_msi + 1
        }
        if (do.hfy == TRUE) {
          metashort[count:(count-1+length(HFY)),col_msi] = HFY; col_msi = col_msi + 1
        }
        if (do.hfz == TRUE) {
          metashort[count:(count-1+length(HFZ)),col_msi] = HFZ; col_msi = col_msi + 1
        }
        if (do.bfx == TRUE) {
          metashort[count:(count-1+length(BFX)),col_msi] = BFX; col_msi = col_msi + 1
        }
        if (do.bfy == TRUE) {
          metashort[count:(count-1+length(BFY)),col_msi] = BFY; col_msi = col_msi + 1
        }
        if (do.bfz == TRUE) {
          metashort[count:(count-1+length(BFZ)),col_msi] = BFZ; col_msi = col_msi + 1
        }

        if (length(myfun) != 0) { # if an external function is applied.
          NcolEF = ncol(OutputExternalFunction)-1 # number of extra columns needed
          metashort[count:(count-1+nrow(OutputExternalFunction)),col_msi:(col_msi+NcolEF)] = as.matrix(OutputExternalFunction); col_msi = col_msi + NcolEF + 1
        }

        count = count + length(EN_shortepoch) #increasing "count" the indicator of how many seconds have been read
        rm(allmetrics)
        # update blocksize depending on available memory
        BlocksizeNew = updateBlocksize(blocksize=blocksize, bsc_qc=bsc_qc)
        bsc_qc = BlocksizeNew$bsc_qc
        blocksize = BlocksizeNew$blocksize
        ##==================================================
        # MODULE 2 - non-wear time & clipping
        #cat("\nmodule 2\n") #notice that windows overlap for non-wear detecting
        window2 = ws2 * sf #window size in samples
        window = ws * sf #window size in samples
        nmin = floor(LD/(window2)) #nmin = minimum number of windows that fit in this block of data
        CW = NW = matrix(0,nmin,3) #CW is clipping, NW is non-wear
        TS1W = TS2W = TS3W = TS4W = TS5W = TS6W = TS7W = CWav = NWav = matrix(0,nmin,1)
        crit = ((window/window2)/2)+1
        for (h in 1: nmin) { #number of windows
          cliphoc1 = (((h-1)*window2)+ window2*0.5 ) - window2*0.5 #does not use "window"
          cliphoc2 = (((h-1)*window2)+ window2*0.5 ) + window2*0.5
          if (h <= crit) {
            hoc1 = 1
            hoc2 = window
          } else if (h >= (nmin - crit)) {
            hoc1 = (nmin-crit)*window2
            hoc2 = nmin*window2 #end of data
          } else if (h > crit & h < (nmin - crit)) {
            hoc1=(((h-1)*window2)+ window2*0.5 ) - window*0.5
            hoc2=(((h-1)*window2)+ window2*0.5 ) + window*0.5
          }
          if (length(rmc.col.wear) > 0) {
            wearTable = table(wearcol[(1+hoc1):hoc2], useNA = FALSE)
            NWav[h,1] = as.logical(tail(names(sort(wearTable)), 1)) * 3 # times 3 to simulate heuristic approach
          }
          for (jj in  1:3) {
            #hoc1 & hoc2 = edges of windows
            #window is bigger& window2 is smaller one
            sdwacc = sd(as.numeric(data[(1+hoc1):hoc2,jj]),na.rm=TRUE)
            maxwacc = max(as.numeric(data[(1+hoc1):hoc2,jj]),na.rm=TRUE)
            minwacc = min(as.numeric(data[(1+hoc1):hoc2,jj]),na.rm=TRUE)
            CW[h,jj] = length(which(abs(as.numeric(data[(1+cliphoc1):cliphoc2,jj])) > clipthres))
            if (length(which(abs(as.numeric(data[(1+cliphoc1):cliphoc2,jj])) > clipthres*1.5)) > 0) {
              CW[h,jj] = window2 # If there is a a value that is more than 150% the dynamic range then ignore entire block.
            }
            absrange = abs(maxwacc - minwacc)
            if (is.numeric(absrange) == TRUE & is.numeric(sdwacc) == TRUE & is.na(sdwacc) == FALSE) {
              if (sdwacc < sdcriter) {
                if (absrange < racriter) {
                  NW[h,jj] = 1
                }
              }
            } else {
              NW[h,jj] = 1 # if sdwacc, maxwacc, or minwacc could not be derived then label as non-wear
            }
          }
          CW = CW / (window2) #changed 30-1-2012, was window*sf
          if (length(rmc.col.wear) == 0) {
            NWav[h,1] = (NW[h,1] + NW[h,2] + NW[h,3]) #indicator of non-wear
          }
          CWav[h,1] = max(c(CW[h,1],CW[h,2],CW[h,3])) #indicator of clipping
        }
        col_mli = 2
        metalong[count2:((count2-1)+nrow(NWav)),col_mli] = NWav; col_mli = col_mli + 1
        metalong[(count2):((count2-1)+nrow(NWav)),col_mli] = CWav; col_mli = col_mli + 1
        if (mon == 2 | (mon == 4 & dformat == 4) | mon == 5) { #going from sample to ws2
          if (mon == 2 | (mon == 4 & dformat == 4)) {
            #light (running mean)
          lightc = cumsum(c(0,light))
          select = seq(1,length(lightc),by=(ws2*sfold))
          lightmean = diff(lightc[round(select)]) / abs(diff(round(select)))
          rm(lightc); gc()
          #light (running max)
          lightmax = matrix(0,length(lightmean),1)
          for (li in 1:(length(light)/(ws2*sfold))) {
            tempm = max(light[((li-1)*(ws2*sfold)):(li*(ws2*sfold))])
            if (length(tempm) > 0) {
              lightmax[li] = tempm[1]
            } else {
              lightmax[li] = max(light[((li-1)*(ws2*sfold)):(li*(ws2*sfold))])
            }
           }
          }
          #temperature (running mean)
          temperaturec = cumsum(c(0,temperature))
          select = seq(1,length(temperaturec),by=(ws2*sfold))
          temperatureb = diff(temperaturec[round(select)]) / abs(diff(round(select)))
          rm(temperaturec); gc()
        }
        #EN going from sample to ws2
        ENc = cumsum(c(0,EN))
        select = seq(1,length(ENc),by=(ws2*sf)) #<= EN is derived from data, so it needs the new sf
        ENb = diff(ENc[round(select)]) / abs(diff(round(select)))
        rm(ENc, EN); gc()
        if (mon == 2 | (mon == 4 & dformat == 4)) {
          metalong[(count2):((count2-1)+nrow(NWav)),col_mli] = lightmean; col_mli= col_mli + 1
          metalong[(count2):((count2-1)+nrow(NWav)),col_mli] = lightmax; col_mli= col_mli + 1
          metalong[(count2):((count2-1)+nrow(NWav)),col_mli] = temperatureb; col_mli= col_mli + 1
        } else if (mon == 5) {
          metalong[(count2):((count2-1)+nrow(NWav)),col_mli] = temperatureb; col_mli= col_mli + 1
        }
        metalong[(count2):((count2-1)+nrow(NWav)),col_mli] = ENb; col_mli= col_mli + 1
        count2  = count2 + nmin
        if (exists("data")) rm(data)
        if (exists("light")) rm(light)
        if (exists("temperature")) rm(temperature)
        gc()
      } #end of section which is skipped when switchoff == 1
    } else {
      LD = 0 #once LD < 1 the analysis stops, so this is a trick to stop it
      # stop reading because there is not enough data in this block
    }
    if (switchoffLD == 1) LD = 0
    if (ceiling(daylimit) != FALSE) {
      if (i == ceiling(daylimit)) { #to speed up testing only read first 'i' blocks of data
        LD = 0 #once LD < 1 the analysis stops, so this is a trick to stop it
        cat(paste("\nstopped reading data because this analysis is limited to ",ceiling(daylimit)," days\n",sep=""))
      }
    }
    i = i + 1 #go to next block
  }
  # deriving timestamps
  if (filecorrupt == FALSE & filetooshort == FALSE & filedoesnotholdday == FALSE) {
    # cut = (count+1):nrow(metashort) # how it was
    cut = count:nrow(metashort)
    if (length(cut) > 1) {
      metashort = as.matrix(metashort[-cut,])
    }
    if (nrow(metashort) > 1) {
      starttime3 = round(as.numeric(starttime)) #numeric time but relative to the desiredtz
      time5 = seq(starttime3,(starttime3+((nrow(metashort)-1)*ws3)),by=ws3)
      if (length(selectdaysfile) > 0 & length(time5) > round((24*(3600/ws3))+1)) { # (Millenium cohort)
        #===================================================================
        # All of the below needed for Millenium cohort
        SDF = read.csv(selectdaysfile)
        if (useRDA == FALSE) {
          I = g.inspectfile(datafile, desiredtz=desiredtz,
                            rmc.dec=rmc.dec,configtz=configtz,
                            rmc.firstrow.acc = rmc.firstrow.acc,
                            rmc.firstrow.header = rmc.firstrow.header,
                            rmc.header.length = rmc.header.length,
                            rmc.col.acc = rmc.col.acc,
                            rmc.col.temp = rmc.col.temp, rmc.col.time=rmc.col.time,
                            rmc.unit.acc = rmc.unit.acc, rmc.unit.temp = rmc.unit.temp,
                            rmc.unit.time = rmc.unit.time,
                            rmc.format.time = rmc.format.time,
                            rmc.bitrate = rmc.bitrate, rmc.dynamic_range = rmc.dynamic_range,
                            rmc.unsignedbit = rmc.unsignedbit,
                            rmc.origin = rmc.origin,
                            rmc.desiredtz = rmc.desiredtz, rmc.sf = rmc.sf,
                            rmc.headername.sf = rmc.headername.sf,
                            rmc.headername.sn = rmc.headername.sn,
                            rmc.headername.recordingid = rmc.headername.sn,
                            rmc.header.structure = rmc.header.structure,
                            rmc.check4timegaps = rmc.check4timegaps)
        }
        hvars = g.extractheadervars(I)
        deviceSerialNumber = hvars$deviceSerialNumber
        SDFi = which(as.numeric(SDF$Monitor) == as.numeric(deviceSerialNumber))
        dateday1 = as.character(SDF[SDFi,2])
        dateday2 = as.character(SDF[SDFi,3])
        dtday1 = as.POSIXlt(paste0(dateday1," 01:00:00"),format="%d/%m/%Y %H:%M:%S")
        dtday2 = as.POSIXlt(paste0(dateday2," 01:00:00"),format="%d/%m/%Y %H:%M:%S")
        deltat = as.numeric(dtday2) - as.numeric(dtday1)
        delta_day = round(deltat / (3600*24)) - 1 # minus 1 because you naturally already
        # make a jump of 1 day
        daych = seq(round((24*(3600/ws3))+1),length(time5),by=1)
        if (length(daych) > (24*(3600/ws3))) {
          daych = daych[1:(24*(3600/ws3))]
        }
        time5[daych] = time5[daych] + (delta_day*24*3600) #+ 5
      }
      if (length(desiredtz) == 0) {
        print("desiredtz not specified, Europe/London used as default")
        desiredtz = "Europe/London"
      }
      time6 = as.POSIXlt(time5,origin="1970-01-01",tz=desiredtz)
      time6 = strftime(time6,format="%Y-%m-%dT%H:%M:%S%z")
      metashort[,1] = as.character(time6)
    }
    # cut2 = (count2+1):nrow(metalong)
    cut2 = (count2):nrow(metalong) # how it was
    if (length(cut2) > 1) {
      metalong = as.matrix(metalong[-cut2,])
    }
    if (nrow(metalong) > 2) {
      starttime4 = round(as.numeric(starttime)) #numeric time but relative to the desiredtz
      time1 = seq(starttime4,(starttime4+(nrow(metalong)*ws2)-1),by=ws2)
      if (length(selectdaysfile) > 0 & round((24*(3600/ws2))+1) < length(time1)) { # (Millenium cohort)
        #===================================================================
        # All of the below needed for Millenium cohort
        SDF = read.csv(selectdaysfile)
        if (useRDA == FALSE) {
          I = g.inspectfile(datafile, desiredtz=desiredtz,
                            rmc.dec=rmc.dec,configtz=configtz,
                            rmc.firstrow.acc = rmc.firstrow.acc,
                            rmc.firstrow.header = rmc.firstrow.header,
                            rmc.header.length = rmc.header.length,
                            rmc.col.acc = rmc.col.acc,
                            rmc.col.temp = rmc.col.temp, rmc.col.time=rmc.col.time,
                            rmc.unit.acc = rmc.unit.acc, rmc.unit.temp = rmc.unit.temp,
                            rmc.unit.time = rmc.unit.time,
                            rmc.format.time = rmc.format.time,
                            rmc.bitrate = rmc.bitrate, rmc.dynamic_range = rmc.dynamic_range,
                            rmc.unsignedbit = rmc.unsignedbit,
                            rmc.origin = rmc.origin,
                            rmc.desiredtz = rmc.desiredtz, rmc.sf = rmc.sf,
                            rmc.headername.sf = rmc.headername.sf,
                            rmc.headername.sn = rmc.headername.sn,
                            rmc.headername.recordingid = rmc.headername.sn,
                            rmc.header.structure = rmc.header.structure,
                            rmc.check4timegaps = rmc.check4timegaps)
        }
        hvars = g.extractheadervars(I)
        deviceSerialNumber = hvars$deviceSerialNumber
        SDFi = which(as.numeric(SDF$Monitor) == as.numeric(deviceSerialNumber))
        if (length(SDFi) == 1) { # if deviceSerialNumber is not in the file then this is skipped
          dateday1 = as.character(SDF[SDFi,2])
          dateday2 = as.character(SDF[SDFi,3])
          dtday1 = as.POSIXlt(paste0(dateday1," 01:00:00"),format="%d/%m/%Y %H:%M:%S")
          dtday2 = as.POSIXlt(paste0(dateday2," 01:00:00"),format="%d/%m/%Y %H:%M:%S")
          deltat = as.numeric(dtday2) - as.numeric(dtday1)
          delta_day = round(deltat / (3600*24)) - 1 # minues 1 because you naturally already make a jump of 1 day
          daych = seq(round((24*(3600/ws2))+1),length(time1),by=1)
          time1[daych] = time1[daych] + (delta_day*24*3600) #+ 5
        }
      }
      if (length(desiredtz) == 0) {
        print("desiredtz not specified, Europe/London used as default")
        desiredtz = "Europe/London"
      }
      time2 = as.POSIXlt(time1,origin="1970-01-01",tz=desiredtz)
      time2 = strftime(time2,format="%Y-%m-%dT%H:%M:%S%z")
      metalong[,1] = as.character(time2)
    }
    metricnames_short = c("timestamp","BFEN","ENMO","LFENMO","EN","HFEN","HFENplus","MAD",
                          "anglex","angley","anglez","roll_med_acc_x","roll_med_acc_y","roll_med_acc_z",
                          "dev_roll_med_acc_x","dev_roll_med_acc_y","dev_roll_med_acc_z","ENMOa","LFEN",
                          "LFX", "LFY", "LFZ", "HFX", "HFY", "HFZ", "BFX", "BFY", "BFZ") #
    metricnames_short = as.character(metricnames_short[c(TRUE,do.bfen,do.enmo,do.lfenmo,do.en,do.hfen,do.hfenplus,do.mad,
                                                         do.anglex,do.angley,do.anglez,
                                                         do.roll_med_acc_x,do.roll_med_acc_y,do.roll_med_acc_z,
                                                         do.dev_roll_med_acc_x,do.dev_roll_med_acc_y,do.dev_roll_med_acc_z,
                                                         do.enmoa,do.lfen,
                                                         do.lfx, do.lfy, do.lfz, do.hfx, do.hfy, do.hfz,
                                                         do.bfx, do.bfy, do.bfz)])
    # Following code is needed to make sure that algorithms that produce character value
    # output are not assumed to be numeric
    NbasicMetrics = length(metricnames_short)
    if (length(myfun) != 0) {
      metricnames_short = c(metricnames_short, myfun$colnames)
      if (myfun$outputtype == "numeric") NbasicMetrics = NbasicMetrics + length(myfun$colnames)
    }

    metashort = data.frame(A = metashort, stringsAsFactors = FALSE)
    names(metashort) = metricnames_short
    for (ncolms in 2:NbasicMetrics) {
      metashort[,ncolms] = as.numeric(metashort[,ncolms])
    }
    if (mon == 1 | mon == 3 | (mon == 4 & dformat == 3) | (mon == 4 & dformat == 2) | (mon == 0 & use.temp == FALSE)) {
      metricnames_long = c("timestamp","nonwearscore","clippingscore","en")
    } else if (mon == 2 | (mon == 4 & dformat == 4)  | (mon == 0 & use.temp == TRUE)) {
      metricnames_long = c("timestamp","nonwearscore","clippingscore","lightmean","lightpeak","temperaturemean","EN")
    } else if (mon == 5) {
      metricnames_long = c("timestamp","nonwearscore","clippingscore","temperaturemean","EN")
    }
    metalong = data.frame(A = metalong, stringsAsFactors = FALSE)
    names(metalong) = metricnames_long
    for (ncolml in 2:ncol(metalong)) {
      metalong[,ncolml] = as.numeric(metalong[,ncolml])
    }
  } else {
    metalong=metashort=wday=wdayname=windowsizes = c()
  }
  # detach(allmetrics,warn.conflicts = FALSE)
  if (length(metashort) == 0 | filedoesnotholdday == TRUE) filetooshort = TRUE
  invisible(list(filecorrupt=filecorrupt,filetooshort=filetooshort,NFilePagesSkipped=NFilePagesSkipped,
                 metalong=metalong, metashort=metashort,wday=wday,wdayname=wdayname,windowsizes=windowsizes,bsc_qc=bsc_qc))
}
