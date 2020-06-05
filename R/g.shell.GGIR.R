g.shell.GGIR = function(mode=1:5,datadir=c(),outputdir=c(),studyname=c(),f0=1,f1=0,
                        do.report=c(2),overwrite=FALSE,visualreport=FALSE,viewingwindow=1,
                        configfile =c(),myfun=c(), ...) {
  #get input variables
  input = list(...)
  if (length(input) > 0) {
    for (i in 1:length(names(input))) {
      txt = paste(names(input)[i],"=",input[i],sep="")
      if (class(unlist(input[i])) == "character" & length(unlist(input[i])) == 1) {
        txt = paste(names(input)[i],"='",unlist(input[i]),"'",sep="")
      }
      eval(parse(text=txt))
    }
  }
  if (length(which(ls() == "timewindow")) != 0) timewindow = input$timewindow
  # verify whether datadir is a directory or a list of files
  filelist = isfilelist(datadir)
  if (dir.exists(outputdir) == FALSE) stop("\nDirectory specified by argument outputdir, does not exist")
  derivef0f1 = FALSE
  if (length(f0) == 0 | length(f1) == 0) {
    derivef0f1 = TRUE
  } else {
    if (f0 == 0 | f1 == 0) derivef0f1 = TRUE
  }
  if (derivef0f1 == TRUE) { # What file to start with?
    f0 = 1
    if (filelist == FALSE) {  # What file to end with?
      f1 <- length(dir(datadir, recursive = TRUE, pattern = "[.](csv|bin|Rda|wa|cw)")) # modified by JH
    } else {
      f1 = length(datadir) #modified
    }
  }
  dopart1 = dopart2 = dopart3 = dopart4 = dopart5 = FALSE
  if (length(which(mode == 0)) > 0) {
    dopart1 = dopart2 = dopart3 = dopart4 = dopart5 = TRUE
  } else {
    # if (length(which(mode == 0)) > 0) dopart0 = TRUE
    if (length(which(mode == 1)) > 0) dopart1 = TRUE
    if (length(which(mode == 2)) > 0) dopart2 = TRUE
    if (length(which(mode == 3)) > 0) dopart3 = TRUE; do.anglez = TRUE
    if (length(which(mode == 4)) > 0) dopart4 = TRUE
    if (length(which(mode == 5)) > 0) dopart5 = TRUE
  }
  # test whether RData input was used and if so, use original outputfolder
  if (length(datadir) > 0) {
    # list of all csv and bin files
    fnames = datadir2fnames(datadir,filelist)
    # check whether these are RDA
    if (length(unlist(strsplit(fnames[1],"[.]RD"))) > 1) {
      useRDA = TRUE
    } else {
      useRDA = FALSE
    }
  } else {
    useRDA = FALSE
  }
  if (filelist == TRUE | useRDA == TRUE) {
    metadatadir = paste(outputdir,"/output_",studyname,sep="")
  } else {
    outputfoldername = unlist(strsplit(datadir,"/"))[length(unlist(strsplit(datadir,"/")))]
    metadatadir = paste(outputdir,"/output_",outputfoldername,sep="")
  }
  config_file_in_outputdir = paste0(metadatadir,"/config.csv")
  # Load config file if it exists:
  if (dir.exists(metadatadir) | length(configfile) > 0) {
    config = c()
    # so if outputdir was not created (first time) and if configfile is no
    # specified then we can safely assume that there is no configfile
    if (length(configfile) > 0) {
      if (file.exists(configfile)) {
        config = read.csv(file = configfile, stringsAsFactors = FALSE)
      } else {
        stop("\nDo not supply argument configfile if the configfile does not exist yet")
      }
    } else if (length(configfile) == 0) {
      # Note that, if both exist, we prioritise configfile over the configfile
      # stored in outputdir.
      if (file.exists(config_file_in_outputdir)) config = read.csv(file = config_file_in_outputdir, stringsAsFactors = FALSE)
    }
    if (length(config) > 0) {
      LS = ls()
      LS = LS[-which(LS == c("config"))]
      for (ci in 1:nrow(config)) {
        if (as.character(config[ci,1]) %in% LS == FALSE) { # only use config file values if argument is not provided as argument to g.shell.GGIR
          conv2logical = conv2num = c()
          suppressWarnings(try(expr = {conv2num = as.numeric(config[ci,2])},silent=TRUE))
          suppressWarnings(try(expr = {conv2logical = as.logical(config[ci,2])},silent=TRUE))
          if (length(conv2num) > 0) {
            numi = is.na(conv2num) == FALSE
          } else {
            numi = FALSE
          }
          logi = FALSE
          if (numi == FALSE & is.na(conv2logical) == FALSE) logi = TRUE
          if (logi == TRUE) {
            txt = paste(as.character(config[ci,1]),"=",as.logical(config[ci,2]),"",sep="")
          } else if (numi == TRUE) {
            txt = paste(as.character(config[ci,1]),"=",as.numeric(config[ci,2]),"",sep="")
          } else if (numi == FALSE & logi == FALSE) {
            if (length(config[ci,2]) > 0) {
              if (config[ci,2] == 'c()') {
                if (config[ci,1] == "def.no.sleep") def.no.sleep = c()
                if (config[ci,1] == "backup.cal.coef") backup.cal.coef = c()
                # Note that we do not re-assign the c(), because they are the default for most arguments that
                # can hold a c() anyway. def.noc.sleep is the only exception.
              } else if (config[ci,2] != 'c()') {
                if (grepl("c\\(", config[ci,2])) { # vector of numbers
                  tmp =  unlist(strsplit(unlist(strsplit(config[ci,2],"\\("))[2],","))[1]
                  isna = c()
                  suppressWarnings(try(expr = {isna = is.na(as.numeric(tmp))},silent=TRUE))
                  if (length(isna) == 0) isna = FALSE
                  if (isna == TRUE) { # it is a vector with characters
                    vecchar = unlist(strsplit(unlist(strsplit(config[ci,2],"\\(|\\)"))[2],","))
                    if (config[ci,1] == "timewindow") timewindow = vecchar

                  } else {
                    txt = paste(as.character(config[ci,1]),"=",config[ci,2],"",sep="")
                  }
                } else {
                  txt = paste(as.character(config[ci,1]),"='",config[ci,2],"'",sep="")
                }
              }
            }
          }
          eval(parse(text=txt))
        }
      }
    }
  }
  # obtain default parameter values if not provided:

  # GENERAL parameters:
  if (exists("overwrite") == FALSE)   overwrite = FALSE
  if (exists("acc.metric") == FALSE)  acc.metric = "ENMO"
  if (exists("storefolderstructure") == FALSE)  storefolderstructure = FALSE
  if (exists("myfun") == FALSE)  myfun = c()

  if (exists("ignorenonwear") == FALSE)  ignorenonwear = TRUE
  if (exists("print.filename") == FALSE)  print.filename = FALSE
  if (exists("do.parallel") == FALSE)  do.parallel = TRUE
  # PART 1
  if (exists("selectdaysfile") == FALSE)  selectdaysfile = c()
  if (exists("do.cal") == FALSE)  do.cal = TRUE
  if (exists("printsummary") == FALSE)  printsummary = FALSE
  if (exists("windowsizes") == FALSE)  windowsizes = c(5,900,3600)
  if (exists("minloadcrit") == FALSE)  minloadcrit = 72
  if (exists("desiredtz") == FALSE)  desiredtz = ""
  if (exists("configtz") == FALSE)  configtz = c()
  if (exists("chunksize") == FALSE)  chunksize = 1
  if (exists("do.enmo") == FALSE)  do.enmo = TRUE
  if (exists("do.lfenmo") == FALSE)  do.lfenmo = FALSE
  if (exists("do.en") == FALSE)  do.en = FALSE
  if (exists("do.bfen") == FALSE)  do.bfen = FALSE
  if (exists("do.hfen") == FALSE)  do.hfen = FALSE
  if (exists("do.hfenplus") == FALSE)  do.hfenplus = FALSE
  if (exists("do.mad") == FALSE)  do.mad = FALSE
  if (exists("do.anglex") == FALSE)  do.anglex = FALSE
  if (exists("do.angley") == FALSE)  do.angley = FALSE
  if (exists("do.anglez") == FALSE)  do.anglez = FALSE
  if (exists("do.roll_med_acc_x") == FALSE)  do.roll_med_acc_x=FALSE
  if (exists("do.roll_med_acc_y") == FALSE)  do.roll_med_acc_y=FALSE
  if (exists("do.roll_med_acc_z") == FALSE)  do.roll_med_acc_z=FALSE
  if (exists("do.dev_roll_med_acc_x") == FALSE)  do.dev_roll_med_acc_x=FALSE
  if (exists("do.dev_roll_med_acc_y") == FALSE)  do.dev_roll_med_acc_y=FALSE
  if (exists("do.dev_roll_med_acc_z") == FALSE)  do.dev_roll_med_acc_z=FALSE
  if (exists("do.enmoa") == FALSE)  do.enmoa = FALSE
  if (exists("do.lfen") == FALSE)  do.lfen = FALSE
  if (exists("do.lfx") == FALSE)  do.lfx = FALSE
  if (exists("do.lfy") == FALSE)  do.lfy = FALSE
  if (exists("do.lfz") == FALSE)  do.lfz = FALSE
  if (exists("do.hfx") == FALSE)  do.hfx = FALSE
  if (exists("do.hfy") == FALSE)  do.hfy = FALSE
  if (exists("do.hfz") == FALSE)  do.hfz = FALSE
  if (exists("do.bfx") == FALSE)  do.bfx = FALSE
  if (exists("do.bfy") == FALSE)  do.bfy = FALSE
  if (exists("do.bfz") == FALSE)  do.bfz = FALSE
  if (exists("dynrange") == FALSE)  dynrange = c()
  if (exists("hb") == FALSE)  hb = 15
  if (exists("lb") == FALSE)  lb = 0.5
  if (exists("n") == FALSE)  n = 4
  if (exists("idloc") == FALSE) idloc = 1
  if (exists("backup.cal.coef") == FALSE)  backup.cal.coef = "retrieve"
  if (exists("minimumFileSizeMB") == FALSE)  minimumFileSizeMB = 2

  if (length(myfun) != 0) { # Run check on myfun object
    check_myfun(myfun, windowsizes)
  }

  # PART 2
  if (exists("strategy") == FALSE)  strategy = 1
  if (exists("maxdur") == FALSE)  maxdur = 0
  if (exists("hrs.del.start") == FALSE)  hrs.del.start = 0
  if (exists("hrs.del.end") == FALSE)  hrs.del.end = 0
  if (exists("includedaycrit") == FALSE)  includedaycrit = 16
  if (exists("M5L5res") == FALSE)  M5L5res = 10
  if (exists("winhr") == FALSE)  winhr = 5
  if (exists("qwindow") == FALSE)  qwindow = c(0,24)
  if (exists("qlevels") == FALSE)  qlevels = c()
  if (exists("ilevels") == FALSE)  ilevels = c()
  if (exists("mvpathreshold") == FALSE)  mvpathreshold = 100
  if (exists("boutcriter") == FALSE)  boutcriter = 0.8
  if (exists("ndayswindow") == FALSE)  ndayswindow = 7
  if (exists("do.imp") == FALSE) do.imp = TRUE
  if (exists("IVIS_windowsize_minutes") == FALSE)  IVIS_windowsize_minutes=60
  if (exists("IVIS_epochsize_seconds") == FALSE)  IVIS_epochsize_seconds=30
  if (exists("mvpadur") == FALSE)  mvpadur = c(1,5,10) # related to part 2 (functionality to anticipate part 5)
  if (exists("epochvalues2csv") == FALSE)  epochvalues2csv = FALSE
  if (exists("window.summary.size") == FALSE) window.summary.size = 10
  if (exists("dayborder") == FALSE)  dayborder = 0
  if (exists("iglevels") == FALSE)  iglevels = c()
  if (exists("TimeSegments2ZeroFile") == FALSE) TimeSegments2ZeroFile = c()
  if (exists("IVIS.activity.metric") == FALSE)  IVIS.activity.metric = 1
  if (exists("qM5L5") == FALSE)  qM5L5 = c()


  # PART 3
  if (exists("anglethreshold") == FALSE)  anglethreshold = 5
  if (exists("timethreshold") == FALSE)  timethreshold = 5
  if (exists("constrain2range") == FALSE) constrain2range = TRUE
  if (exists("do.part3.pdf") == FALSE) do.part3.pdf = TRUE

  # PART 4
  if (exists("loglocation") == FALSE)  loglocation = c()
  if (length(loglocation) == 1) {
    if (loglocation == "") loglocation = c() #inserted because some users mistakingly use this
  }
  if (exists("coldid") == FALSE)  colid=1
  if (exists("coln1") == FALSE)  coln1=1
  if (exists("nnights") == FALSE)  nnights=c()
  if (exists("outliers.only") == FALSE)  outliers.only=FALSE
  if (exists("excludefirstlast") == FALSE)  excludefirstlast=FALSE
  if (exists("criterror") == FALSE)  criterror=3
  if (exists("relyonguider") == FALSE)  relyonguider=FALSE
  if (exists("relyonsleeplog") == FALSE)  relyonsleeplog=c()
  if (exists("relyonsleeplog") == TRUE & exists("relyonguider") == FALSE)  relyonguider=relyonsleeplog
  if (exists("sleeplogidnum") == FALSE)  sleeplogidnum=TRUE
  if (exists("def.noc.sleep") == FALSE)  def.noc.sleep=1
  if (exists("do.visual") == FALSE)  do.visual=FALSE
  if (exists("data_cleaning_file") == FALSE) data_cleaning_file = c()
  if (exists("excludefirst.part4") == FALSE) excludefirst.part4 = FALSE
  if (exists("excludelast.part4") == FALSE)  excludelast.part4 = FALSE

  # PART 5
  if (exists("excludefirstlast.part5") == FALSE)  excludefirstlast.part5=FALSE
  if (exists("includenightcrit") == FALSE)  includenightcrit=16
  if (exists("bout.metric") == FALSE)  bout.metric = 4 # changed on 13-04-2020, because it is what we have been recommending all the time
  if (exists("closedbout") == FALSE)  closedbout = FALSE
  if (exists("boutcriter.in") == FALSE)  boutcriter.in = 0.9
  if (exists("boutcriter.lig") == FALSE)  boutcriter.lig = 0.8
  if (exists("boutcriter.mvpa") == FALSE)  boutcriter.mvpa = 0.8
  if (exists("threshold.lig") == FALSE)  threshold.lig = 40
  if (exists("threshold.mod") == FALSE)  threshold.mod = 100
  if (exists("threshold.vig") == FALSE)  threshold.vig = 400
  if (exists("timewindow") == FALSE)  timewindow = c("MM","WW")
  if (exists("boutdur.mvpa") == FALSE)  boutdur.mvpa = c(1,5,10)
  if (exists("boutdur.in") == FALSE)  boutdur.in = c(10,20,30)
  if (exists("boutdur.lig") == FALSE)  boutdur.lig = c(1,5,10)
  if (exists("save_ms5rawlevels") == FALSE) save_ms5rawlevels = FALSE
  if (exists("save_ms5raw_format") == FALSE) save_ms5raw_format = "csv"
  if (exists("save_ms5raw_without_invalid") == FALSE) save_ms5raw_without_invalid = TRUE
  if (exists("includedaycrit.part5") == FALSE) includedaycrit.part5 = 2/3
  if (exists("minimum_MM_length.part5") == FALSE) minimum_MM_length.part5 = 23
  if (exists("frag.classes.day") == FALSE) frag.classes.day = c()
  if (exists("frag.classes.spt") == FALSE) frag.classes.spt = c()
  if (exists("frag.metrics") == FALSE) frag.metrics = c()


  # Related to (r)ead (m)yacc (c)sv file:
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
  if (length(which(ls() == "rmc.desiredtz")) == 0) rmc.desiredtz= ""
  if (length(which(ls() == "rmc.sf")) == 0) rmc.sf  = c()
  if (length(which(ls() == "rmc.headername.sf")) == 0) rmc.headername.sf = c()
  if (length(which(ls() == "rmc.headername.sn")) == 0) rmc.headername.sn = c()
  if (length(which(ls() == "rmc.headername.recordingid")) == 0) rmc.headername.recordingid = c()
  if (length(which(ls() == "rmc.header.structure")) == 0) rmc.header.structure = c()
  if (length(which(ls() == "rmc.check4timegaps")) == 0) rmc.check4timegaps = FALSE
  if (length(which(ls() == "rmc.noise")) == 0) rmc.noise = FALSE
  if (length(which(ls() == "rmc.col.wear")) == 0) rmc.col.wear = c()
  if (length(which(ls() == "rmc.doresample")) == 0) rmc.doresample = FALSE
  if (length(which(ls() == "part5_agg2_60seconds")) == 0) part5_agg2_60seconds = FALSE


  # VISUAL REPORT

  if (exists("viewingwindow") == FALSE)  viewingwindow = 1
  if (exists("dofirstpage") == FALSE)  dofirstpage = TRUE
  if (exists("visualreport") == FALSE)  visualreport = FALSE

  cat("\n   Do not forget to cite GGIR in your publications via a version number and\n")
  cat("   Migueles et al. 2019 JMPB. doi: 10.1123/jmpb.2018-0063. \n")
  cat("   See also: https://cran.r-project.org/package=GGIR/vignettes/GGIR.html#citing-ggir \n")

  if (dopart1 == TRUE) {
    cat('\n')
    cat(paste0(rep('_',options()$width),collapse=''))
    cat("\nPart 1\n")
    g.part1(datadir = datadir, outputdir = outputdir,
            f0 = f0, f1 = f1, windowsizes = windowsizes,
            desiredtz = desiredtz, chunksize = chunksize,
            studyname = studyname, minloadcrit = minloadcrit,
            do.enmo = do.enmo,
            do.lfenmo = do.lfenmo, do.en = do.en,
            do.bfen = do.bfen, do.hfen=do.hfen,
            do.hfenplus = do.hfenplus, do.mad=do.mad,
            do.anglex=do.anglex,do.angley=do.angley,do.anglez=do.anglez,
            do.roll_med_acc_x=do.roll_med_acc_x,
            do.roll_med_acc_y=do.roll_med_acc_y,
            do.roll_med_acc_z=do.roll_med_acc_z,
            do.dev_roll_med_acc_x=do.dev_roll_med_acc_x,
            do.dev_roll_med_acc_y=do.dev_roll_med_acc_y,
            do.dev_roll_med_acc_z=do.dev_roll_med_acc_z,
            do.enmoa = do.enmoa,
            do.lfx=do.lfx, do.lfy=do.lfy, do.lfz=do.lfz,
            do.hfx=do.hfx, do.hfy=do.hfy, do.hfz=do.hfz,
            do.bfx=do.bfx, do.bfy=do.bfy, do.bfz=do.bfz,
            printsummary=printsummary,
            do.cal = do.cal,print.filename=print.filename,
            overwrite=overwrite,backup.cal.coef=backup.cal.coef,
            selectdaysfile=selectdaysfile,dayborder=dayborder,
            dynrange=dynrange, configtz=configtz, do.lfen=do.lfen, hb=hb, lb=lb, n=n,
            do.parallel = do.parallel, minimumFileSizeMB = minimumFileSizeMB,
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
            rmc.check4timegaps = rmc.check4timegaps, rmc.noise=rmc.noise,
            rmc.col.wear=rmc.col.wear,
            rmc.doresample=rmc.doresample,
            myfun=myfun)
  }
  if (dopart2 == TRUE) {
    cat('\n')
    cat(paste0(rep('_',options()$width),collapse=''))
    cat("\nPart 2\n")
    if (f1 == 0) f1 = length(dir(paste(metadatadir,"/meta/basic",sep="")))
    g.part2(datadir =datadir,metadatadir=metadatadir,f0=f0,f1=f1,strategy = strategy,
            hrs.del.start = hrs.del.start,hrs.del.end = hrs.del.end,
            maxdur =  maxdur, includedaycrit = includedaycrit,
            M5L5res = M5L5res, winhr = winhr,
            qwindow=qwindow, qlevels = qlevels,
            ilevels = ilevels, mvpathreshold = mvpathreshold,
            boutcriter = boutcriter,ndayswindow=ndayswindow,idloc=idloc,do.imp=do.imp,
            storefolderstructure=storefolderstructure,overwrite=overwrite,epochvalues2csv=epochvalues2csv,
            mvpadur=mvpadur,selectdaysfile=selectdaysfile,bout.metric=bout.metric,window.summary.size=window.summary.size,
            dayborder=dayborder,closedbout=closedbout,desiredtz=desiredtz,
            IVIS_windowsize_minutes = IVIS_windowsize_minutes,
            IVIS_epochsize_seconds = IVIS_epochsize_seconds, iglevels = iglevels,
            IVIS.activity.metric=IVIS.activity.metric, TimeSegments2ZeroFile = TimeSegments2ZeroFile,
            qM5L5=qM5L5, do.parallel = do.parallel, myfun=myfun)
  }
  if (dopart3 == TRUE) {
    cat('\n')
    cat(paste0(rep('_',options()$width),collapse=''))
    cat("\nPart 3\n")
    if (f1 == 0) f1 = length(dir(paste(metadatadir,"/meta/basic",sep="")))
    g.part3(metadatadir=metadatadir,f0=f0, acc.metric = acc.metric,
            f1=f1,anglethreshold=anglethreshold,timethreshold=timethreshold,
            ignorenonwear=ignorenonwear,overwrite=overwrite,desiredtz=desiredtz,
            constrain2range=constrain2range, do.parallel = do.parallel, dayborder=dayborder,
            myfun=myfun)
  }
  if (dopart4 == TRUE) {
    cat('\n')
    cat(paste0(rep('_',options()$width),collapse=''))
    cat("\nPart 4\n")
    if (f1 == 0) f1 = length(dir(paste(metadatadir,"/meta/ms3.out",sep="")))
    g.part4(datadir=datadir,metadatadir=metadatadir,loglocation = loglocation,
            f0=f0,f1=f1,idloc=idloc, colid = colid,coln1 = coln1,nnights = nnights,
            outliers.only = outliers.only,
            excludefirstlast=excludefirstlast,criterror = criterror,
            includenightcrit=includenightcrit,relyonguider=relyonguider,
            sleeplogidnum=sleeplogidnum,def.noc.sleep=def.noc.sleep,do.visual = do.visual, #
            storefolderstructure=storefolderstructure,overwrite=overwrite,desiredtz=desiredtz,
            data_cleaning_file=data_cleaning_file,
            excludefirst.part4= excludefirst.part4,excludelast.part4=excludelast.part4)
  }
  if (dopart5 == TRUE) {
    cat('\n')
    cat(paste0(rep('_',options()$width),collapse=''))
    cat("\nPart 5\n")
    if (f1 == 0) f1 = length(dir(paste(metadatadir,"/meta/ms4.out",sep="")))
    g.part5(datadir=datadir,metadatadir=metadatadir,f0=f0,f1=f1,strategy=strategy,maxdur=maxdur,
            hrs.del.start=hrs.del.start,
            hrs.del.end=hrs.del.end,
            loglocation=loglocation,excludefirstlast.part5=excludefirstlast.part5, acc.metric=acc.metric,
            windowsizes=windowsizes,boutcriter.in=boutcriter.in,boutcriter.lig=boutcriter.lig,
            boutcriter.mvpa=boutcriter.mvpa,storefolderstructure=storefolderstructure,
            threshold.lig = threshold.lig,
            threshold.mod = threshold.mod,
            threshold.vig = threshold.vig,timewindow=timewindow,
            boutdur.mvpa = boutdur.mvpa,
            boutdur.in = boutdur.in,
            boutdur.lig = boutdur.lig,
            winhr = winhr,M5L5res = M5L5res,
            overwrite=overwrite,desiredtz=desiredtz,dayborder=dayborder,
            save_ms5rawlevels = save_ms5rawlevels, do.parallel = do.parallel,
            part5_agg2_60seconds=part5_agg2_60seconds, save_ms5raw_format=save_ms5raw_format,
            save_ms5raw_without_invalid=save_ms5raw_without_invalid,
            frag.classes.day = frag.classes.day, frag.classes.spt = frag.classes.spt,
            frag.metrics = frag.metrics,
            data_cleaning_file=data_cleaning_file,
            includedaycrit.part5=includedaycrit.part5)
  }
  #--------------------------------------------------
  # Store configuration parameters in config file
  LS = ls()
  LS = LS[which(LS %in% c("input", "txt", "derivef0f1", "dopart1", "dopart2", "dopart3", "LS",
                          "dopart4", "dopart5", "fnames", "useRDA", "metadatadir", "ci", "config",
                          "configfile", "filelist", "outputfoldername", "numi", "logi",
                          "conv2logical", "conv2num") == FALSE)]
  config.parameters = mget(LS) #lapply(mget(ls()), is.data.frame)
  config.matrix = createConfigFile(config.parameters)
  if (dir.exists(metadatadir)) {
    write.csv(config.matrix, file = paste0(metadatadir,"/config.csv"), row.names = FALSE)
  } else {
    if (dir.exists(datadir) == FALSE) {
      warning("\nCould not write config file because studyname or datadir are not correctly specified.")
    }
  }
  #==========================
  # Report generation:
  # check a few basic assumptions before continuing
  if (length(which(do.report==4 | do.report==5)) > 0 | visualreport==TRUE) {
    if (file.exists(paste(metadatadir,"/meta/ms4.out",sep=""))) {
    } else {
      cat("Warning: First run g.shell.GGIR with mode = 4 to generate required milestone data\n")
      cat("before you can use argument visualreport or create a report for part 4\n")
      # stop()
    }
  }
  if (length(which(do.report == 2)) > 0) {
    cat('\n')
    cat(paste0(rep('_',options()$width),collapse=''))
    cat("\nReport part 2\n")
    N.files.ms2.out = length(dir(paste(metadatadir,"/meta/ms2.out",sep="")))
    if (N.files.ms2.out < f0) f0 = 1
    if (N.files.ms2.out < f1) f1 = N.files.ms2.out
    if (f1 == 0) f1 = N.files.ms2.out
    g.report.part2(metadatadir=metadatadir,f0=f0,f1=f1,maxdur=maxdur,selectdaysfile=selectdaysfile)
  }
  if (length(which(do.report == 4)) > 0) {
    cat('\n')
    cat(paste0(rep('_',options()$width),collapse=''))
    cat("\nReport part 4\n")
    N.files.ms4.out = length(dir(paste(metadatadir,"/meta/ms4.out",sep="")))
    if (N.files.ms4.out < f0) f0 = 1
    if (N.files.ms4.out < f1) f1 = N.files.ms4.out
    if (f1 == 0) f1 = N.files.ms4.out
    g.report.part4(datadir=datadir,metadatadir=metadatadir,loglocation =loglocation,f0=f0,f1=f1,
                   storefolderstructure=storefolderstructure, data_cleaning_file=data_cleaning_file)
  }
  if (length(which(do.report == 5)) > 0) {
    cat('\n')
    cat(paste0(rep('_',options()$width),collapse=''))
    cat("\nReport part 5\n")
    N.files.ms5.out = length(dir(paste(metadatadir,"/meta/ms5.out",sep="")))
    if (N.files.ms5.out < f0) f0 = 1
    if (N.files.ms5.out < f1) f1 = N.files.ms5.out
    if (f1 == 0) f1 = N.files.ms5.out

    g.report.part5(metadatadir=metadatadir,f0=f0,f1=f1,loglocation=loglocation,
                   includenightcrit=includenightcrit,includedaycrit=includedaycrit,
                   data_cleaning_file=data_cleaning_file, includedaycrit.part5=includedaycrit.part5,
                   minimum_MM_length.part5=minimum_MM_length.part5)
  }
  if (visualreport == TRUE) {
    cat('\n')
    cat(paste0(rep('_',options()$width),collapse=''))
    cat("\nGenerate visual reports\n")
    # f1 = length(dir(paste(metadatadir,"/meta/ms4.out",sep=""))) # Note: I have moved this line to the g.plot5 function.
    g.plot5(metadatadir=metadatadir,dofirstpage=dofirstpage,
            viewingwindow=viewingwindow,f0=f0,f1=f1,overwrite=overwrite,desiredtz = desiredtz,
            metric=acc.metric,threshold.lig,threshold.mod,threshold.vig)
  }
}
