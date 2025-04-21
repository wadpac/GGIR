g.getmeta = function(datafile, params_metrics = c(), params_rawdata = c(),
                     params_general = c(), params_cleaning = c(), daylimit = FALSE,
                     offset = c(0, 0, 0), scale = c(1, 1, 1), tempoffset = c(0, 0, 0),
                     meantempcal = c(), myfun = c(),
                     inspectfileobject = c(),
                     verbose = TRUE, ...) {
  
  #get input variables
  input = list(...)
  if (length(input) > 0 ||
      length(params_metrics) == 0 || length(params_rawdata) == 0 ||
      length(params_general) == 0 || length(params_cleaning) == 0) {
    # Extract and check parameters if user provides more arguments than just the parameter arguments,
    # or if params_[...] aren't specified (so need to be filled with defaults).
    # So, inside GGIR this will not be used, but it is used when g.getmeta is used on its own
    # as if it was still the old g.getmeta function
    params = extract_params(params_metrics = params_metrics,
                            params_rawdata = params_rawdata,
                            params_general = params_general,
                            params_cleaning = params_cleaning,
                            input = input,
                            params2check = c("metrics", "rawdata", "general", "cleaning")) # load default parameters
    params_metrics = params$params_metrics
    params_rawdata = params$params_rawdata
    params_general = params$params_general
    params_cleaning = params$params_cleaning
  }
  
  metrics2do = data.frame(do.bfen = params_metrics[["do.bfen"]],
                          do.enmo = params_metrics[["do.enmo"]],
                          do.lfenmo = params_metrics[["do.lfenmo"]],
                          do.en = params_metrics[["do.en"]],
                          do.hfen = params_metrics[["do.hfen"]],
                          do.hfenplus = params_metrics[["do.hfenplus"]],
                          do.mad = params_metrics[["do.mad"]],
                          do.anglex = params_metrics[["do.anglex"]],
                          do.angley = params_metrics[["do.angley"]],
                          do.anglez = params_metrics[["do.anglez"]],
                          do.roll_med_acc_x = params_metrics[["do.roll_med_acc_x"]],
                          do.roll_med_acc_y = params_metrics[["do.roll_med_acc_y"]],
                          do.roll_med_acc_z = params_metrics[["do.roll_med_acc_z"]],
                          do.dev_roll_med_acc_x = params_metrics[["do.dev_roll_med_acc_x"]],
                          do.dev_roll_med_acc_y = params_metrics[["do.dev_roll_med_acc_y"]],
                          do.dev_roll_med_acc_z = params_metrics[["do.dev_roll_med_acc_z"]],
                          do.enmoa = params_metrics[["do.enmoa"]],
                          do.lfen = params_metrics[["do.lfen"]],
                          do.lfx = params_metrics[["do.lfx"]],
                          do.lfy = params_metrics[["do.lfy"]],
                          do.lfz = params_metrics[["do.lfz"]],
                          do.hfx = params_metrics[["do.hfx"]],
                          do.hfy = params_metrics[["do.hfy"]],
                          do.hfz = params_metrics[["do.hfz"]],
                          do.bfx = params_metrics[["do.bfx"]],
                          do.bfy = params_metrics[["do.bfy"]],
                          do.bfz = params_metrics[["do.bfz"]],
                          do.zcx = params_metrics[["do.zcx"]],
                          do.zcy = params_metrics[["do.zcy"]],
                          do.zcz = params_metrics[["do.zcz"]],
                          do.brondcounts = params_metrics[["do.brondcounts"]],
                          do.neishabouricounts = params_metrics[["do.neishabouricounts"]],
                          stringsAsFactors = FALSE)
  
  nmetrics = sum(c(params_metrics[["do.bfen"]], params_metrics[["do.enmo"]],
                   params_metrics[["do.lfenmo"]], params_metrics[["do.en"]],
                   params_metrics[["do.hfen"]], params_metrics[["do.hfenplus"]],
                   params_metrics[["do.mad"]], params_metrics[["do.anglex"]],
                   params_metrics[["do.angley"]], params_metrics[["do.anglez"]],
                   params_metrics[["do.roll_med_acc_x"]], params_metrics[["do.roll_med_acc_y"]],
                   params_metrics[["do.roll_med_acc_z"]],
                   params_metrics[["do.dev_roll_med_acc_x"]], params_metrics[["do.dev_roll_med_acc_y"]],
                   params_metrics[["do.dev_roll_med_acc_z"]],
                   params_metrics[["do.enmoa"]], params_metrics[["do.lfen"]],
                   params_metrics[["do.lfx"]], params_metrics[["do.lfy"]],
                   params_metrics[["do.lfz"]],  params_metrics[["do.hfx"]],
                   params_metrics[["do.hfy"]], params_metrics[["do.hfz"]],
                   params_metrics[["do.bfx"]], params_metrics[["do.bfy"]],
                   params_metrics[["do.bfz"]],
                   params_metrics[["do.zcx"]], params_metrics[["do.zcy"]],
                   params_metrics[["do.zcz"]], params_metrics[["do.brondcounts"]] * 3,
                   params_metrics[["do.neishabouricounts"]] * 4))
  if (length(myfun) != 0) {
    nmetrics = nmetrics + length(myfun$colnames)
    # check myfun object already, because we do not want to discover
    # bugs after waiting for the data to load
    check_myfun(myfun, params_general[["windowsizes"]])
  }
  
  if (length(nmetrics) == 0) {
    warning("No metrics selected.", call. = FALSE)
  }

  ws3 = params_general[["windowsizes"]][1]; ws2 = params_general[["windowsizes"]][2]; ws = params_general[["windowsizes"]][3]

  PreviousEndPage = c()
  
  filequality = data.frame(filetooshort = FALSE, filecorrupt = FALSE,
                           filedoesnotholdday = FALSE, NFilePagesSkipped = 0)
  filetooshort = FALSE
  filecorrupt = FALSE
  filedoesnotholdday = FALSE
  NFilePagesSkipped = 0
  
  i = 1 #counter to keep track of which binary block is being read
  count = 1 #counter to keep track of the number of seconds that have been read
  count2 = 1 #count number of blocks read with length "ws2" (long epoch, 15 minutes by default)
  LD = 2 #dummy variable used to identify end of file and to make the process stop
  bsc_qc = data.frame(time = c(), size = c(), stringsAsFactors = FALSE)
  
  if (length(inspectfileobject) > 0) {
    INFI = inspectfileobject
  } else {
    stop("argument inspectfileobject not specified")
  }
  
  mon = INFI$monc
  dformat = INFI$dformc
  sf = INFI$sf

  if (is.null(sf)) { # sf is NULL for corrupt files
    return(invisible(list(filecorrupt = TRUE, filetooshort = FALSE, NFilePagesSkipped = 0,
                     metalong = c(), metashort = c(), wday = c(), wdayname = c(),
                     windowsizes = c(), bsc_qc = bsc_qc, QClog = NULL)))
  }

  hvars = g.extractheadervars(INFI)
  deviceSerialNumber = hvars$deviceSerialNumber

  # get now-wear, clip, and blocksize parameters (thresholds)
  if (mon == MONITOR$PARMAY_MTX) params_rawdata[["dynrange"]] = GGIRread::readParmayMatrix(datafile, output = "dynrange")
  ncb_params = get_nw_clip_block_params(monc = mon, dformat = dformat,
                                        deviceSerialNumber = deviceSerialNumber,
                                        sf = sf,
                                        params_rawdata = params_rawdata)
  clipthres = ncb_params$clipthres
  blocksize = ncb_params$blocksize
  sdcriter = ncb_params$sdcriter
  racriter = ncb_params$racriter
  n_decimal_places = 4 # number of decimal places to which features should be rounded
  # creating matrices for storing output
  S = matrix(0,0,4) #dummy variable needed to cope with head-tailing succeeding blocks of data
  nev = 80*10^7 # number expected values
  NR = ceiling(nev / (sf*ws3)) + 1000 #NR = number of 'ws3' second rows (this is for 10 days at 80 Hz)
  metashort = matrix(" ",NR,(1 + nmetrics)) #generating output matrix for acceleration signal
  QClog = NULL

  #===============================================
  # Read file
  isLastBlock = FALSE # dummy variable part "end of loop mechanism"

  PreviousLastValue = c(0, 0, 1)
  PreviousLastTime = NULL
  header = NULL

  while (LD > 1) {
    if (verbose == TRUE) {
      if (i  == 1) {
        cat(paste0("\nLoading chunk: ", i))
      } else {
        cat(paste0(" ", i))
      }
    }
    
    accread = g.readaccfile(filename = datafile, blocksize = blocksize, blocknumber = i,
                            filequality = filequality,
                            ws = ws, PreviousEndPage = PreviousEndPage,
                            inspectfileobject = INFI,
                            PreviousLastValue = PreviousLastValue,
                            PreviousLastTime = PreviousLastTime,
                            params_rawdata = params_rawdata, params_general = params_general, 
                            header = header)
    header = accread$header

    if ("PreviousLastValue" %in% names(accread$P)) { # output when reading ad-hoc csv
      PreviousLastValue = accread$P$PreviousLastValue
      PreviousLastTime = accread$P$PreviousLastTime
    } 
        
    filequality = accread$filequality
    filetooshort = filequality$filetooshort
    filecorrupt = filequality$filecorrupt
    filedoesnotholdday = filequality$filedoesnotholdday
    NFilePagesSkipped = filequality$NFilePagesSkipped
    isLastBlock = accread$isLastBlock
    PreviousEndPage = accread$endpage
    
    #============
    #process data as read from binary file
    if (length(accread$P) > 0) { # would have been set to zero if file was corrupt or empty
      data = accread$P$data
      QClog = rbind(QClog, accread$P$QClog)
      rm(accread)

      if (i == 1) {
        light.available = ("light" %in% colnames(data))

        use.temp = ("temperature" %in% colnames(data))
        if (use.temp) {
          if (mean(data$temperature[1:10], na.rm = TRUE) > 50) {
            warning("temperature value is unreaslistically high (> 50 Celcius) and will not be used.", call. = FALSE)
            use.temp = FALSE
          }
        }

        # output matrix for 15 minutes summaries
        if (!use.temp && !light.available) {
          metalong = matrix(" ", ((nev/(sf*ws2)) + 100), 4)
          metricnames_long = c("timestamp","nonwearscore","clippingscore","EN")
        } else if (use.temp && !light.available) {
          metalong = matrix(" ", ((nev/(sf*ws2)) + 100), 5)
          metricnames_long = c("timestamp","nonwearscore","clippingscore","temperaturemean","EN")
        } else if (!use.temp && light.available) {
          metalong = matrix(" ", ((nev/(sf*ws2)) + 100), 6)
          metricnames_long = c("timestamp","nonwearscore","clippingscore","lightmean","lightpeak","EN")
        } else if (use.temp && light.available) {
          metalong = matrix(" ", ((nev/(sf*ws2)) + 100), 7)
          metricnames_long = c("timestamp","nonwearscore","clippingscore","lightmean","lightpeak","temperaturemean","EN")
        }
      }

      if (params_rawdata[["imputeTimegaps"]] && (dformat == FORMAT$CSV ||
                                                 dformat == FORMAT$AD_HOC_CSV ||
                                                 dformat == FORMAT$GT3X)) {
        P = g.imputeTimegaps(data, sf = sf, k = 0.25,
                             PreviousLastValue = PreviousLastValue,
                             PreviousLastTime = PreviousLastTime,
                             epochsize = c(ws3, ws2))
        data = P$x
        PreviousLastValue = data[nrow(data), c("x", "y", "z")]
        if ("time" %in% colnames(data)) {
          PreviousLastTime = as.POSIXct(data$time[nrow(data)], origin = "1970-1-1")
        } else {
          PreviousLastTime = NULL
        }
        QClog = rbind(QClog, P$QClog)
        rm(P)
      }

      gc()

      data = as.matrix(data, rownames.force = FALSE)
      #add leftover data from last time
      if (nrow(S) > 0) {
        if (params_rawdata[["imputeTimegaps"]]) {
          if ("remaining_epochs" %in% colnames(data)) {
            if (ncol(S) == (ncol(data) - 1)) {
              # this block has time gaps while the previous block did not
              S = cbind(S, 1)
              colnames(S)[ncol(S)] = "remaining_epochs"
            }
          } else if ("remaining_epochs" %in% colnames(S)) {
            if ((ncol(S) - 1) == ncol(data)) {
              # this block does not have time gaps while the previous blog did
              data = cbind(data, 1)
              colnames(data)[ncol(S)] = "remaining_epochs"
            }
          }
        }
        data = rbind(S,data)
      }
      if (i == 1) {
        SWMT = get_starttime_weekday_truncdata(mon, dformat,
                                               data, header, desiredtz = params_general[["desiredtz"]],
                                               sf, datafile,  ws2,
                                               configtz = params_general[["configtz"]])
        starttime = SWMT$starttime
        wday = SWMT$wday; wdayname = SWMT$wdayname
        data = SWMT$data
        
        rm(SWMT)
      }

      LD = nrow(data)
      if (LD < (ws*sf) && i == 1) {
        warning('\nWarning data too short for doing non-wear detection 3\n')
        isLastBlock = TRUE
        LD = 0 #ignore rest of the data and store what has been loaded so far.
      }
      
      #store data that could not be used for this block, but will be added to next block
      if (LD >= (ws*sf)) {
        
        use = (floor(LD / (ws2*sf))) * (ws2*sf) #number of datapoint to use # changes from ws to ws2 Vvh 23/4/2017
        if (length(myfun) != 0) { # if using external function, then check that use is a multitude of the expected windowlength
          Nminlength = use / myfun$minlength
          if (Nminlength != floor(Nminlength)) { # it is not a multitude
            use = floor(Nminlength) * myfun$minlength # correct use accordingly
          }
        }
        if ((LD - use) > 1) {
          S = data[(use + 1):LD,] #store leftover data
          if (ncol(S) == 1) {
            S = t(S)
          }
        } else { #use all data
          S = matrix(0, 0, ncol(data))
        }
        data = data[1:use,]
        LD = nrow(data) #redefine LD because there is less data
        if ("remaining_epochs" %in% colnames(data)) { #
          # remove remaining_epochs from data object and keep it seperately
          remaining_epochs = data[,"remaining_epochs"]
          data = data[, -which(colnames(data) == "remaining_epochs")]
        }
        ##==================================================
        # Feature calculation
        dur = nrow(data)	#duration of experiment in data points
        durexp = nrow(data) / (sf*ws)	#duration of experiment in hrs
        #--------------------------------------------
        temperature = light = c()
        if (light.available) {
          light = data[, "light"]
        }
        if (use.temp) {
          temperature = data[, "temperature"]
        }

        # rescale data
        data[, c("x", "y", "z")] = scale(data[, c("x", "y", "z")], center = -offset, scale = 1/scale)
        if (use.temp && length(meantempcal) > 0) {
          yy = cbind(temperature,
                     temperature,
                     temperature)
          data[, c("x", "y", "z")] = data[, c("x", "y", "z")] + scale(yy, center = rep(meantempcal,3), scale = 1/tempoffset)
        }

        EN = sqrt(data[, "x"]^2 + data[, "y"]^2 + data[, "z"]^2) # Do not delete Used for long epoch calculation
        accmetrics = g.applymetrics(data = data,
                                    sf = sf, ws3 = ws3,
                                    metrics2do = metrics2do,
                                    n = params_metrics[["n"]],
                                    lb = params_metrics[["lb"]],
                                    hb = params_metrics[["hb"]],
                                    zc.lb = params_metrics[["zc.lb"]],
                                    zc.hb = params_metrics[["zc.hb"]],
                                    zc.sb = params_metrics[["zc.sb"]],
                                    zc.order = params_metrics[["zc.order"]],
                                    actilife_LFE = params_metrics[["actilife_LFE"]])
        # round decimal places, because due to averaging we get a lot of information
        # that only slows down computation and increases storage size
        accmetrics = lapply(accmetrics, round, n_decimal_places)
        accmetrics = data.frame(sapply(accmetrics,c)) # collapse to data.frame
        # update LD in case data has been imputed at epoch level
        if (floor(LD / (ws3 * sf)) < nrow(accmetrics)) { # then, data has been imputed
          LD = nrow(accmetrics) * ws3 * sf
        }
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
          OutputExternalFunction = applyExtFunction(data, myfun, sf, ws3, interpolationType = params_rawdata[["interpolationType"]])
        }
      }
      if (LD >= (ws*sf)) { #LD != 0
        #-----------------------------------------------------
        #extend metashort and metalong if it is expected to be too short
        if (exists("remaining_epochs")) {
          totalgap = sum(remaining_epochs[which(remaining_epochs != 1)])
        } else {
          totalgap = 0
        }
        if (count > (nrow(metashort) - ((2.5*(3600/ws3) * 24)) + totalgap)) {
          extension = matrix(" ", ((3600/ws3) * 24) + totalgap, ncol(metashort)) #add another day to metashort once you reach the end of it
          metashort = rbind(metashort,extension)
          extension2 = matrix(" ", ((3600/ws2) * 24)  + (totalgap * (ws2/ws3)), ncol(metalong)) #add another day to metashort once you reach the end of it
          metalong = rbind(metalong, extension2)
        }
        col_msi = 2
        # Add metric time series to metashort object
        metnames = grep(pattern = "BrondCount|NeishabouriCount", x = names(accmetrics), invert = TRUE, value = TRUE)
        for (metnam in metnames) {
          dovalue = paste0("do.",tolower(metnam))
          dovalue = gsub(pattern = "angle_", replacement = "angle", x = dovalue)
          if (params_metrics[[dovalue]] == TRUE) {
            metashort[count:(count - 1 + length(accmetrics[[metnam]])), col_msi] = accmetrics[[metnam]]
            col_msi = col_msi + 1
          }
        }
        if (params_metrics[["do.brondcounts"]] == TRUE) {
          metashort[count:(count - 1 + length(accmetrics$BrondCount_x)), col_msi] = accmetrics$BrondCount_x
          metashort[count:(count - 1 + length(accmetrics$BrondCount_y)), col_msi + 1] = accmetrics$BrondCount_y
          metashort[count:(count - 1 + length(accmetrics$BrondCount_z)), col_msi + 2] = accmetrics$BrondCount_z
          col_msi = col_msi + 3
          metnames = c(metnames, "BrondCount_x", "BrondCount_y", "BrondCount_z")
        }
        if (params_metrics[["do.neishabouricounts"]] == TRUE) {
          metashort[count:(count - 1 + length(accmetrics$NeishabouriCount_x)), col_msi] = accmetrics$NeishabouriCount_x
          metashort[count:(count - 1 + length(accmetrics$NeishabouriCount_y)), col_msi + 1] = accmetrics$NeishabouriCount_y
          metashort[count:(count - 1 + length(accmetrics$NeishabouriCount_z)), col_msi + 2] = accmetrics$NeishabouriCount_z
          metashort[count:(count - 1 + length(accmetrics$NeishabouriCount_vm)), col_msi + 3] = accmetrics$NeishabouriCount_vm
          col_msi = col_msi + 3
          metnames = c(metnames, "NeishabouriCount_x", "NeishabouriCount_y", "NeishabouriCount_z", "NeishabouriCount_vm")
        }
        metnames = gsub(pattern = "angle_", replacement = "angle", x = metnames)
        if (length(myfun) != 0) { # if an external function is applied.
          NcolEF = ncol(OutputExternalFunction) - 1 # number of extra columns needed
          metashort[count:(count - 1 + nrow(OutputExternalFunction)), col_msi:(col_msi + NcolEF)] = as.matrix(OutputExternalFunction); col_msi = col_msi + NcolEF + 1
        }
        
        length_acc_metrics =  length(accmetrics[[1]]) # changing indicator to whatever metric is calculated, EN produces incompatibility when deriving both ENMO and ENMOa
        rm(accmetrics)
        # update blocksize depending on available memory
        BlocksizeNew = updateBlocksize(blocksize = blocksize, bsc_qc = bsc_qc)
        bsc_qc = BlocksizeNew$bsc_qc
        blocksize = BlocksizeNew$blocksize
        ##==================================================
        # MODULE 2 - non-wear time & clipping
        NWCW = detect_nonwear_clipping(data = data, windowsizes = c(ws3, ws2, ws), sf = sf,
                                       clipthres = clipthres, sdcriter = sdcriter, racriter = racriter,
                                       nonwear_approach = params_cleaning[["nonwear_approach"]],
                                       params_rawdata = params_rawdata)
        NWav = NWCW$NWav; CWav = NWCW$CWav; nmin = NWCW$nmin
        # metalong
        col_mli = 2
        metalong[count2:((count2 - 1) + length(NWav)),col_mli] = NWav; col_mli = col_mli + 1
        metalong[count2:((count2 - 1) + length(NWav)),col_mli] = CWav; col_mli = col_mli + 1

        if(light.available) {
          #light (running mean)
          lightc = cumsum(c(0,light))
          select = seq(1, length(lightc), by = (ws2 * sf))
          lightmean = diff(lightc[round(select)]) / abs(diff(round(select)))
          rm(lightc)
          #light (running max)
          lightmax = matrix(0, length(lightmean), 1)
          for (li in 1:(length(light)/(ws2*sf))) {
            tempm = max(light[((li - 1) * (ws2 * sf)):(li * (ws2 * sf))])
            if (length(tempm) > 0) {
              lightmax[li] = tempm[1]
            } else {
              lightmax[li] = max(light[((li - 1) * (ws2 * sf)):(li * (ws2 * sf))])
            }
          }

          metalong[(count2):((count2 - 1) + length(NWav)), col_mli] = round(lightmean, digits = n_decimal_places)
          col_mli = col_mli + 1
          metalong[(count2):((count2 - 1) + length(NWav)), col_mli] = round(lightmax, digits = n_decimal_places)
          col_mli = col_mli + 1
        }

        if(use.temp) {
          #temperature (running mean)
          temperaturec = cumsum(c(0, temperature))
          select = seq(1, length(temperaturec), by = (ws2 * sf))
          temperatureb = diff(temperaturec[round(select)]) / abs(diff(round(select)))
          rm(temperaturec)

          metalong[(count2):((count2 - 1) + length(NWav)), col_mli] = round(temperatureb, digits = n_decimal_places)
          col_mli = col_mli + 1
        }

        #EN going from sample to ws2
        ENc = cumsum(c(0, EN))
        select = seq(1, length(ENc), by = (ws2 * sf)) #<= EN is derived from data, so it needs the new sf
        ENb = diff(ENc[round(select)]) / abs(diff(round(select)))
        rm(ENc, EN)
        metalong[(count2):((count2 - 1) + length(NWav)), col_mli] = round(ENb, digits = n_decimal_places)

        if (exists("remaining_epochs")) {
          # Impute long gaps at epoch levels, because imputing them at raw level would
          # be too memory hungry
          impute_at_epoch_level = function(gapsize, timeseries, gap_index, metnames) {
            # gap_index: where do gaps occur (epoch indexing)
            # gap_size: how long is gap (epoch numbers)
            if (any(duplicated(gap_index))) {
              # When 2 gap_index are within the same epoch (either short or long)
              # we would have a duplicated gap_index here, then combine information
              dup_index_tmp = which(duplicated(gap_index))
              dup_index = gap_index[dup_index_tmp]
              for (dup_index_i in dup_index) {
                to_combine = which(gap_index == dup_index_i)
                length_to_combine = length(to_combine) # In the unlikely event that a gap_index appears more than 2, this should be able to deal with it.
                delete = to_combine[-1] # leave only the first index and remove duplicates
                gap_index = gap_index[-delete] # remove from gap index
                gapsize[to_combine[1]] = sum(gapsize[to_combine]) - (length_to_combine - 1) # minus 1 because it was summed 1 to each gapsize (which is +2 when it is duplicated) in the function call
                gapsize = gapsize[-delete]
              }
            }
            if ("nonwearscore" %in% metnames) {
              timeseries[gap_index, which(metnames == "nonwearscore")]  = 3
            } else {
              # set all features to zero except time and angle feature
              timeseries[gap_index, grep(pattern = "time|angle",
                                         x = metnames, invert = TRUE, value = FALSE)] = 0
              # set EN to 1 if it is available
              if ("EN" %in% metnames) timeseries[gap_index, which(metnames == "EN")] = 1
            }
            N_time = nrow(timeseries)
            newindi = rep(1, N_time)
            newindi[gap_index] = as.numeric(gapsize)
            newindi = rep(1:N_time, newindi)
            timeseries = timeseries[newindi,]
            return(timeseries)
          }
          gaps_to_fill = which(remaining_epochs != 1)
          if (length(gaps_to_fill) > 0) {
            nr_before = c(nrow(metalong), nrow(metashort))
            # metalong
            metalong = impute_at_epoch_level(gapsize = floor(remaining_epochs[gaps_to_fill] * (ws3/ws2)) + 1, # plus 1 needed to count for current epoch
                                             timeseries = metalong,
                                             gap_index = floor(gaps_to_fill / (ws2 * sf)) + count2, # Using floor so that the gap is filled in the epoch in which it is occurring
                                             metnames = metricnames_long)
            # metashort
            # added epoch-level nonwear to metashort to get it imputed, then remove it
            metashort = impute_at_epoch_level(gapsize = remaining_epochs[gaps_to_fill], # gapsize in epochs
                                              timeseries = metashort,
                                              gap_index = floor(gaps_to_fill / (ws3 * sf)) + count, # Using floor so that the gap is filled in the epoch in which it is occurring
                                              metnames = c("timestamp", metnames)) # epoch level index of gap
            nr_after = c(nrow(metalong), nrow(metashort))
            count2 = count2 + (nr_after[1] - nr_before[1])
            count = count + (nr_after[2] - nr_before[2])
          }
        }
        col_mli = col_mli + 1
        count2 = count2 + nmin
        count = count + length_acc_metrics
        if (exists("data")) rm(data)
        if (exists("light")) rm(light)
        if (exists("temperature")) rm(temperature)
        gc()
      } #end of section which is skipped when switchoff == 1
    } else {
      LD = 0 #once LD < 1 the analysis stops, so this is a trick to stop it
      # stop reading because there is not enough data in this block
    }
    if (isLastBlock) LD = 0
    if (ceiling(daylimit) != FALSE) {
      if (i == ceiling(daylimit)) { #to speed up testing only read first 'i' blocks of data
        LD = 0 #once LD < 1 the analysis stops, so this is a trick to stop it
        if (verbose == TRUE) cat(paste0("\nstopped reading data because this analysis is limited to ", ceiling(daylimit), " days\n"))
      }
    }
    i = i + 1 #go to next block
  }
  # deriving timestamps
  if (!filecorrupt && !filetooshort && !filedoesnotholdday) {
    cut = count:nrow(metashort)
    if (length(cut) > 1) {
      metashort = metashort[-cut,]
      # for a very small file, there could be just one row in metashort[-cut,], so it gets coerced to a vector.
      # But what we actually need is a 1-row matrix. So we need to transpose it. 
      if(is.vector(metashort)) { 
        metashort = as.matrix(t(metashort))
      }
    }
    if (nrow(metashort) > 1) {
      starttime3 = round(as.numeric(starttime)) #numeric time but relative to the desiredtz
      time5 = seq(starttime3, (starttime3 + ((nrow(metashort) - 1) * ws3)), by = ws3)
      time6 = as.POSIXlt(time5,origin = "1970-01-01", tz = params_general[["desiredtz"]])
      time6 = strftime(time6, format = "%Y-%m-%dT%H:%M:%S%z")
      metashort[,1] = as.character(time6)
    }
    cut2 = count2:nrow(metalong)
    if (length(cut2) > 1) {
      metalong = metalong[-cut2,]
      # for a very small file, there could be just one row in metalong[-cut2,], so it gets coerced to a vector.
      # But what we actually need is a 1-row matrix. So we need to transpose it. 
      if(is.vector(metalong)) { 
        metalong = as.matrix(t(metalong))
      }
    }
    if (nrow(metalong) > 2) {
      starttime4 = round(as.numeric(starttime)) #numeric time but relative to the desiredtz
      time1 = seq(starttime4,(starttime4 + (nrow(metalong) * ws2) - 1), by = ws2)
      if (length(params_general[["desiredtz"]]) == 0) {
        warning("desiredtz not specified, system timezone used as default")
        params_general[["desiredtz"]] = ""
      }
      time2 = as.POSIXlt(time1, origin = "1970-01-01", tz = params_general[["desiredtz"]])
      time2 = strftime(time2, format = "%Y-%m-%dT%H:%M:%S%z")
      metalong[, 1] = as.character(time2)
    }
    metricnames_short = c("timestamp", metnames)
    
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
    
    metalong = data.frame(A = metalong, stringsAsFactors = FALSE)
    names(metalong) = metricnames_long
    for (ncolml in 2:ncol(metalong)) {
      metalong[,ncolml] = as.numeric(metalong[,ncolml])
    }
  } else {
    metalong = metashort = wday = wdayname = params_general[["windowsizes"]] = c()
  }
  if (length(metashort) == 0 | filedoesnotholdday == TRUE) filetooshort = TRUE
  invisible(list(filecorrupt = filecorrupt, filetooshort = filetooshort, NFilePagesSkipped = NFilePagesSkipped,
                 metalong = metalong, metashort = metashort, wday = wday, wdayname = wdayname,
                 windowsizes = params_general[["windowsizes"]], bsc_qc = bsc_qc, QClog = QClog))
}
