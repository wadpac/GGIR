part6AlignIndividuals = function(GGIR_ts_dir = NULL, outputdir = NULL,
                                 path_ggirms = NULL, desiredtz = "", verbose = TRUE) {
  if (verbose == TRUE) {
    cat("\n  Align individuals:")
  }
  #======================================================================
  # identify houdesholds and IDs based on filenames and put these in table
  path_timeseries = GGIR_ts_dir
  path_results = paste0(outputdir, "/part6HouseholdCoAnalysis")
  if (!dir.exists(path_results)) dir.create(path = path_results, recursive = TRUE)
  
  path_results_ts = paste0(path_results, "/alignedTimeseries")
  if (!dir.exists(path_results_ts)) dir.create(path = path_results_ts, recursive = TRUE)
  
  fns = dir(path_timeseries, pattern = "RData")
  getIDs = function(x) {
    tmp = unlist(strsplit(x, "-|_"))
    return(data.frame(HID = tmp[2], MID = tmp[3], filename = x))
  }
  fileOverview = lapply(fns, getIDs)
  fileOverview = do.call(rbind,fileOverview)
  S = table(fileOverview$HID)
  fileOverview$houshold_size = NA
  for (i in 1:length(S)) {
    rowi = which(fileOverview$HID == names(S)[i])
    fileOverview$houshold_size[rowi] = S[i] 
  }
  
  # remove all households with less than two individuals:
  fileOverview = fileOverview[which(fileOverview$houshold_size > 1),]
  uHID = unique(fileOverview$HID)
  
  pdf(file = paste0(path_results, "/timeseriesPlot.pdf"))
  #======================================================================
  # Loop over households and IDs to check and merge the data
  
  mdat = NULL
  # Loop over houdeshold IDS
  for (h in 1:length(uHID)) {
    if (verbose == TRUE) cat(paste0("\n    Houdeshold ", uHID[h],": "))
    uMID = fileOverview$MID[which(fileOverview$HID == uHID[h])]
    uFilename = fileOverview$filename[which(fileOverview$HID == uHID[h])]
    out = NULL
    # Loop over Houdeshold members
    for (p in 1:length(uMID)) {
      if (verbose == TRUE) cat(paste0(uMID[p], " ")) #, uFilename[p]))
      # load data
      load(file = paste0(path_timeseries, "/", uFilename[p]))
      D = mdat
      if (nrow(D) > 0) { # check if file is not empty
        # Identify corresponding GGIR part 1 milestone data
        pat1 = unlist(strsplit(x = uFilename[p], split = "[.]csv"))[1]
        pat2 = unlist(strsplit(x = pat1, split = "_"))
        pat2 = paste0(pat2[1:(length(pat2) - 1)], collapse = "_")
        ptrn = paste0("meta_",pat2)
        msfile = dir(path = path_ggirms,  pattern = ptrn, full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
        # merge temperature and light if they are available
        extravars = c()
        if (length(msfile) > 0) {
          load(msfile)
          if (all(c("lightmean", "lightpeak", "temperaturemean") %in% colnames(M$metalong))) {
            M$metalong$timestamp = as.POSIXct(M$metalong$timestamp,
                                              format = "%Y-%m-%dT%H:%M:%S%z", tz = desiredtz, origin = "1970-1-1")
            M$metalong$timenum = as.numeric(M$metalong$timestamp)
            
            epochSizeMetalong = mean(abs(diff(M$metalong$timenum[1:3])))
            epochSizeMS5 = mean(abs(diff(D$timenum[1:3])))
            if (epochSizeMetalong > epochSizeMS5) { # If long epoch size in part 1 was larger than 60 seconds 
                          M$metalong = M$metalong[rep(1:nrow(M$metalong),
                                                      each = epochSizeMetalong / epochSizeMS5), ]
              newTimenum = seq(M$metalong$timenum[1], M$metalong$timenum[nrow(M$metalong)] + epochSizeMetalong, by = epochSizeMS5)
              M$metalong$timenum = newTimenum[1:nrow(M$metalong)]
              M$metalong$timestamp = as.POSIXct(M$metalong$timenum,
                                                origin = "1970-1-1", tz = desiredtz)
            }
            if ("lightpeak" %in% colnames(D)) D = D[, -which(colnames(D) == "lightpeak")]
            D = merge(D, M$metalong[,c("lightmean", "lightpeak", "temperaturemean", "timenum")], by = "timenum")
            extravars = c("lightmean", "lightpeak", "temperaturemean")
          }
        } else {
          warning(paste0("\nGGIR milestone data not found for ", msfile))
        }
        invalid = which((D$invalid_fullwindow > 20 |
                           D$invalid_wakinghours > 20 | 
                           D$invalid_sleepperiod > 20) & D$window != 0)
        if (length(invalid) > 0) {
          is.na(D[invalid, 
                  c("ACC", "SleepPeriodTime", "invalidepoch", "guider", "window", "class_id", extravars)]) = TRUE
        }
        D = D[,c("timenum", "ACC", "SleepPeriodTime", "invalidepoch", "guider", "window", "class_id", extravars)]
        # add wakeup indicators
        D$onset = 0
        D$onset[which(diff(D$SleepPeriodTime) > 0)] = 1
        D$wakeup = 0
        D$wakeup[which(diff(D$SleepPeriodTime) < 0)] = 1
        
        if (length(which(is.na(D$ACC) == FALSE)) > 0) {
          D$MID = uMID[p]
          D$HID = uHID[h]
          if (length(out) == 0) {
            out = D
          } else {
            out = rbind(out, D)
          }
        }
      }
      rm(mdat, M)
    }
    
    out$validepoch = (-out$invalidepoch) + 1
    # only continue if there are at least 2 Houdeshold members left
    if (length(unique(out$MID)) > 1) {
      uMID = unique(out$MID)
      out = out[order(out$timenum),]
      
      # reshape to be in wide format to ease doing pairwise comparisons
      alignedTimeseries = reshape(out,
                     idvar = c("timenum", "HID"),
                     timevar = "MID",
                     direction = "wide")
      colnames = paste0("ACC.", uMID)
      
      alignedTimeseries$time_POSIX = as.POSIXct(alignedTimeseries$timenum,
                                                tz = "America/Halifax", origin = "1970-01-01")
      
      # Add indicator of valid number of family members per time point
      alignedTimeseries$N_valid_hhmembers = rowSums(alignedTimeseries[,grep(pattern = "validepoch",
                                                                            x = names(alignedTimeseries), value = FALSE)],
                                                    na.rm = TRUE)
      # Add indicator of valid data for a pair of family members per time point
      varn_remember = c()
      Nmembers = length(uMID)
      for (pa1 in 1:Nmembers) {
        for (pa2 in 1:Nmembers) {
          if (pa1 != pa2 & pa2 > pa1) {
            col1 = grep(pattern = paste0("validepoch.", uMID[pa1]),
                        x = names(alignedTimeseries), value = FALSE)
            col2 = grep(pattern = paste0("validepoch.", uMID[pa2]),
                        x = names(alignedTimeseries), value = FALSE)
            validpair = ifelse(test = rowSums(alignedTimeseries[, c(col1, col2)], na.rm = TRUE) == 2,
                               yes = TRUE, no = FALSE)
            alignedTimeseries$new = validpair
            varname = paste0("validpair_",uMID[pa1],"_", uMID[pa2])
            colnames(alignedTimeseries)[which(colnames(alignedTimeseries) == "new")] = varname
            varn_remember = c(varn_remember, varname)
          }
        }
      }
      Npairs = length(varn_remember)
      #======================================================================
      # Visualise data
      if (exists("currentColorPalette")) rm(currentColorPalette)
      Ncols = (Npairs + Nmembers)
      palette(rainbow(n = Ncols, alpha = 1)) 
      currentColorPalette = palette()
      colorsM = currentColorPalette[1:Nmembers]
      colorsP = currentColorPalette[(Nmembers + 1):(Nmembers + Npairs)]
      
      par(mfrow = c(2,1))
      plot(alignedTimeseries$time_POSIX, alignedTimeseries[, colnames[1]], type = "l", col = colorsM[1],
           main = paste0("Houshold ", uHID[h]), xlab = "", ylab = "ACC", bty = "l")
      for (jj in 2:length(colnames)) {
        lines(alignedTimeseries$timenum, alignedTimeseries[, colnames[jj]], type = "l", col = colorsM[jj])
      }
      legend("topright", legend = colnames,
             col = colorsM[1:length(colnames)], lty = 1, cex = 0.6, bg = "white", ncol = 3)
      LTY = rep(1, Npairs)
      
      for (vn in 1:Npairs) {
        if (vn == 1) {
          plot(alignedTimeseries$time_POSIX, alignedTimeseries[,varn_remember[vn]] - 0.05 + (vn/50), type = "l",
               col = colorsP[vn], ylim = c(0, 1.5),
               xlab = "", ylab = "Valid pair", bty = "l",
               axes = FALSE, lty = LTY[vn], lwd = 1.3)
        } else {
          lines(alignedTimeseries$time_POSIX, alignedTimeseries[,varn_remember[vn]] - 0.05 + (vn/50), type = "l",
                col = colorsP[vn], lty = LTY[vn], lwd = 1.3)
        }
      }
      axis(side = 2, at = c(0, 1), labels = c("FALSE", "TRUE"))
      legend("topright", legend = gsub(pattern = "valid", replacement = "", x = varn_remember),
             col = colorsP[1:Npairs], lty = LTY[1:Npairs], cex = 0.6, bg = "white", ncol = 3)
      for (i in 1:ncol(alignedTimeseries)) {
        NA_NaN = which(is.na(alignedTimeseries[, i]))
        alignedTimeseries[NA_NaN, i] = ""
      }
      rm(currentColorPalette)

      # Convert character to appropriate data format
      numericColumns = grep(pattern = "onset|wake|valid_hhmembers|epoch|temperature|light|class|SleepPeriod|ACC|window|guider", x = colnames(alignedTimeseries))
      for (jj in numericColumns) {
        alignedTimeseries[,jj] = as.numeric(alignedTimeseries[ ,jj])
      }
      boolColumns = grep(pattern = "validpair", x = colnames(alignedTimeseries))
      for (jj in boolColumns) {
        alignedTimeseries[,jj] = as.logical(alignedTimeseries[ ,jj])
      }
      # Store data
      save(alignedTimeseries,file = paste0(path_results, "/alignedTimeseries/timeseries_HID_", uHID[h], ".RData"))
      # write.csv(x = alignedTimeseries,
      #           file = paste0(path_results, "/alignedTimeseries/timeseries_HID_",uHID[h],".csv"),
      #           row.names = FALSE)
    }
  }
  dev.off()
}