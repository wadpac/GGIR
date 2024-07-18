visualreport = function(metadatadir = c(), 
                        f0 = c(), f1 = c(), overwrite = FALSE,
                        desiredtz = "",
                        verbose = TRUE,
                        part6_threshold_combi = NULL) {
  if (!file.exists(paste0(metadatadir, "/results/file summary reports"))) {
    dir.create(file.path(paste0(metadatadir, "/results"), "file summary reports"))
  }
  
  # Declare functions:
  panelplot = function(mdat, ylabels_plot2, Nlevels, selfreport_vars, binary_vars,
                       BCN, BCC, title = "", NhoursPerRow = NULL, plotid = 0) {
    window_duration = mdat$timenum[nrow(mdat)] - mdat$timenum[1]
    
    # create vector with color blind friendly colors
    mycolors = c("#E69F00","#56B4E9","#009E73","#F0E442",
                 "#0072B2","#D55E00", "#999999", #"#CC79A7"
                 "#222255", "black")
    mycolors = rep(mycolors, 3)
    signalcolor = "black"
    mycolors = grDevices::adjustcolor(col = mycolors, alpha.f = 0.8)

    grey_for_lux = gray.colors(n = 100, start = 0.95, end = 0.1)
    grey_for_lux[1:2] = "white"
    myred = grDevices::adjustcolor(col = "#CC79A7", alpha.f = 0.6) #"red"
    
    # Prepare time tick points
    date = paste0(as.numeric(format(mdat$timestamp, "%d")), " ", format(mdat$timestamp, "%b"))
    hour = as.numeric(format(mdat$timestamp, "%H"))
    min = as.numeric(format(mdat$timestamp, "%M"))
    sec = as.numeric(format(mdat$timestamp, "%S"))
    ticks = which(min == 0 & sec == 0 & hour %in% seq(0, 24, by = 1))
    atTime = mdat$timestamp[ticks]
    datLIM = as.Date(min(mdat$timestamp, na.rm = TRUE), tz = desiredtz)
    XLIM = as.POSIXct(paste0(datLIM[1], " 00:00:00"), tz = desiredtz)
    XLIM[2] = XLIM[1] + NhoursPerRow * 3600
  
    par(mar = c(2.5, 0, 1.5, 4.5))
    #============================================
    # Acceleration and angle signal
    # scale 0 - 100, where bottom half is used for angle and top half for acceleration
    Ymax = 100
    Ymin = 0 #min(mdat$ACC, na.rm = TRUE)
    accy = (mdat$ACC / 25) + 50
    angy = (((mdat$angle + 90) * 50) / 180)
    luxy = ceiling(pmin(mdat$lightpeak + 1, 20000) / 400) * 2
    plot(mdat$timestamp, accy, type = "l",
         ylim = c(0, Ymax), xlim = XLIM, col = signalcolor,
         cex = 0.5, xaxt = 'n', axes = FALSE,
         xlab = "", ylab = "", cex.lab = 0.6, cex.axis = 0.7,
         main = "", lwd = 0.3, cex.main = 0.9)
    lines(mdat$timestamp, angy, type = "l",
         col = signalcolor, lwd = 0.3)
    text(x = mdat$timestamp[1], y = 60, labels = "Acceleration",
         pos = 4, cex = 0.7, col = signalcolor, font = 2)
    text(x = mdat$timestamp[1], y = 40, labels = "Angle-z",
         pos = 4, cex = 0.7, col = signalcolor, font = 2)
    
    if (plotid == 1) {
      mtext(text = "Generated with R package GGIR", cex = 0.5, side = 3, line = 0, 
            at = XLIM[1] + 1800, adj = 0)
    }
    
    if (NhoursPerRow <= 36) {
      cex_axis = 0.5
    } else {
      cex_axis = 0.5
    }
    
    changepoints = function(x) {
      Nvalues = length(x)
      if (Nvalues > 3) {
        changes = which(x[2:Nvalues] != x[1:(Nvalues - 1)])
        changes = sort(unique(c(1, changes, changes + 1, Nvalues)))
        if (length(changes) == 0) {
          changes = 1:Nvalues
        }
      } else {
        changes = 1:Nvalues
      } 
      return(changes)
    }  
    if (NhoursPerRow <= 36) {
      cex_mtext = 0.4
    } else {
      cex_mtext = 0.35 #25
    }
    
    
    # assign timestamp axis:
    mtext(text = hour[ticks], side = 1, line = 0, cex = cex_mtext, at = atTime)

    # with window numbers
    windowNow = mdat$window[ticks]
    changes = changepoints(windowNow)
    mtext(text = windowNow[changes], side = 1, line = 0.75, cex = cex_mtext,
          at = atTime[changes])
    # with dates
    dateNow = as.numeric(format(mdat$timestamp[ticks], "%d"))
                     # substring(format(mdat$timestamp[ticks], "%b"), 1, 1))

    changes = changepoints(dateNow)
    mtext(text = dateNow[changes], side = 1, line = 1.5, cex = cex_mtext,
          at = atTime[changes])
    
    mtext(text = "hour", side = 1, line = 0, cex = cex_mtext,
          at = atTime[length(atTime)] + 5500, adj = 0)
    mtext(text = "window number", side = 1, line = 0.75, cex = cex_mtext,
          at = atTime[length(atTime)] + 5500, adj = 0)
    mtext(text = "date", side = 1, line = 1.5, cex = cex_mtext,
          at = atTime[length(atTime)] + 5500, adj = 0)
    
    
    # Add grid of dots
    abline(v = atTime, col = "black", lty = "1F", lwd = 0.5)
    
    #============================================
    # add rectangular blocks to reflect classes
    Nlevels = sum(Nlevels) - 1 # -1 because invalidepoch will not be a rectangle
    if ("lightpeak" %in% colnames(mdat)) Nlevels = Nlevels + 1
    COL = mycolors[1:Nlevels]
    yticks = Ymax * seq(from = 0.05, to = 0.95, length.out = Nlevels)
    yStepSize = min(diff(yticks)) * 0.5
    lev = 1
    # Binary classes
    for (labi in 1:length(binary_vars)) {
      freqtab = table(mdat[,binary_vars[labi]])
      if (length(freqtab) > 1 || names(freqtab)[1] == "1") {
        t0 = mdat$timestamp[which(diff(c(0, mdat[,binary_vars[labi]])) == 1)]
        t1 = mdat$timestamp[which(diff(c(mdat[, binary_vars[labi]], 0)) == -1)]
        if (binary_vars[labi] != "invalidepoch") {
          y0 = yticks[lev] + yStepSize
          y1 = yticks[lev] - yStepSize
          col = mycolors[lev]
          rect(xleft = t0, xright = t1, ybottom = y0, ytop = y1 , col = col, border = FALSE)
        }
      }
      if (binary_vars[labi] != "invalidepoch") lev = lev + 1
    }
    # Diary-based variables
    for (si in 1:length(selfreport_vars)) {
      tempi = which(mdat$selfreport == selfreport_vars[si])
      if (length(tempi) > 0) {
        A = rep(0, nrow(mdat))
        A[tempi] = 1
        t0 = mdat$timestamp[which(diff(c(0, A)) == 1)]
        t1 = mdat$timestamp[which(diff(c(A, 0)) == -1)]
        y0 = yticks[lev] + yStepSize
        y1 = yticks[lev] - yStepSize
        rect(xleft = t0, xright = t1, ybottom = y0, ytop = y1 , col = mycolors[lev], border = FALSE)
      }
      lev = lev + 1
    }
    
    # Behavioural classes
    for (clai in 1:length(BCC)) {
      tempi = which(mdat$class_id == BCC[clai])
      if (length(tempi) > 0) {
        A = rep(0, nrow(mdat))
        A[tempi] = 1
        t0 = mdat$timestamp[which(diff(c(0, A)) == 1)]
        t1 = mdat$timestamp[which(diff(c(A, 0)) == -1) + 1]
        y0 = yticks[lev] - yStepSize * 0.8
        y1 = yticks[lev] + yStepSize * 0.8
        rect(xleft = t0, xright = t1, ybottom = y0, ytop = y1 , col = mycolors[lev], border = FALSE) #boarder = FALSE
      }
      lev = lev + 1
    }
    
    if ("lightpeak" %in% colnames(mdat)) {
      lines(mdat$timestamp, rep(yticks[lev], nrow(mdat)), type = "p", pch = 15,
            col = grey_for_lux[luxy], cex = 0.8, lwd = 1)
      lev = lev + 1
    }
    
    # Highlight invalid epochs as hashed area on top of all rects
    if ("invalidepoch" %in% binary_vars) {
      freqtab = table(mdat$invalidepoch)
      if (length(freqtab) > 1 || names(freqtab)[1] == "1") {
        t0 = mdat$timestamp[which(diff(c(0, mdat$invalidepoch)) == 1)]
        t1 = mdat$timestamp[which(diff(c(mdat$invalidepoch, 0)) == -1)]
        col = myred
        rect(xleft = t0, xright = t1, ybottom = 0, ytop = Ymax,
             col = col, density = 20, border = FALSE)
      }
    }
    
    # Add color labels
    tickLabels = c(ylabels_plot2, BCN)
    if ("lightpeak" %in% colnames(mdat)) tickLabels = c(tickLabels, "lux")
    tickLabels = tickLabels[which(tickLabels != "invalid")]
    for (gi in 1:length(yticks)) {
      if (tickLabels[gi] != "lux") {
        col = mycolors[gi]
      } else {
        col = "grey"
      }
      axis(side = 4, at = yticks[gi], lwd = 2,
           labels = tickLabels[gi], col = col, las = 2, cex.axis = 0.5,
           line = 0)
    }
    # onset/wake lines:
    window_edges = which(diff(mdat$SleepPeriodTime) != 0)
    if (length(window_edges) > 0) {
      abline(v = mdat$timestamp[window_edges], col = "black", lwd = 0.7)
      for (wei in 1:length(window_edges)) {
        # onset and wake
        if (mdat$SleepPeriodTime[window_edges[wei]] == 1) {
          toptext = paste0("wake-up ", weekdays(mdat$timestamp[window_edges[wei]]))
        } else {
          toptext = "onset"  
        }
        mtext(text = toptext, side = 3, line = 0, cex = 0.5,
              at = mdat$timestamp[window_edges[wei]])
      }
    }
  }
  
  #---------------------------------------
  # Attempt to load time series directly
  # Identify correct subfolder
  
  expected_ts_path = paste0(metadatadir, "/meta/ms5.outraw/", part6_threshold_combi)
  expected_ms5raw_path = paste0(metadatadir, "/meta/ms5.outraw")
  
  if (dir.exists(expected_ts_path)) {
    fnames.ms5raw = dir(expected_ts_path, pattern = "[.]RData")
    N_ts_files = length(fnames.ms5raw)
    if (f1 > N_ts_files) f1 = N_ts_files # this is intentionally ms3 and not ms4, do not change!
    if (f1 == 0) f1 = N_ts_files
    
    ts_exists = ifelse(length(fnames.ms5raw) > 0, yes = TRUE, no = FALSE) 
    
    mdat = NULL
    if (ts_exists) {
      
      Nlevels = c(0, 0)
      # Extract behavioural class names and codes:
      legendfiles = list.files(path = paste0(metadatadir, "/meta/ms5.outraw"), pattern = "codes", full.names = TRUE)
      df = file.info(legendfiles)
      legendfiles = rownames(df)[which.max(df$mtime)]
      legendF = read.csv(rownames(df)[which.max(df$mtime)])
      BCN = legendF$class_name # behavioural class names (characters)
      BCC = legendF$class_id # behavioural class codes (numeric)
      
      neworder = c(grep(pattern = "sleep", x = BCN), grep(pattern = "IN", x = BCN),
                   grep(pattern = "LIG", x = BCN), grep(pattern = "MVPA", x = BCN)) 
      BCN = BCN[neworder]
      BCC = BCC[neworder]
      class2remove = grep(pattern = "unbt|spt_wake", x = BCN, invert = FALSE, value = FALSE)
      BCN = BCN[-class2remove]
      BCC = BCC[BCC %in% BCC[class2remove] == FALSE]
      
      Nlevels[1] = length(BCC)
      maxN = 9 # maximum number of levels to show
      if (Nlevels[1] > maxN) {
        BCC = BCC[1:maxN]
        BCN = BCN[1:maxN]
        Nlevels[1] = maxN
      }
      BCN = gsub(pattern = "day_|spt_", replacement = "", x = BCN)
      BCN = gsub(pattern = "sleep", replacement = "sleep_in_spt", x = BCN)
      BCN = tolower(BCN)
      # Bottom plot
      selfreport_vars = c("nap", "nonwear", "sleeplog")
      binary_vars = c("SleepPeriodTime", "sibdetection", "invalidepoch")
      Nlevels[2] = length(binary_vars) + length(selfreport_vars)
      ylabels_plot2 = c(binary_vars, selfreport_vars)
      ylabels_plot2 = gsub(pattern = "invalidepoch", replacement = "invalid", x = ylabels_plot2)
      ylabels_plot2 = gsub(pattern = "SleepPeriodTime", replacement = "spt", x = ylabels_plot2)
      ylabels_plot2 = gsub(pattern = "sibdetection", replacement = "sib", x = ylabels_plot2)
      ylabels_plot2 = gsub(pattern = "nap", replacement = "diary_nap", x = ylabels_plot2)
      ylabels_plot2 = gsub(pattern = "nonwear", replacement = "diary_nonwear", x = ylabels_plot2)
      ylabels_plot2 = gsub(pattern = "sleeplog", replacement = "diary_sleepwindow", x = ylabels_plot2)
      ylabels_plot2 = tolower(ylabels_plot2)
      # loop through files
      for (i in f0:f1) {
        load(file = paste0(metadatadir, "/meta/ms5.outraw/",
                           part6_threshold_combi, "/", fnames.ms5raw[i]))
        
        if (length(mdat) == 0) next
        if (nrow(mdat) == 0) next
        
        if (all(c("lightpeak", "selfreported", "sibdetection") %in% colnames(mdat))) {
          simple_filename = gsub(pattern = ".RData", replacement = "", x = fnames.ms5raw[i] ) #"patientID12345"
          pdf(paste0(metadatadir, "/results/file summary reports/Time_report_",
                     simple_filename, ".pdf"), paper = "a4",
              width = 0, height = 0)
          # zoom on windows that have either daytime sib or selfreported nap
          acc_naps = which((mdat$sibdetection == 1 | mdat$selfreport == "nap") &
                             mdat$SleepPeriodTime == 0)
          
          
          NhoursPerRow = 48
          midnightsi = which(format(mdat$timestamp, "%H") == "00" &
                               format(mdat$timestamp, "%M") == "00" &
                               format(mdat$timestamp, "%S") == "00")
          subploti = c(1, midnightsi + 1)
          subploti = cbind(subploti,
                           c(midnightsi + (NhoursPerRow - 24) * 60, nrow(mdat)))
          
          invalid = which(mdat$invalidepoch == 1)
          # Skip windows without naps?
          # for (jj in 1:nrow(subploti)) {
          #   ma = which(acc_naps > subploti[jj, 1] & acc_naps < subploti[jj, 2])
          #   if (length(ma) == 0) {
          #     is.na(subploti[jj, ]) = TRUE
          #   }
          # }
          subploti[which(subploti[,2] > nrow(mdat)), 2] = nrow(mdat)
          
          NdaysPerPage = 7
          par(mfrow = c(NdaysPerPage, 1), mgp = c(2, 0.8, 0), omi = c(0, 0, 0, 0), bty = "n")
          if (nrow(subploti) > 0) {
            for (ani in 1:nrow(subploti)) {
              
              panelplot(mdat[(subploti[ani, 1] + 1):subploti[ani, 2], ],
                        ylabels_plot2, Nlevels, selfreport_vars, binary_vars,
                        BCN, BCC, title = "", NhoursPerRow = NhoursPerRow, plotid = ani)
            }
          }
          dev.off()
        }
      }
    }
  }
}