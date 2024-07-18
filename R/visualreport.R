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
                       BCN, BCC, title = "", dayid) {
    window_duration = mdat$timenum[nrow(mdat)] - mdat$timenum[1]
    
    # create vector with color blind friendly colors
    mycolors = c("#E69F00","#56B4E9","#009E73","#F0E442",
                 "#0072B2","#D55E00", "#999999", #"#CC79A7"
                 "#222255", "black")
    mycolors = rep(mycolors, 3)
    
    signalcolor = "grey50"
    mycolors = grDevices::adjustcolor(col = mycolors, alpha.f = 0.6)
    # mygreys = rep(c("darkblue", "lightblue"), 20)
    mygreys = rep(c("#009E73","#F0E442","#56B4E9", "#D55E00"), 20)
    mygreys = grDevices::adjustcolor(col = mygreys, alpha.f = 0.6)
    
    myred = grDevices::adjustcolor(col = "#CC79A7", alpha.f = 0.6) #"red"
    
    Ymax = pmax(800, max(mdat$ACC, na.rm = TRUE))
    Ymin = 0 #min(mdat$ACC, na.rm = TRUE)
    
    # Prepare time tick points
    date = paste0(as.numeric(format(mdat$timestamp, "%d")), " ", format(mdat$timestamp, "%b"))
    hour = as.numeric(format(mdat$timestamp, "%H"))
    min = as.numeric(format(mdat$timestamp, "%M"))
    sec = as.numeric(format(mdat$timestamp, "%S"))
    ticks = which(min == 0 & sec == 0 & hour %in% seq(0, 24, by = 1))
    atTime = mdat$timestamp[ticks]
    datLIM = as.Date(range(mdat$timestamp, na.rm = TRUE), tz = desiredtz)
    if (datLIM[1] == datLIM[2]) datLIM = datLIM + c(0, 1)
    XLIM = as.POSIXct(c(paste0(datLIM[1], " 00:00:00"),
                        paste0(datLIM[2], " 12:00:00")), tz = desiredtz)
    #----- LUX
    if (title == "") {
      date = paste0(as.numeric(format(mdat$timestamp[1], "%d")), " ", format(mdat$timestamp[1], "%b"))
      title = paste0("Day ", dayid, " | ", weekdays(mdat$timestamp[1]), " | ", date)
    }
    #----- Acceleration and main non-overlapping behavioural classes
    par(mar = c(0, 0, 2, 4.5))
    meanAcc = diff(range(mdat$ACC)) / 2
    plot(mdat$timestamp, (mdat$ACC / 2) + meanAcc, type = "l",
         ylim = c(0, Ymax), xlim = XLIM, col = signalcolor, cex = 0.5, xaxt = 'n', axes = FALSE,
         xlab = "", ylab = "", cex.lab = 0.6,
         main = "", lwd = 0.3, cex.main = 0.9)
    
    lines(mdat$timestamp, (-mdat$ACC / 2) + meanAcc, type = "l", col = signalcolor, cex = 0.5, lwd = 0.3)
    text(x = mdat$timestamp[1], y = Ymax * 0.5, labels = "Acceleration",
         pos = 4, cex = 0.7, col = signalcolor, font = 2)
    
    title(main = title, adj = 0)
    abline(v = atTime, col = "black", lty = "1F", lwd = 0.5)
    COL = mycolors[1:Nlevels[1]]
    yticks = Ymax * seq(from = 0.1, to = 0.9, length.out = Nlevels[1])
    yStepSize = min(diff(yticks)) * 0.5
    for (gi in 1:length(yticks)) {
      axis(side = 4, at = yticks[gi], lwd = 2,
           labels = BCN[gi], col = mycolors[gi], las = 2, cex.axis = 0.5,
           line = 0)
    }
    lev = 1
    for (clai in 1:length(BCC)) {
      tempi = which(mdat$class_id == BCC[clai])
      if (length(tempi) > 0) {
        A = rep(0, nrow(mdat))
        A[tempi] = 1
        t0 = mdat$timestamp[which(diff(c(0, A)) == 1)]
        t1 = mdat$timestamp[which(diff(c(A, 0)) == -1) + 1]
        y0 = yticks[lev] - yStepSize * 0.8
        y1 = yticks[lev] + yStepSize * 0.8
        rect(xleft = t0, xright = t1, ybottom = y0, ytop = y1 , col = COL[clai], border = FALSE) #boarder = FALSE
      }
      lev = lev + 1
    }
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
    
    # onset/wake lines:
    window_edges = which(diff(mdat$SleepPeriodTime) != 0)
    if (length(window_edges) > 0) {
      abline(v = mdat$timestamp[window_edges], col = "black", lwd = 1)
    }
    
    #----- Angle and overlapping classes and self-reported classes:
    par(mar = c(2, 0, 0, 4.5), bty = "n")
    XLAB = ""
    Ymax = 70
    angleChange = abs(diff(c(mdat$angle, 0)))
    plot(mdat$timestamp, (angleChange / 2), type = "l",
         ylim = c(-Ymax, Ymax), xlim = XLIM, col = signalcolor, cex = 0.5, xaxt = 'n', axes = FALSE,
         xlab = XLAB, ylab = "", cex.lab = 0.6, lwd = 0.3)
    
    lines(mdat$timestamp, (-angleChange / 2), type = "l",
          col = signalcolor, cex = 0.5, lwd = 0.3)
    text(x = mdat$timestamp[1], y = Ymax * 0.5, labels = "Angle-z change",
         pos = 4, cex = 0.7, col = signalcolor, font = 2)
    LUX_scale_hundred = ceiling(pmin(mdat$lightpeak + 1, 20000) / 400) * 2
    CL = rep("yellow", 100)
    for (ci in 1:100) {
      CL[ci] = grDevices::adjustcolor(col = CL[ci], alpha.f = 1/ci)
    }
    CL = rev(CL)
    CL[1:2] = "white"
    CL = CL[LUX_scale_hundred]
    stepticks = (Ymax*2) / Nlevels[2]
    yticks = seq(-Ymax + (Ymax/Nlevels[2]/2), Ymax, by = stepticks)
    for (gi in 1:length(yticks)) {
      if (ylabels_plot2[gi] == "invalid") {
        col = myred
      } else {
        col = mygreys[gi]
      }
      axis(side = 4, at = yticks[gi], lwd = 2,
           labels = ylabels_plot2[gi], col = col, las = 2, cex.axis = 0.5)
    }
    abline(v = atTime, col = "black", lty = "1F", lwd = 0.5)
    # assign timestamp axis:
    labTime = paste0(hour[ticks], "H")
    axis(side = 1, at = atTime,
         labels = labTime, las = 1, cex.axis = 0.5)
    
    buffer = stepticks / 2
    lev = 1
    for (si in 1:length(selfreport_vars)) {
      tempi = which(mdat$selfreport == selfreport_vars[si])
      if (length(tempi) > 0) {
        A = rep(0, nrow(mdat))
        A[tempi] = 1
        t0 = mdat$timestamp[which(diff(c(0, A)) == 1)]
        t1 = mdat$timestamp[which(diff(c(A, 0)) == -1)]
        y0 = yticks[lev] + buffer
        y1 = yticks[lev] - buffer
        rect(xleft = t0, xright = t1, ybottom = y0, ytop = y1 , col = mygreys[lev], border = FALSE)
      }
      lev = lev + 1
    }
    
    for (labi in 1:length(binary_vars)) {
      freqtab = table(mdat[,binary_vars[labi]])
      if (length(freqtab) > 1 || names(freqtab)[1] == "1") {
        t0 = mdat$timestamp[which(diff(c(0, mdat[,binary_vars[labi]])) == 1)]
        t1 = mdat$timestamp[which(diff(c(mdat[, binary_vars[labi]], 0)) == -1)]
        if (binary_vars[labi] == "invalidepoch") {
          col = myred
          rect(xleft = t0, xright = t1, ybottom = -Ymax, ytop = Ymax,
               col = col, density = 20, border = FALSE)
        } else {
          y0 = yticks[lev] + buffer
          y1 = yticks[lev] - buffer
          col = mygreys[lev]
          rect(xleft = t0, xright = t1, ybottom = y0, ytop = y1 , col = col, border = FALSE)
        }
        # print(paste0("y for rect bi: ", y0, " ", y1))
        
      }
      lev = lev + 1
    }
    
    # plot lux
    lines(mdat$timestamp, rep(Ymax*1.07, nrow(mdat)), type = "p", pch = 15, col = CL, cex = 0.8, lwd = 1)
    
    # onset/wake lines:
    if (length(window_edges) > 0) {
      abline(v = mdat$timestamp[window_edges], col = "black", lwd = 1)
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
      BCN = gsub(pattern = "sleep", replacement = "spt_sleep", x = BCN)
      
      # Bottom plot
      selfreport_vars = c("nap", "nonwear", "sleeplog")
      binary_vars = c("SleepPeriodTime", "sibdetection", "invalidepoch")
      Nlevels[2] = length(binary_vars) + length(selfreport_vars)
      ylabels_plot2 = c(selfreport_vars, binary_vars)
      ylabels_plot2 = gsub(pattern = "invalidepoch", replacement = "invalid", x = ylabels_plot2)
      ylabels_plot2 = gsub(pattern = "SleepPeriodTime", replacement = "acc_spt", x = ylabels_plot2)
      ylabels_plot2 = gsub(pattern = "sibdetection", replacement = "acc_sib", x = ylabels_plot2)
      ylabels_plot2 = gsub(pattern = "nap", replacement = "diary_nap", x = ylabels_plot2)
      ylabels_plot2 = gsub(pattern = "nonwear", replacement = "diary_nonwear", x = ylabels_plot2)
      ylabels_plot2 = gsub(pattern = "sleeplog", replacement = "diary_sleepwindow", x = ylabels_plot2)
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
          
          
          midnightsi = which(format(mdat$timestamp, "%H") == "00" &
                               format(mdat$timestamp, "%M") == "00" &
                               format(mdat$timestamp, "%S") == "00")
          subploti = c(1, midnightsi + 1)
          subploti = cbind(subploti,
                           c(midnightsi + 720, nrow(mdat)))
          
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
          par(mfrow = c(NdaysPerPage * 2, 1), mgp = c(2, 0.8, 0), omi = c(0, 0, 0, 0), bty = "n")
          if (nrow(subploti) > 0) {
            for (ani in 1:nrow(subploti)) {
              
              panelplot(mdat[(subploti[ani, 1] + 1):subploti[ani, 2], ],
                        ylabels_plot2, Nlevels, selfreport_vars, binary_vars,
                        BCN, BCC, title = "", dayid = ani)
            }
          }
          dev.off()
        }
      }
    }
  }
}