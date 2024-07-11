g.plot_ts = function(metadatadir = c(), 
                     viewingwindow = 1,
                     f0 = c(), f1 = c(), overwrite = FALSE,
                     desiredtz = "",
                     verbose = TRUE,
                     part6_threshold_combi = NULL) {
  if (!file.exists(paste0(metadatadir, "/results/file summary reports"))) {
    dir.create(file.path(paste0(metadatadir, "/results"), "file summary reports"))
  }
  
  # Declare functions:
  panelplot = function(mdat, ylabels_plot2, Nlevels, selfreport_vars, binary_vars,
                       BCN, BCC, axis_resolution) {
    window_duration = mdat$timenum[nrow(mdat)] - mdat$timenum[1]
    # print(paste0("window duration: ", window_duration / 3600))
    
    # create vector with color blind friendly colors
    mycolors = c("#E69F00","#56B4E9","#009E73","#F0E442",
                 "#0072B2","#D55E00","#CC79A7", "#999999",
                 "#222255", "black")
    mygreys = rep(c("darkblue", "lightblue"), 20)
    mygreys = grDevices::adjustcolor(col = mygreys, alpha.f = 0.5)
    
    Ymax = max(mdat$ACC, na.rm = TRUE) * 2
    Ymin = min(mdat$ACC, na.rm = TRUE)
    if (is.na(Ymax)) Ymax = 100
    if (is.na(Ymin)) Ymin = 0
    
    layout.matrix <- matrix(c(1, 2, 3), nrow = 3, ncol = 1)
    layout(mat = layout.matrix,
           heights = c(1, 2, 2), # Heights rows
           widths = c(5)) # Widths columns
    #----- LUX
    par(mar = c(1, 4, 1, 8))
    plot(mdat$timestamp, mdat$lightpeak / 1000, type = "l", col = "grey", cex = 0.8, bty = "l",
         xlab = "Timestamp", ylab = "Light (klux)", xaxt = 'n', ylim = c(0, 20))
    
    #----- Acceleration and main non-overlapping behavioural classes
    par(mar = c(1, 4, 1, 8))
    plot(mdat$timestamp, mdat$ACC, type = "l",
         ylim = c(0, Ymax), col = "grey", cex = 0.5, bty = "l", xaxt = 'n',
         xlab = "", ylab =  expression(paste("Acceleration (m", italic("g"), ")")))
    COL = mycolors[1:Nlevels[1]]
    y = (0:(Nlevels[1] - 1)) / (Nlevels[1] - 1)
    fraction_bottom_bars = 0.45
    fraction_for_bars = 0.35
    yticks = Ymax * fraction_bottom_bars + (y * fraction_for_bars * Ymax)
    yStepSize = (Ymax * fraction_for_bars) / Nlevels[1]
    axis(side = 4, at = yticks,
         labels = BCN, las = 2, cex.axis = 0.7)
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
    legend("top", legend = BCN, bty = 'n',
           col = COL, pch = rep(15, length(BCN)),
           ncol = pmin(5, length(BCN)), cex = 1, pt.cex = 2) #
    #----- Angle and overlapping classes and self-reported classes:
    par(mar = c(4, 4, 1, 8))
    plot(mdat$timestamp, mdat$angle, type = "l",
         ylim = c(-90, 90), col = "grey", cex = 0.5, bty = "l", xaxt = 'n',
         xlab = "Timestamp", ylab = "Angle (degrees)")
    axis(side = 4, at = seq(-90 + (180/Nlevels[2]/2), 90, by = 180 / Nlevels[2]),
         labels = ylabels_plot2, las = 2, cex.axis = 0.7)
    
    # assign timestamp axis:
    date = as.numeric(format(mdat$timestamp, "%d"))
    hour = as.numeric(format(mdat$timestamp, "%H"))
    min = as.numeric(format(mdat$timestamp, "%M"))
    sec = as.numeric(format(mdat$timestamp, "%S"))
    ticks = which(min == 0 & sec == 0 & hour %in% seq(0, 24, by = axis_resolution))
    atTime = mdat$timestamp[ticks]
    if (axis_resolution <= 12) {
      labTime = paste0(hour[ticks], ":00")
      axis(side = 1, at = atTime,
           labels = labTime, las = 1, cex.axis = 0.7)
    } else {
      labTime = date[ticks]
      axis(side = 1, at = atTime,
           labels = rep("", length(labTime)), las = 1, cex.axis = 0.7)
      axis(side = 1, at = atTime + 12 * 3600,
           labels = labTime,  tick = FALSE, las = 1, cex.axis = 0.7)
    }
    
    lev = 1
    for (si in 1:length(selfreport_vars)) {
      tempi = which(mdat$selfreport == selfreport_vars[si])
      if (length(tempi) > 0) {
        A = rep(0, nrow(mdat))
        A[tempi] = 1
        t0 = mdat$timestamp[which(diff(c(0, A)) == 1)]
        t1 = mdat$timestamp[which(diff(c(A, 0)) == -1)]
        y0 = -90 + 180 * ((lev - 1) / Nlevels[2])
        y1 = -90 + 180 * ((lev) / Nlevels[2])
        rect(xleft = t0, xright = t1, ybottom = y0, ytop = y1 , col = mygreys[lev], border = FALSE)
      }
      lev = lev + 1
    }
    for (labi in 1:length(binary_vars)) {
      if (length(table(mdat[,binary_vars[labi]])) > 1) {
        t0 = mdat$timestamp[which(diff(c(0, mdat[,binary_vars[labi]])) == 1)]
        t1 = mdat$timestamp[which(diff(c(mdat[,binary_vars[labi]], 0)) == -1)]
        y0 = -90 + 180 * ((lev - 1) / Nlevels[2])
        y1 = -90 + 180 * ((lev) / Nlevels[2])
        rect(xleft = t0, xright = t1, ybottom = y0, ytop = y1 , col = mygreys[lev], border = FALSE)
      }
      lev = lev + 1
    }
  }
  
  #---------------------------------------
  # Attempt to load time series directly
  
  # Identify correct subfolder
  
  expected_ts_path = paste0(metadatadir, "/meta/ms5.outraw/", part6_threshold_combi)
  expected_ms5raw_path = paste0(metadatadir, "/meta/ms5.outraw")
  # if (!dir.exists(expected_ts_path)) {
  #   if (!dir.exists(expected_ms5raw_path)) {
  #     stop("\nPath ", expected_ms5raw_path, " does not exist", call. = FALSE)
  #   } else {
  #     subDirs = list.dirs(expected_ms5raw_path)
  #     if (length(subDirs) > 0) {
  #       expected_ts_path = paste0(expected_ms5raw_path, "/", subDirs[1])
  #       warning(paste0("\nThreshold combi ", part6_threshold_combi,
  #                      " in time series ouput. Instead now using", subDirs[1]), call. = FALSE)
  #     } else {
  #       stop(paste0("\nNo subfolders found inside ", expected_ms5raw_path), call. = FALSE)
  #     }
  #   }
  # }
  
  
  axis_resolution = 6  # assumption that this is either (0, 12] or 24.
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
      BCN = gsub(pattern = "sleep", replacement = "noc_sleep", x = BCN)
      
      # Bottom plot
      selfreport_vars = c("nap", "nonwear", "sleeplog")
      binary_vars = c("SleepPeriodTime", "sibdetection", "invalidepoch")
      Nlevels[2] = length(binary_vars) + length(selfreport_vars)
      ylabels_plot2 = c(selfreport_vars, binary_vars)
      ylabels_plot2 = gsub(pattern = "invalidepoch", replacement = "invalid", x = ylabels_plot2)
      ylabels_plot2 = gsub(pattern = "SleepPeriodTime", replacement = "acc_spt", x = ylabels_plot2)
      ylabels_plot2 = gsub(pattern = "sibdetection", replacement = "acc_sib", x = ylabels_plot2)
      ylabels_plot2 = gsub(pattern = "nap", replacement = "selfreported_nap", x = ylabels_plot2)
      ylabels_plot2 = gsub(pattern = "nonwear", replacement = "selfreported_nonwear", x = ylabels_plot2)
      ylabels_plot2 = gsub(pattern = "sleeplog", replacement = "selfreported_sleepwindow", x = ylabels_plot2)
      
      # loop through files
      for (i in f0:f1) {
        # cat("\n")
        # print(paste0("file ", i, " ", fnames.ms5raw[i]))
        load(file = paste0(metadatadir, "/meta/ms5.outraw/",
                           part6_threshold_combi, "/", fnames.ms5raw[i]))
        if (all(c("lightpeak", "selfreported", "sibdetection") %in% colnames(mdat))) {
          # TO DO:
          # - Add vertical grid to ease inspection tailored to window selected
          
          pdf(paste0(metadatadir, "/results/file summary reports/Time_report_",
                     fnames.ms5raw[i], ".pdf"), paper = "a4r",
              width = 0, height = 0)
          
          # whole data
          panelplot(mdat, ylabels_plot2, Nlevels, selfreport_vars, binary_vars,
                    BCN, BCC, axis_resolution = 24)
          
          # zoom on windows that have either daytime sib or selfreported nap
          acc_naps = which((mdat$sibdetection == 1 | mdat$selfreport == "nap") &
                             mdat$SleepPeriodTime == 0)
          subploti = seq(1, nrow(mdat), by = 540)
          subploti = cbind(subploti,
                           subploti + 720)
          for (jj in 1:nrow(subploti)) {
            ma = which(acc_naps > subploti[jj, 1] & acc_naps < subploti[jj, 2])
            if (length(ma) == 0) {
              is.na(subploti[jj, ]) = TRUE
            }
          }
          subploti = subploti[!is.na(subploti[,1]), , drop = FALSE]
          if (nrow(subploti) > 0) {
            for (ani in 1:nrow(subploti)) {
              panelplot(mdat[(subploti[ani, 1] + 1):subploti[ani, 2], ],
                        ylabels_plot2, Nlevels, selfreport_vars, binary_vars,
                        BCN, BCC, axis_resolution = 1)
            }
          }
          dev.off()
        }
      }
    }
  }
}