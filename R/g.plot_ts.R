g.plot_ts = function(metadatadir = c(), 
                     viewingwindow = 1,
                     f0 = c(), f1 = c(), overwrite = FALSE,
                     desiredtz = "",
                     verbose = TRUE,
                     part6_threshold_combi = NULL) {
  
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
  
  # create vector with color blind friendly colors
  mycolors = cbPalette <- c("#E69F00","#56B4E9","#009E73","#F0E442",
                            "#0072B2","#D55E00","#CC79A7", "#999999",
                            "#222255", "black")
  mygreys = rep(c("darkblue", "lightblue"), 20)
  mygreys = adjustcolor(col = mygreys, alpha.f = 0.5)
  
  if (dir.exists(expected_ts_path)) {
    
    fnames.ms5raw = dir(expected_ts_path, pattern = "[.]RData")
    N_ts_files = length(fnames.ms5raw)
    if (f1 > N_ts_files) f1 = N_ts_files # this is intentionally ms3 and not ms4, do not change!
    if (f1 == 0) f1 = N_ts_files
    
    ts_exists = ifelse(length(fnames.ms5raw) > 0, yes = TRUE, no = FALSE) 
    
    mdat = NULL
    if (ts_exists) {
      # Extract behavioural class names:
      legendfiles = list.files(path = paste0(metadatadir, "/meta/ms5.outraw"), pattern = "codes", full.names = TRUE)
      df = file.info(legendfiles)
      legendfiles = rownames(df)[which.max(df$mtime)]
      legend = read.csv(rownames(df)[which.max(df$mtime)])
      behClassNames = legend$class_name
      behClassCodes = legend$class_id
      
      neworder = c(grep(pattern = "sleep", x = behClassNames), grep(pattern = "IN", x = behClassNames),
                   grep(pattern = "LIG", x = behClassNames), grep(pattern = "MVPA", x = behClassNames)) 
      behClassNames = behClassNames[neworder]
      behClassCodes = behClassCodes[neworder]
      class2remove = grep(pattern = "unbt|spt_wake", x = behClassNames, invert = FALSE, value = FALSE)
      behClassNames = behClassNames[-class2remove]
      behClassCodes = behClassCodes[behClassCodes %in% behClassCodes[class2remove] == FALSE]
      
      Nlevels = length(behClassCodes)
      maxN = 9
      if (Nlevels > maxN) {
        behClassCodes = behClassCodes[1:maxN]
        behClassNames = behClassNames[1:maxN]
        Nlevels = maxN
      }
      behClassNames = gsub(pattern = "day_|spt_", replacement = "", x = behClassNames)
      behClassNames = gsub(pattern = "sleep", replacement = "noc_sleep", x = behClassNames)
      
      selfreport = c("nap", "nonwear", "sleeplog")
      
      binlabs = c("SleepPeriodTime", "sibdetection", "invalidepoch")
      Nlevels2 = length(binlabs) + length(selfreport)
      
      YLAB = c(selfreport, binlabs)
      YLAB = gsub(pattern = "invalidepoch", replacement = "invalid", x = YLAB)
      YLAB = gsub(pattern = "SleepPeriodTime", replacement = "acc_spt", x = YLAB)
      YLAB = gsub(pattern = "sibdetection", replacement = "acc_sib", x = YLAB)
      YLAB = gsub(pattern = "nap", replacement = "selfreported_nap", x = YLAB)
      YLAB = gsub(pattern = "nonwear", replacement = "selfreported_nonwear", x = YLAB)
      YLAB = gsub(pattern = "sleeplog", replacement = "selfreported_sleepwindow", x = YLAB)
      # loop through files
      for (i in f0:f1) {
        print(paste0("file", i))
        load(file = paste0(metadatadir, "/meta/ms5.outraw/",
                           part6_threshold_combi, "/", fnames.ms5raw[i]))
        
        # TO DO:
        # - (v) Replace colours by more contrasting colours
        # - Allow for controlling timestamp axis, e.g. 1, 3, 6, 12, 24 hourly ticks by only date at main ticks
        # - Tidy up code and move to separate function
        # - Allow for splitting (day/WW/.../x hours before onset/window surrounding each nap, etc)
        # - Allow for selecting variables to show?
        # - Add vertical grid to ease inspection
        mdat = mdat[1:1440,]
        Ymax = max(mdat$ACC) * 2
        Ymin = min(mdat$ACC)
        
        graphics.off()
        x11()
        layout.matrix <- matrix(c(1, 2, 3), nrow = 3, ncol = 1)
        layout(mat = layout.matrix,
               heights = c(1, 2, 2), # Heights rows
               widths = c(5)) # Widths columns
        #----- LUX
        par(mar = c(1, 4, 1, 8))
        plot(mdat$timestamp, mdat$lightpeak / 1000, type = "l", col = "grey", cex = 0.8, bty = "l",
             xlab = "Timestamp", ylab = "Light (klux)", xaxt = 'n')
        
        #----- Acceleration and main non-overlapping behavioural classes
        par(mar = c(1, 4, 1, 8))
        plot(mdat$timestamp, mdat$ACC, type = "l",
             ylim = c(0, Ymax), col = "grey", cex = 0.5, bty = "l", xaxt = 'n',
             xlab = "", ylab =  expression(paste("Acceleration (m", italic("g"), ")")))
        COL = mycolors[1:Nlevels]
        y = (0:(Nlevels - 1)) / (Nlevels - 1)
        
        
        yticks = Ymax * 0.45 + (y * 0.35 * Ymax)
        yStepSize = (Ymax * 0.35) / Nlevels
        axis(side = 4, at = yticks,
             labels = behClassNames, las = 2, cex.axis = 0.7)
        lev = 1
        for (clai in 1:length(behClassCodes)) {
          tempi = which(mdat$class_id == behClassCodes[clai])
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
        legend("top", legend = behClassNames, bty = 'n',
               col = COL, pch = rep(15, length(behClassNames)),
               ncol = pmin(5, length(behClassNames)), cex = 1, pt.cex = 2) #
        #----- Angle and overlapping classes and self-reported classes:
        par(mar = c(4, 4, 1, 8))
        plot(mdat$timestamp, mdat$angle, type = "l",
             ylim = c(-90, 90), col = "grey", cex = 0.5, bty = "l",
             xlab = "Timestamp", ylab = "Angle (degrees)")
        axis(side = 4, at = seq(-90 + (180/Nlevels2/2), 90, by = 180 / Nlevels2),
             labels = YLAB, las = 2, cex.axis = 0.7)
        # COL = mycolors[1:Nlevels2]
        lev = 1
        for (si in 1:length(selfreport)) {
          tempi = which(mdat$selfreport == selfreport[si])
          if (length(tempi) > 0) {
            A = rep(0, nrow(mdat))
            A[tempi] = 1
            t0 = mdat$timestamp[which(diff(c(0, A)) == 1)]
            t1 = mdat$timestamp[which(diff(c(A, 0)) == -1)]
            y0 = -90 + 180 * ((lev - 1) / Nlevels2)
            y1 = -90 + 180 * ((lev) / Nlevels2)
            rect(xleft = t0, xright = t1, ybottom = y0, ytop = y1 , col = mygreys[lev], border = FALSE)
          }
          lev = lev + 1
        }
        for (labi in 1:length(binlabs)) {
          if (length(table(mdat[,binlabs[labi]])) > 1) {
            t0 = mdat$timestamp[which(diff(c(0, mdat[,binlabs[labi]])) == 1)]
            t1 = mdat$timestamp[which(diff(c(mdat[,binlabs[labi]], 0)) == -1)]
            y0 = -90 + 180 * ((lev - 1) / Nlevels2)
            y1 = -90 + 180 * ((lev) / Nlevels2)
            rect(xleft = t0, xright = t1, ybottom = y0, ytop = y1 , col = mygreys[lev], border = FALSE)
          }
          lev = lev + 1
        }
        print("end")
        # browser()
      }
    }
  }
}
