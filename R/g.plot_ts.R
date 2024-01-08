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
  if (dir.exists(expected_ts_path)) {
    fnames.ms5raw = dir(expected_ts_path)
    ts_exists = ifelse(length(fnames.ms5raw) > 0, yes = TRUE, no = FALSE) 
    
    # Extract behavioural class names:
    legendfiles = list.files(path = paste0(metadatadir, "/meta/ms5.outraw"), pattern = "codes", full.names = TRUE)
    df = file.info(legendfiles)
    legendfiles = rownames(df)[which.max(df$mtime)]
    legend = read.csv(rownames(df)[which.max(df$mtime)])
    behClassNames = legend$class_name
    behClassCodes = legend$class_id
    
    neworder = c(grep(pattern = "sleep", x = behClassNames), grep(pattern = "IN", x = behClassNames),
                 grep(pattern = "LIG", x = behClassNames),
                 grep(pattern = "MVPA", x = behClassNames)) 
    behClassNames = behClassNames[neworder]
    behClassCodes = behClassCodes[neworder]
    # loop through files
    mdat = NULL
    for (i in f0:f1) {
      if (ts_exists) {
        load(file = paste0(metadatadir, "/meta/ms5.outraw/",
                           part6_threshold_combi, "/", fnames.ms5raw[i]))
        
        
        # TO DO:
        # - Allow for splitting (day/WW/.../x hours before onset/window surrounding each nap, etc)
        # - Allow for selecting variables to show
        # - Add vertical grid to ease inspection
        # mdat = mdat[1:1440,]
        Ymax = max(mdat$ACC)
        Ymin = min(mdat$ACC)
        
        # graphics.off()
        # x11()
        # layout.matrix <- matrix(c(1, 2, 3, 4), nrow = 4, ncol = 1)
        # layout(mat = layout.matrix,
        #        heights = c(1, 1, 1, 3), # Heights rows
        #        widths = c(5)) # Widths columns
        # 
        # par(mar = c(1, 4, 1, 8))
        # plot(mdat$timestamp, mdat$ACC, type = "l", col = "grey", cex = 0.5, bty = "l",
        #      xlab = "Timestamp", ylab = expression(paste("Acceleration (m", italic("g"), ")")),
        #      xaxt = 'n')
        # par(mar = c(1, 4, 1, 8))
        # plot(mdat$timestamp, mdat$angle, type = "l", col = "grey", cex = 0.5, bty = "l",
        #      xlab = "Timestamp", ylab = "Angle (degrees)", xaxt = 'n')
        # par(mar = c(1, 4, 1, 8))
        # plot(mdat$timestamp, mdat$lightpeak / 1000, type = "l", col = "grey", cex = 0.5, bty = "l", 
        #      xlab = "Timestamp", ylab = "Light (klux)", xaxt = 'n')
        # par(mar = c(4, 4, 1, 8))
        # plot(mdat$timestamp, mdat$ACC, type = "l",
        #      ylim = c(0, Ymax), col = "white", cex = 0.5, bty = "l",
        #      xlab = "Timestamp", ylab =  expression(paste("Acceleration (m", italic("g"), ")")))
        # selfreport = c("nap", "nonwear", "sleeplog")
        # uclasses = behClassCodes
        # 
        # class2remove = grep(pattern = "unbt|spt_wake", x = behClassNames, invert = FALSE, value = FALSE)
        # behClassNames = behClassNames[-class2remove]
        # uclasses = uclasses[uclasses %in% (class2remove - 1) == FALSE]
        # 
        # binlabs = c("SleepPeriodTime", "sibdetection", "invalidepoch")
        # Nlevels = length(uclasses) + length(binlabs) + length(selfreport)
        # 
        # YLAB = c(selfreport, binlabs, behClassNames)
        # YLAB = gsub(pattern = "sibdetection", replacement = "sib", x = YLAB)
        # YLAB = gsub(pattern = "nap", replacement = "selfreported_nap", x = YLAB)
        # YLAB = gsub(pattern = "nonwear", replacement = "selfreported_nonwear", x = YLAB)
        # YLAB = gsub(pattern = "sleeplog", replacement = "selfreported_sleepwindow", x = YLAB)
        # 
        # COL = rainbow(n = Nlevels, alpha = 0.5)
        # 
        # axis(side = 4, at = seq((Ymax / Nlevels) / 2, Ymax, by = Ymax / Nlevels),
        #      labels = YLAB, las = 2, cex.axis = 0.7)
        # lev = 1
        # 
        # for (si in 1:length(selfreport)) {
        #   tempi = which(mdat$selfreport == selfreport[si])
        #   if (length(tempi) > 0) {
        #     A = rep(0, nrow(mdat))
        #     A[tempi] = 1
        #     t0 = mdat$timestamp[which(diff(c(0, A)) == 1)]
        #     t1 = mdat$timestamp[which(diff(c(A, 0)) == -1)]
        #     y0 = Ymax * ((lev - 1) / Nlevels)
        #     y1 = Ymax * ((lev) / Nlevels)
        #     rect(xleft = t0, xright = t1, ybottom = y0, ytop = y1 , col = COL[lev], border = FALSE)
        #   }
        #   lev = lev + 1
        # }
        # for (labi in 1:length(binlabs)) {
        #   if (length(table(mdat[,binlabs[labi]])) > 1) {
        #     t0 = mdat$timestamp[which(diff(c(0, mdat[,binlabs[labi]])) == 1)]
        #     t1 = mdat$timestamp[which(diff(c(mdat[,binlabs[labi]], 0)) == -1)]
        #     y0 = Ymax * ((lev - 1) / Nlevels)
        #     y1 = Ymax * ((lev) / Nlevels)
        #     rect(xleft = t0, xright = t1, ybottom = y0, ytop = y1 , col = COL[lev], border = FALSE)
        #   }
        #   lev = lev + 1
        # }
        # for (clai in 1:length(uclasses)) {
        #   tempi = which(mdat$class_id == uclasses[clai])
        #   if (length(tempi) > 0) {
        #     A = rep(0, nrow(mdat))
        #     A[tempi] = 1
        #     t0 = mdat$timestamp[which(diff(c(0, A)) == 1)]
        #     t1 = mdat$timestamp[which(diff(c(A, 0)) == -1) + 1]
        #     y0 = Ymax * ((lev - 1) / Nlevels)
        #     y1 = Ymax * ((lev) / Nlevels)
        #     rect(xleft = t0, xright = t1, ybottom = y0, ytop = y1 , col = COL[lev], border = FALSE) #boarder = FALSE
        #   }
        #   lev = lev + 1
        # }
        # browser()
      }
    }
  }
}
