g.plot = function(IMP, M, I, durplot) {
  # Extracting filename and monitor type 
  fname = I$filename
  mon = I$monc
  monn = I$monn
  dformat = I$dformc
  # cutting out long"___" trains in some of the filenames (especially for Genea)
  fname2 = unlist(strsplit(fname,"_______"))
  if (length(fname2) > 1) fname = paste0(fname2[1], fname2[2])
  
  # Time variable and duration of recording
  ws3 = M$windowsize[1]
  ws2 = M$windowsize[2]
  n_ws2_perday = (1440*60) / ws2
  timeline = 1:nrow(M$metalong)
  durplot = durplot * n_ws2_perday
  
  # Moved function here, because it was nowhere used inside GGIR
  createcoordinates = function(r,timeline) {
    if (length(which(abs(diff(r)) == 1) > 0)) {	
      if (r[1] == 1) {
        x0 = c(timeline[1],timeline[which(diff(r) == 1) + 1])
        x1 = timeline[which(diff(r) == -1) + 1]
        if (r[length(timeline)] == 1) { #file ends with non-wear
          x1 = c(x1,timeline[length(timeline)])
        }
      } else {
        x0 = timeline[which(diff(r) == 1) + 1]
        x1 = timeline[which(diff(r) == -1) + 1]
        if (r[length(timeline)] == 1) { #file ends with non-wear
          x1 = c(x1,timeline[length(timeline)])
        }
      }
    } else {
      x0 = c()
      x1 = c()
    }
    invisible(list(x0=x0,x1=x1))
  }
  
  #create coordinates for rectangles non-wear
  cd1 = createcoordinates(IMP$rout[,1], timeline) #nonwear
  cd2 = createcoordinates(IMP$rout[,2], timeline) #clipping
  cd3 = createcoordinates(IMP$rout[,3], timeline) #additional nonwear
  cd4 = createcoordinates(IMP$rout[,4], timeline) #protocol
  s0 = cd1$x0; 	s1 = cd1$x1
  b0 = cd2$x0; 	b1 = cd2$x1
  g0 = cd3$x0; 	g1 = cd3$x1
  w0 = cd4$x0; 	w1 = cd4$x1  
  dens = 30
  
  # start plot with empty canvas
  plot.new()	
  par(fig = c(0, 1, 0, 1), new = T, mar = c(5, 4, 3, 0))
  plot(seq(0, durplot), seq(0, durplot), col = "white", type = "l", axes = F, 
       xlab = "", ylab = "", main = paste0("device brand: ", monn, " | filename: ", fname), cex.main = 0.6)#dummy plot
  # lim = par("usr")
  # draw coloured rectangles
  Y0 = -50
  Y1 = c(durplot) # + n_ws2_perday)
  legend_names = c("non-wear", "signal clipping", "more non-wear", "study protocol")
  legend_lty = c(NA, NA, NA, NA)
  legend_density = c(100, 100, 100, dens)
  x.intersp = rep(0.5, 4)
  legend_colors = c()
  legend_index = c()
  if (length(s0) > 0) { #non-wear
    CL = colors()[148]
    for (ri in 1:length(s0)) {
      rect(s0[ri], Y0, s1[ri], Y1, border = colors()[148], col = CL) #red 404
    }
    legend_index = 1
    legend_colors = CL
  }
  if (length(b0) > 0) { #clip
    CL = colors()[464]
    for (ri in 1:length(b0)) {
      rect(b0[ri], Y0, b1[ri], Y1, border = colors()[464], col = CL)
    }
    legend_index = c(legend_index, 2)
    legend_colors = c(legend_colors, CL)
  }
  if (length(g0) > 0) {
    CL = colors()[150]
    for (ri in 1:length(g0)) { #additional non-wear
      rect(g0[ri], Y0, g1[ri], Y1, border = colors()[150], col = CL)
    }
    legend_index = c(legend_index, 3)
    legend_colors = c(legend_colors, CL)
  }
  if (length(w0) > 0) {
    CL = colors()[24]
    for (ri in 1:length(w0)) { #protocol
      rect(w0[ri], Y0, w1[ri], Y1, border = colors()[24], col = CL, density = dens)
    }
    legend_index = c(legend_index, 4)
    legend_colors = c(legend_colors, CL)
  }
  
  # Add legend for rectengles
  legend_names = legend_names[legend_index]
  legend_lty = legend_lty[legend_index]
  legend_density = legend_density[legend_index]
  x.intersp = x.intersp[legend_index]
  
  if (length(legend_index) > 0) {
    legend("top", legend = legend_names, col = legend_colors, density = legend_density, #lty = legend_lty,  lty = legend_lty, 
           fill = legend_colors, border = legend_colors, x.intersp = x.intersp, ncol = 4, cex = 0.7, bty = 'n')
  }
  
  abline(v = timeline[length(timeline)], col = "blue", lwd = 1)	
  
  # derive acceleration signal
  # Index of first acceleration metric in metashort other than timestamp, or angle-related
  IndOtMetric = which(colnames(M$metashort) %in% c("timestamp","anglex","angley","anglez") == FALSE)[1]
  metricName = colnames(M$metashort)[IndOtMetric]
  accel = as.numeric(as.matrix(M$metashort[,IndOtMetric]))
  

  accel2 = cumsum(c(0, accel))
  select = seq(1,length(accel2), by = ws2 / ws3)
  Acceleration = diff(accel2[round(select)]) / abs(diff(round(select[1:(length(select))])))
  if (length(timeline) > length(Acceleration)) {
    Acceleration = c(Acceleration, rep(0, abs(length(Acceleration) - length(timeline))))
  } else if (length(timeline) < length(Acceleration)) {
    Acceleration = Acceleration[1:length(timeline)]
  }
  
  MEND = length(timeline)
  ticks = seq(0, nrow(M$metalong) + n_ws2_perday, by = n_ws2_perday)
  # creating plot functions to avoid duplicated code
  plot_acc = function(timeline, Acceleration, durplot, ticks, metricName) {
    if (metricName %in% c("ZCX", "ZCY", "ZCX") == TRUE | 
        length(grep(pattern = "count", x = metricName, ignore.case = TRUE)) > 0) {
      # Metric is not on a G scale
      ylabel = paste0(metricName, " (counts)")
      YLIM = c(0, max(Acceleration, na.rm = TRUE) * 1.05)
      YTICKS = round(c(0, YLIM[2] * 0.3, YLIM[2] * 0.65, YLIM[2] * 0.95))
      YTICKS = unique(round(YTICKS/10) * 10) # round to nearest ten and remove possible duplicates
    } else {
      ylabel = expression(paste("Acceleration (", italic("g"), ")"))
      YLIM = c(0, 0.6)
      YTICKS = c(0, 0.2, 0.4, 0.6)
    }
    plot(timeline, Acceleration, type = "l", xlab = "Day (24 hour blocks relative to start of recording)",
         ylab = ylabel,
         bty = "l", lwd = 0.1, xlim = c(0, durplot), ylim = YLIM, axes = FALSE, cex.lab = 0.8)
    axis(side = 2, at = YTICKS)
    axis(side = 1, at = ticks, labels = 0:(length(ticks) - 1))
  }
  
  plot_nonwear = function(timeline, M, durplot, ticks) {
    plot(timeline, M$metalong$nonwearscore, type = "s",
         xlab = "", ylab = "Non-wear score", axes = F,
         lwd = 0.1, xlim = c(0,durplot), ylim = c(0, 3), cex.lab = 0.8)
    axis(side = 2,at = c(0, 1, 2, 3))
    # axis(side = 1,at = ticks, labels = 1:length(ticks))
  }
  # plot data
  if (mon == 2 | (mon == 4 & dformat == 4)) {
    # Recordings with temperature
    par(fig = c(0,1,0,0.65), new = T)
    plot_acc(timeline, Acceleration, durplot, ticks, metricName)
    
    par(fig = c(0, 1, 0.45, 0.80), new = T)
    plot_nonwear(timeline, M, durplot, ticks)
    
    par(fig = c(0, 1, 0.60, 0.95), new = T)
    plot(timeline, M$metalong$temperaturemean[1:MEND], type = "l",
         xlab = "", ylab = "Temp. (C)", axes = F, lwd = 0.1,
         xlim = c(0, durplot), ylim = c(20,35), cex.lab = 0.8)
    abline(h = 20, col = "black", lwd = 1, lty = 2)
    abline(h = 35, col = "black", lwd = 1, lty = 2)
    axis(side = 2,at = c(20,35))
    # axis(side = 1,at = ticks, labels = 1:length(ticks))
  } else {
    # Recordings without temperature
    par(fig = c(0, 1, 0, 0.80), new = T)
    plot_acc(timeline, Acceleration, durplot, ticks, metricName)

    par(fig = c(0, 1, 0.60, 0.95), new = T)
    plot_nonwear(timeline, M, durplot, ticks)
  }
}