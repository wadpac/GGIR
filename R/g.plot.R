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
  
  #create coordinates for rectangles non-wear
  cd1 = g.createcoordinates(IMP$rout[,1], timeline) #nonwear
  cd2 = g.createcoordinates(IMP$rout[,2], timeline) #clipping
  cd3 = g.createcoordinates(IMP$rout[,3], timeline) #additional nonwear
  cd4 = g.createcoordinates(IMP$rout[,4], timeline) #protocol
  s0 = cd1$x0; 	s1 = cd1$x1
  b0 = cd2$x0; 	b1 = cd2$x1
  g0 = cd3$x0; 	g1 = cd3$x1
  w0 = cd4$x0; 	w1 = cd4$x1  
  dens = 30
  
  # start plot with empty canvas
  plot.new()	
  par(fig = c(0, 1, 0, 1), new = T)
  plot(seq(0, durplot), seq(0, durplot), col = "white", type = "l", axes = F, 
       xlab = "", ylab = "", main = paste0("device: ", monn, " | filename: ", fname), cex.main = 0.6)#dummy plot
  # lim = par("usr")
  # draw coloured rectangles
  Y0 = -50
  Y1 = c(durplot + n_ws2_perday)
  if (length(s0) > 0) { #non-wear
    for (ri in 1:length(s0)) {
      rect(s0[ri], Y0, s1[ri], Y1, border = colors()[148], col = colors()[148]) #red 404
    }
  }
  if (length(b0) > 0) { #clip
    for (ri in 1:length(b0)) {
      rect(b0[ri], Y0, b1[ri], Y1, border = colors()[464], col = colors()[464])
    }
  }
  if (length(g0) > 0) {
    for (ri in 1:length(g0)) { #end blue
      rect(g0[ri], Y0, g1[ri], Y1, border = colors()[150], col = colors()[150])
    }
  }
  if (length(w0) > 0) {
    for (ri in 1:length(w0)) { #end blue
      rect(w0[ri], Y0, w1[ri], Y1, border = colors()[24], col = colors()[24], density = dens)
    }
  }
  abline(v = timeline[length(timeline)], col = "blue", lwd = 1)	
  # derive acceleration signal
  if (length(which(colnames(M$metashort) == "ENMO")) > 0) {
    accel = M$metashort$ENMO
  } else {
    # Index of first acceleration metric in metashort other than ENMO, timestamp, or angle-related
    IndOtEnmo = which(colnames(M$metashort) %in% c("ENMO","timestamp","anglex","angley","anglez") == FALSE)[1]
    accel = as.numeric(as.matrix(M$metashort[,IndOtEnmo]))
  }
  accel2 = cumsum(c(0, accel))
  select = seq(1,length(accel2), by = ws2 / ws3)
  var1 = diff(accel2[round(select)]) / abs(diff(round(select[1:(length(select))])))
  if (length(timeline) > length(var1)) {
    var1 = c(var1, rep(0, length(var1) - length(timeline)))
  } else if (length(timeline) < length(var1)) {
    var1 = var1[1:length(timeline)]
  }
  
  MEND = length(timeline)
  ticks = seq(n_ws2_perday, nrow(M$metalong), by = n_ws2_perday)
  # creating plot functions to avoid duplicated code
  plotacc = function(timeline, var1, durplot, ticks) {
    plot(timeline, var1, type = "l", xlab = "Day", ylab = "Acceleration (g)",
         bty = "l", lwd = 0.1, xlim = c(0, durplot), ylim = c(0, 0.6), axes = FALSE)
    axis(side = 2, at = c(0, 0.2, 0.4, 0.6))
    axis(side = 1, at = ticks, labels = 1:length(ticks))
  }
  
  plotnw = function(timeline, M, durplot, ticks) {
    plot(timeline, M$metalong$nonwearscore, type = "s",
         xlab = "", ylab = "Non-wear score", axes = F,
         lwd = 0.1, xlim = c(0,durplot), ylim = c(0, 3))
    axis(side = 2,at = c(0, 1, 2, 3))
    axis(side = 1,at = ticks, labels = 1:length(ticks))
  }
  # plot data
  if (mon == 2 | (mon == 4 & dformat == 4)) {
    # Recordings with temperature
    par(fig = c(0,1,0,0.65), new = T)
    plotacc(timeline, var1, durplot, ticks)
    
    par(fig = c(0, 1, 0.45, 0.80), new = T)
    plotnw(timeline, M, durplot, ticks)
    
    par(fig = c(0, 1, 0.60, 1), new = T)
    plot(timeline, M$metalong$temperaturemean[1:MEND], type = "l",
         xlab = "", ylab = "Temp. (C)", axes = F, lwd = 0.1,
         xlim = c(0, durplot), ylim = c(20,35))
    abline(h = 20, col = "black", lwd = 1, lty = 2)
    abline(h = 35, col = "black", lwd = 1, lty = 2)
    axis(side = 2,at = c(20,35))
    axis(side = 1,at = ticks, labels = 1:length(ticks))
  } else {
    # Rcordings without temperature
    par(fig = c(0, 1, 0, 0.80), new = T)
    plotacc(timeline, var1, durplot, ticks)

    par(fig = c(0, 1, 0.60, 1), new = T)
    plotnw(timeline, M, durplot, ticks)
  }
}