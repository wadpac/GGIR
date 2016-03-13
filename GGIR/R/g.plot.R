g.plot = function(IMP,M,I,durplot) {
  r1 = IMP$rout[,1]
  r2 = IMP$rout[,2]
  r3 = IMP$rout[,3]
  r4 = IMP$rout[,4]
  #==============================================
  # Generating time variable
  ws3 = M$windowsize[1]
  ws2 = M$windowsize[2]
  n_ws2_perday = (1440*60) / ws2
  n_ws3_perday = (1440*60) / ws3
  timeline = seq(0,ceiling(nrow(M$metalong)/n_ws2_perday),by=1/n_ws2_perday)  
  timeline = timeline[1:nrow(M$metalong)]
  fname = I$filename
  mon = I$monc
  monn = I$monn
  #=====================================================
  #create coordinates for rectangles non-wear
  cd1 = g.createcoordinates(r1,timeline) #nonwear
  cd2 = g.createcoordinates(r2,timeline) #clipping
  cd3 = g.createcoordinates(r3,timeline) #additional nonwear
  cd4 = g.createcoordinates(r4,timeline) #protocol
  s0 = cd1$x0; 	s1 = cd1$x1
  b0 = cd2$x0; 	b1 = cd2$x1
  g0 = cd3$x0; 	g1 = cd3$x1
  w0 = cd4$x0; 	w1 = cd4$x1  
  dens = 30
  #cutting out those silly "___" trains in some of the filenames (especially for Genea)
  fname2 = unlist(strsplit(fname,"_______"))
  if (length(fname2) > 1) fname = paste(fname2[1],fname2[2],sep="")
  plot.new()	
  par(fig=c(0,1,0,1),new=T)
  plot(seq(0,durplot),seq(0,durplot),col="white",type="l",axes=F,xlab="",ylab="",main=paste("device: ",monn," | filename: ",fname,sep=""),cex.main=0.6)#dummy plot
  lim = par("usr")
  Y0 = c(-1)
  Y1 = c(durplot+1)
  if (length(s0) > 0) { #non-wear
    for (ri in 1:length(s0)) {
      rect(s0[ri],Y0,s1[ri],Y1,border = colors()[148],col=colors()[148]) #red 404
    }
  }
  if (length(b0) > 0) { #clip
    for (ri in 1:length(b0)) {
      rect(b0[ri],Y0,b1[ri],Y1,border = colors()[464],col=colors()[464])
    }
  }
  if (length(g0) > 0) {
    for (ri in 1:length(g0)) { #end blue
      rect(g0[ri],Y0,g1[ri],Y1,border = colors()[150],col=colors()[150])
    }
  }
  if (length(w0) > 0) {
    for (ri in 1:length(w0)) { #end blue
      rect(w0[ri],Y0,w1[ri],Y1,border = colors()[24],col=colors()[24],density=dens)
    }
  }
  abline(v=timeline[length(timeline)],col="blue",lwd=1)	
  if (length(which(colnames(M$metashort) == "ENMO")) > 0) {
    accel = as.numeric(as.matrix(M$metashort[,which(colnames(M$metashort) == "ENMO")]))
  } else {
    accel = as.numeric(as.matrix(M$metashort[,which(colnames(M$metashort) == "BFEN")]))
  }
  accel2 =cumsum(c(0,accel))
  select = seq(1,length(accel2),by=ws2/ws3)
  var1 = diff(accel2[round(select)]) / abs(diff(round(select[1:(length(select))])))
  if (length(timeline) > length(var1)) {
    timeline = timeline[1:length(var1)]
  }
  MEND = length(timeline)
  if (mon == 2) { #for measurement with temperature
    par(fig=c(0,1,0,0.65),new=T)
    plot(timeline,var1,type="l",xlab="Day",ylab="Acceleration (g)",bty="l",lwd=0.1,xlim=c(0,durplot),ylim=c(0,0.6))
    par(fig=c(0,1,0.45,0.80),new=T) #0.55 1 -- 0.35 0.8
    plot(timeline,as.numeric(as.matrix(M$metalong[1:MEND,which(colnames(M$metalong) == "nonwearscore")])),type="s",xlab="",ylab="Non-wear score",axes=F,lwd=0.1,xlim=c(0,durplot),ylim=c(0,3))
    axis(side = 2,at=c(0,1,2,3))
    par(fig=c(0,1,0.60,1),new=T) #0.55 --1
    plot(timeline,as.numeric(as.matrix(M$metalong[1:MEND,which(colnames(M$metalong) == "temperaturemean")])),type="l",xlab="",ylab="Temp. (C)",axes=F,lwd=0.1,xlim=c(0,durplot),ylim=c(20,35))
    abline(h=20,col="black",lwd=1,lty=2)
    abline(h=35,col="black",lwd=1,lty=2)
    axis(side = 2,at=c(20,35))
  } else { #for measurement without temperature
    par(fig=c(0,1,0,0.80),new=T)
    plot(timeline,var1,type="l",xlab="Day",ylab="Acceleration (g)",bty="l",lwd=0.1,xlim=c(0,durplot),ylim=c(0,0.6))
    par(fig=c(0,1,0.60,1),new=T) #0.55 1 -- 0.35 0.8
    plot(timeline,as.numeric(as.matrix(M$metalong[1:MEND,which(colnames(M$metalong) == "nonwearscore")])),type="s",xlab="",ylab="Non-wear score",axes=F,lwd=0.1,xlim=c(0,durplot),ylim=c(0,3))
    axis(side = 2,at=c(0,1,2,3))
  }
}