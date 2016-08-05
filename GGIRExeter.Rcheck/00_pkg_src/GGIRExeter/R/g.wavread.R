g.wavread = function(binfile,start=1,end=100,units="minutes") {
#   start=1
#   end=100
#   binfile = "D:/accelerometry/wav/test/test.wav"
#   units="minutes"
  if (start == 0) start = 1
  #-----------------------------------------------------
  # get data
  S = tuneR::readWave(binfile, from = start, to = end, units = units)
  B = tuneR::extractWave(S, from = start, to = length(S),xunit = c("samples", "time")) #interactunitsteractive()
  S = as.data.frame(S)
  B = as.data.frame(B)
  
  #-------------------------------------------------------
  # extract info from header
  header = rownames(read.csv(binfile,nrow=13,header=TRUE))
  P = sapply(as.character(header),function(x) {
    tmp = unlist(strsplit(x,": "))
    if (length(tmp) == 1) {
      tmp = c(tmp, NA)
    }
    tmp
  })
  P = as.data.frame(t(P))
  names(P) = c("hnames","hvalues")
  # get header part that can be extracted with readWave
  H = tuneR::readWave(binfile, from = 1, to = 3600,units = c("seconds"), header = TRUE) #get wav file header
  #-----------------------------------------------
  # scale acceleration
  scale = as.numeric(as.character(P$hvalues[which(P$hnames == "Scale-1" | P$hnames == "Scale-2" | P$hnames == "Scale-3")]))
  if (length(scale) != 3) scale = rep(scale[1],3)
  range = 2^(H$bits -1) # should be 32768 for 16 bit
  x = (B$C1/range) * scale[1]
  y = (B$C2/range) * scale[2]
  z = (B$C3/range) * scale[3]
  rawxyz = cbind(x,y,z)
  #---------------------------------------------
  # get time (we only need first timestamp
  A = scan(binfile,what="character",nlines=12,quiet=TRUE)
  timestamp = paste0(A[which(A == "ICMTz")+1:2],collapse=" ")
  if (length(timestamp) == 0) { #if not possible use other time in fileheader
    timestamp = as.character(P$hvalues[which(P$hnames == "Start")]) 
  }
  
  # extra information is available in channel 4, but no clear documentation found how it should be interpretted
  # therefore i will not extract temperature for now, and rely on auto-calibration as performed before .wav file generation
  # g = (B$C4/ range) # temperature, light and battery ?
  invisible(list(rawxyz=rawxyz,header=P,timestamp = timestamp))
  
}