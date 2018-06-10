# GGIR used to depend on CRAN package GENEAread as developed by Joss Langford and Zhou Fang.
# Now GENEAread is depricated the essential parts of the code has been copied to GGIR to ensure
# ongoing functionality.

#utility functions for GENEAread
#many of these are invisible by default, change to exportPattern(".") in
#NAMESPACE if you want to make use of them in your own code!

constrain <- function(x, minimum, maximum){
  if (missing(maximum)) maximum = Inf
  if (missing(minimum)) minimum = -Inf
  
  if (minimum > maximum) {
    temp <- minimum
    minimum <- maximum
    maximum <- temp
  }
  x = replace(x, which(x > maximum), maximum)
  x = replace(x, which(x < minimum), minimum)
  x
}

shift = function( x, offset, expand = 0, fill = c("zero", "edge", "loop", "mean")){
  fill = match.arg(fill)
  if (length(dim(x)) == 2){
    if (length(expand) == 1) expand = rep(expand,2)
    n = nrow(x)
    p = ncol(x)
    xlim = nrow(x) + expand[1]
    ylim = ncol(x) + expand[2]
    x2 = matrix(0, xlim, ylim)
    if (fill =="mean") x2 = matrix(mean(x), xlim, ylim)
    TL = c(1,1) - pmin(0, offset)
    #BR = pmin(c(nrow(x) , ncol(x)) - TL + offset + c(1,1), c(xlim, ylim)) - offset
    BR = pmin(offset + c(n,p) , c(xlim, ylim)) - offset
    x2[0:((BR-TL)[1]) +max(offset[1],0)+1,0:((BR-TL)[2])+max(offset[2],0)+1] =  x[TL[1]:BR[1], TL[2]:BR[2]]
    if ((fill != "zero") && (fill != "mean") ){
      #top left corner
      #if (min(offset) > 0){
      #
      #if (fill == "loop"){
      #	x2[1: offset[1], 1:offset[2]] = x[ 1:offset[1] + n - offset[1], 1:offset[2] + p - offset[2]]
      #} else {
      #x2[1: offset[1], 1:offset[2]] = x[1,1]
      #}
      #}
      #top edge
      if (offset[1] > 0) x2[1:offset[1],] = matrix(shift(x[1,], offset[2], expand = ylim - p, fill="edge"), nrow  =  offset[1], byrow = T, ncol = ylim)
      #left edge
      if (offset[2] > 0) x2[,1:offset[2]] = matrix(shift(x[,1], offset[1], expand = xlim - n, fill="edge"), ncol = 1)
      #bottom edge
      if (n + offset[1] < xlim) x2[(n + offset[1]+1) :xlim,] = matrix(shift(x[n,], offset[2], expand = ylim - p, fill="edge"), nrow = xlim - offset[1] - n,  ncol= ylim, byrow = T)
      #right edge
      if (p + offset[2] < ylim) x2[,(p + offset[2]+1) :ylim] = matrix(shift(x[,p], offset[1], expand = xlim - n, fill="edge"), ncol = 1)
      if (fill == "loop") print("Currently umimplemented loop fill, edging instead")
      #if (sum(expand) != 0)	print("Warning, loop results may not make sense if matrix size changes")
    }
  } else {
    #return(drop(shift(x=matrix(x, ncol=1), offset = c(offset,0), expand =c(expand, 0), fill = fill)))
    offset = offset[1]
    expand  = expand[1]
    x2 = rep(0, length(x) + expand)
    beg = 1 - pmin(offset, 0)
    end =  min (offset + length(x), length(x2)) - offset # min(  length(x) - beg + offset + 1, length(x) + expand) - offset
    x2[( max(offset,0) +1) : min(length(x2), length(x)+ offset)] = x[beg:end]
    if (fill == "loop"){
      if (offset > 0){
        x2[1:offset] = utils::tail(x, offset)
      }else{
        x2[((length(x) + offset + 1): length(x2))] = x[1:(-offset)]
      }
    } else if (fill == "edge"){
      if (offset > 0){
        x2[1:offset] = x[1]
      }
      if ((length(x) + offset) <  length(x2)){
        x2[ (length(x) + offset +1) : length(x2)] = utils::tail(x,1)
      }
    }
  }
  x2
}


#puts a vector into the range 0-1
conv01 <- function(x){
  (x - min(x))/ (max(x)- min(x))
}

#convert time intervals
#size <- desired number of measurements

get.intervals = function(x, start=0, end = 1, length = NULL, time.format = c("auto", "seconds", "days", "proportion", "measurements", "time"), incl.date = FALSE, simplify = TRUE ,read.from.file=FALSE, size=Inf, ...){
  if (inherits(x, "VirtAccData")) read.from.file = TRUE
  #virtual database, go get the relevant period first
  
  if (read.from.file){
    #rerun read.bin to grab the relevant part of the data
    #note: only time-like or proportion addresses of start and end work at this point....
    argorig = x$call
    readargs = c("gain", "offset", "luxv", "voltv","warn", "verbose", "do.temp", "calibrate", "downsample", "blocksize")
    
    argl<-as.list(match.call())
    argind<-pmatch(names(argl),readargs)
    readargs = readargs[na.omit(argind)]
    argind<-which(!is.na(argind))
    argorig = c( argl[argind], argorig)
    argorig = argorig[which((!duplicated(names(argorig))) & (names(argorig)!= ""))]
    argorig$virtual = F
    argorig$start= start
    argorig$end = end
    
    x = do.call(read.bin, args = argorig)
    
    start = 0
    end = 1
    time.format = "proportion"
  }
  
  sampling.freq = 100
  time.format = match.arg(time.format)
  
  
  if (length(start) > 1) {
    end = (start)[2]
    start = (start)[1]
  }
  #auto detect time format
  if (time.format == "auto"){
    if (is.character(start)){
      time.format = "time"
    }else if (start <1){
      time.format = "proportion"
    }else if (floor(start) == start) {
      time.format = "seconds"
    } else {
      time.format = "days"
    }
  }
  
  
  if (is.list(x)){
    
    if ((time.format == "time")||(time.format =="seconds")){
      times = x[,1]
    }
    
    sampling.freq = x$freq
    if (simplify) {
      x = x$data.out[,(2- incl.date):4]
    }
    
  } else {
    if (ncol(x) == 3) x =cbind (1:nrow(x)/sampling.freq, x)
    
    if ((time.format == "time") ){
      times =x[,1]
    }
    if (!incl.date)  x = x[,-1]
  }
  
  if (simplify){
    n = nrow(x)
  } else {
    n = nrow(x$data.out)
  }
  
  if (time.format == "time"){
    #if (nchar(start) >= 11){
    #tmp = strsplit(start, " ")
    #tmp = tmp[[1]][which( nchar(tmp[[1]]) >= 5)]
    #if (length(tmp) > 1){
    #start = tmp[1]
    #end = tmp[2]
    #}
    #}
    #todo: make this work
    
    start = parse.time(start, format = "seconds")
    t1midnight = floor(times[1] / (60*60*24)) * 60*60*24
    t1 = times[1]
    if (start < t1midnight)	start = start + t1midnight
    if (start < t1) start = start + 60*60*24
    start = findInterval ( start, times, all.inside = T)
    t1 = times[start+1]
    
    if (is.character(end)){
      end = parse.time(end, format = "seconds")
      if (end < t1midnight){
        if (end >= 24*60*60){
          end = end + t1midnight
        } else {
          end = end +ceiling((t1 - end)/(60*60*24)) * 60*60*24
        }
      }
      end = findInterval(end, times, all.inside = T) + 1
      
      
    } else {
      length = end * sampling.freq
      end = NULL
    }
    time.format = "measurements"
  }
  
  #if (time.format == "date"){
  #start = (start - times[1]) * 60*60*24
  #if (inherits(end, "times2")){
  #end =(end - times[1]) * 60*60*24
  #}
  #time.format = "seconds"
  #}
  
  if (is.null(length)){
    if ( end < start){
      length = end
      end = NULL
    }
  }
  
  if (!is.null(length)) {
    if ((time.format == "proportion") && (length >= 1)){
      time.format ="seconds"
      start = (start * n/sampling.freq)
    }
    end = start + length
  }
  
  #convert into measurements
  
  if (time.format == "proportion"){
    start = ceiling(start * n)
    end  = floor(end * n)
  } else if (time.format == "seconds") {
    if (exists("times") && (start > times[1])){
      start = findInterval(start, times)
      end = findInterval(end+0.01, times)
    } else {
      start = ceiling(start * sampling.freq)
      end = floor(end * sampling.freq)
    }
  } else if (time.format == "days"){
    start = ceiling(start * sampling.freq*60*60*24)
    end = floor(end * sampling.freq*60*60*24)
  }
  
  start = max(start,1)
  end = min(end, n)
  
  if (incl.date) cat("Extracting time interval: ", format(convert.time(c(x[start,1], x[end,1]))) , "\n")
  ind = start:end
  tmp = 1
  if (length(ind) > size){
    tmp = ceiling(length(ind) / size)
    ind = ind[(ind %% tmp == 0)]
  }
  
  x = x[ind,]
  if (is.list(x)) x$freq = x$freq /tmp
  
  return(x)
}

#c.AccData = function(x,y){
#x$data.out = rbind(x$data.out, y$data.out)
#x$page.timestamps = c(x$page.timestamps, y$page.timestamps)
#x
#}


dim.AccData <- function(x) dim(x$data.out)

plot.AccData <- function(x, y=NULL, what = c("sd", "mean", "temperature", "light", "voltage"), draw = TRUE,resolution = 200,...){
  what = match.arg(what)
  if (is.null(y)){
    epoch = floor(nrow(x)/resolution + 1)/x$freq
    if (what == "sd"){
      obj = epoch.apply(x, epoch, TRUE, FUN = function(t) sd(svm(t)))
    } else if (what == "mean"){
      obj = epoch.apply(x, epoch, TRUE, FUN = function(t) mean(svm(t)))
    } else if (what == "temperature"){
      obj = epoch.apply( x, epoch, incl.date = T, function(t) mean(t[,7]))
    } else if (what == "voltage"){
      obj = x[(1: floor(nrow(x) / epoch) * epoch), c(1, 7)]
      obj[2] = x$voltage[(1: floor(nrow(x) / epoch) * epoch)]
    } else {
      #current workaround for light?
      obj = epoch.apply(x, epoch, incl.date = T, FUN = function(t) max(t[,5]))
    }
    if (draw) plot(convert.time(obj[,1]) ,obj[,2] ,  type = "l", xlab = "Time", ylab = what, ...)
    return(invisible(obj))
  } else {
    plot(convert.time(x[,1]), y, ...)
    return(NULL)
  }
}

#quantile version of bt
"%bq%" = function(X, y){
  if (is.character(y)){
    if (length(y) == 4) y = paste(y[1],y[2], ",",y[3], y[4], sep="")
    y = strsplit(y, ",")[[1]]
    nc = nchar(y[2])
    yl = quantile(X, c(as.numeric(substring(y[1], 2)), as.numeric(substring(y[2], 1,nc - 1))))
    if (substr(y[1],1,1) == "["){
      res = (X >= yl[1])
    }else {
      res = (X >  yl[1])
    }
    if (substr(y[2],nc,nc) == "]"){
      res = res &(X <= yl[2] )
    }else {
      res = res & (X < yl[2])
    }
  } else {
    y = quantile(X, y)
    
    res = (X >= y[1] ) & (X<= y[2])
  }
  res
}

#'between' operator for convenience
#takes [min, max), or c("[", min, max, "]") style second terms
#default is [min, max] for c(,) terms
"%bt%" = function(X, y){
  if (is.character(y)){
    if (length(y) == 4) y = paste(y[1],y[2], ",",y[3], y[4], sep="")
    y = strsplit(y, ",")[[1]]
    if (substr(y[1],1,1) == "["){
      res = (X >= as.numeric(substring(y[1], 2)))
    }else {
      res = (X > as.numeric(substring(y[1], 2)))
    }
    nc = nchar(y[2])
    if (substr(y[2],nc,nc) == "]"){
      res = res &(X <= as.numeric(substring(y[2],1, nc -1)))
    }else {
      res = res & (X < as.numeric(substring(y[2], 1,nc - 1)))
    }
  } else {
    res = (X >= y[1] ) & (X<= y[2])
  }
  res
}

removeZero <- function(obj){
  obj[which(obj!=0)]
}


seq.log <- function(from = 1, to = 1, length.out = 50, add.zero = FALSE, shifting = 0){
  res = exp(seq(from = log(from + shifting), to = log(to + shifting), length=length.out - add.zero)) - shifting
  if (add.zero) {
    if (from > to) {
      res = c(res,0)
    } else {
      res = c(0,res)
    }
  }
  res
}

expand <- function(X, length = (length(X)*100)){
  c(rep(X, each = floor(length / length(X))), rep(utils::tail(X,1), length - length(X) * floor(length/length(X))))
}

summary.AccData <- function(object, ...){
  summary(epoch.sd(object, 10))
}


# Plot a line graph, with breaks when things change too much
# (assume x is sorted)
pseudolines <- function(x,y=NULL, max.shift, new = FALSE,...){
  if (new) plot(x,y, type = "n", ...)
  if (is.null(y)){
    if ((length(dim(x)) > 1) && (ncol(x)==2 ) ){
      y = x[,2]
      x = x[,1]
    } else {
      y = drop(x)
      x = 1:length(y)
    }
  }
  n = length(y)
  if (missing(max.shift)) max.shift = max(sqrt(2) /n, 0.05)
  xrng = par("usr")
  yrng = xrng[4] - xrng[3]
  xrng = xrng[2] - xrng[1]
  
  deltas = sqrt(rowSums(cbind(diff(x/xrng), diff(y/yrng))^2))
  deltas = (deltas > max.shift)
  switches = c(which(diff(deltas) != 0), n)
  
  pos = 1
  for (i in 1: (length(switches)+1)){
    type = deltas[pos]
    leng = switches[i] - pos + 2
    if (type == TRUE){
      leng = leng - 2
      if (leng > 0){
        graphics::points(x[pos + 1:leng ], y[pos + 1:leng], ...)
      }
      leng = leng + 2
    }else{
      lines(x[pos -1+ 1:leng], y[pos -1+ 1:leng], ...)
    }
    pos = pos + leng-1
    if (pos > n ) break
  }
}

