# GGIR used to depend on CRAN package GENEAread as developed by Joss Langford and Zhou Fang.
# Now GENEAread is depricated the essential parts of the code has been copied to GGIR to ensure
# ongoing functionality.

#utility functions for GENEAread
#many of these are invisible by default, change to exportPattern(".") in
#NAMESPACE if you want to make use of them in your own code!
#----------------------------------------

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

summary.AccData <- function(object, ...){
  summary(epoch.sd(object, 10))
}

# moved here from read.bin:
print.VirtAccData <- function(x, ...){
  cat("[Virtual GENEAread dataset]: ", length(x$data.out)*x$nobs, "records at", 
      round(x$freq,2), "Hz (Approx ", 
      round(56 * as.double(length(x$data.out) * x$nobs )/1000000)   ,"MB of RAM)\n")
  cat(  format.GRtime(x$data.out[1], format = "%y-%m-%d %H:%M:%OS3 (%a)")," to ", 
        format.GRtime(utils::tail(x$data.out,1) + x$nobs /x$freq, format = "%y-%m-%d %H:%M:%OS3 (%a)"), "\n")
  cat("[", x$filename, "]\n")
}
print.AccData <- function(x, ...){
  cat("GENEAread dataset: ", nrow(x$data.out), "records at", round(x$freq,2),
      "Hz (Approx ", round(utils::object.size(x$data.out)/1000000) ,"MB of RAM)\n")
  cat(format.GRtime(x$data.out[1,1], format = "%y-%m-%d %H:%M:%OS3 (%a)"),
      " to ", format.GRtime(utils::tail(x$data.out[,1],1),format = "%y-%m-%d %H:%M:%OS3 (%a)"), "\n")
  #}
  cat("[", x$filename, "]\n")
}

"[.AccData"    <- function (x, i=1:dim(x$data.out)[1], j=NULL, drop=TRUE) {
  if (is.null(j)){
    x$page.timestamps = x$page.timestamps[ unique(ceiling(i/300))]
    x$data.out = x$data.out[i,]
    return(x)
  }
  if ((length(j) == ncol(x$data.out) )&& (max(j) <= 1)) j = which(j)
  if ( j[1] == 1 ){
    
    if (length(j) != 1){
      value = x$data.out[i, j[-1] , drop = F]
      
      return( data.frame( time = convert.time(x$data.out[i,1, drop = T]), value  ))
    } else{
      return (convert.time(x$data.out[i,1, drop = drop]))
    }
  } else {
    return(x$data.out[i,j, drop=drop])
  }
}

#dollar operator: accept some keywords, in addition to the usual
"$.AccData" <- function(x, name){
  nmatch <- try(match.arg(name, c("time", "x", "y", "z", "xyz", 
                                  "temperature", "button", "voltage",
                                  "light", "svm")), silent = TRUE)
  if (inherits(nmatch, "try-error")){
    class(x) <- NULL
    return(x[[name, exact = FALSE]])
  }else { 
     ind = switch(nmatch, time = 1, x = 2, y = 3, z = 4, xyz = 2:4, 
                 temperature = 7, button = 6, light = 5, voltage = 8, svm = 9)
    if (identical(ind, 8)){
      return(rep(x$page.volt, each = ceiling(nrow(x)/length(x$page.volt)) )[1:nrow(x)])
    } else if (identical(ind, 9)){
      return(svm(x))
    } else {
      return(x[,ind])
    }
  }
}

c.AccData <- function(..., recursive=FALSE){
  tmp = list(...)
  out = list()
  out$data.out = NULL
  out$page.timestamps = NULL
  for (i in 1:length(tmp)){
    out$data.out = rbind(out$data.out, tmp[[i]]$data.out)
    out$page.timestamps = c(out$page.timestamps, tmp[[i]]$page.timestamps)
  }
  out$freq = tmp[[1]]$freq
  out$filename = tmp[[1]]$filename
  class(out) = class(tmp[[1]])
  out
}