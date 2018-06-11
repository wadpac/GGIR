# GGIR used to depend on CRAN package GENEAread as developed by Joss Langford and Zhou Fang.
# Now GENEAread is depricated the essential parts of the code has been copied to GGIR to ensure
# ongoing functionality.

svm <- function(obj,  sqrt ){
  if (missing(sqrt)) sqrt = (length(dim(obj)) < 2)  
  if (length(dim(obj)) == 2) {
    obj = rowSums(obj[,-2:0 + min(ncol(obj), 4)]^2)
    if (sqrt) obj = sqrt(obj)
  } else {
    if (!sqrt) obj = obj^2
  }
  obj
}

#epoch.apply wrapper function for bapply.basic
epoch.apply <- function(obj, epoch.size=10, incl.date = FALSE, FUN){
  sampling.freq = 1
  if (length(dim(obj)) < 2) obj = matrix(obj, ncol = 1)
  ind = 1:nrow(obj)
  if (inherits(obj, "AccData")){
    sampling.freq = obj$freq
    times = obj[,1]
    obj = obj$data.out#[,2:4]
  } else {
    times = ind
  }
  epoch.size = floor(epoch.size* sampling.freq)
  if (length(FUN(obj[1:epoch.size,])) > 1){
    obj = bapply(ind, epoch.size, function(t) FUN(obj[t,]))
  } else {
    obj = bapply.basic(ind, epoch.size, function(t) FUN(obj[t,]))
  }
  if (incl.date){
    obj = data.frame(time = times[seq(1, length(times)-epoch.size+1, by = epoch.size) + ceiling(epoch.size/2)],value = obj)
  }
  obj
  
}


epoch.mean<- function(obj, epoch.size=10, incl.date = FALSE, sqrt) {if (missing(sqrt)) sqrt = (length(dim(obj)) < 2);epoch.apply(obj, epoch.size = epoch.size, FUN = function(x) mean(svm(x, sqrt)), incl.date= incl.date)}
epoch.sd<- function(obj, epoch.size=10, incl.date = FALSE, sqrt) {if (missing(sqrt)) sqrt = (length(dim(obj)) < 2); epoch.apply(obj, epoch.size = epoch.size, FUN = function(x) sd(svm(x, sqrt)), incl.date= incl.date)}
epoch.median<- function(obj, epoch.size=10, incl.date = FALSE, sqrt){if (missing(sqrt)) sqrt = (length(dim(obj)) < 2); epoch.apply(obj, epoch.size = epoch.size, FUN = function(x) median(svm(x, sqrt)), incl.date= incl.date)}
epoch.mad<- function(obj, epoch.size=10, incl.date = FALSE, sqrt){if (missing(sqrt)) sqrt = (length(dim(obj)) < 2); epoch.apply(obj, epoch.size = epoch.size, FUN = function(x) mad(svm(x, sqrt)), incl.date= incl.date)}
epoch.autocor<- function(obj, epoch.size=10, lag = 1, type = c("correlation", "covariance", "partial"), incl.date = FALSE, sqrt ){if (missing(sqrt)) sqrt = (length(dim(obj)) < 2); epoch.apply(obj, epoch.size = epoch.size, FUN = function(x) acf(svm(x, sqrt), type = type , lag.max = max(lag), plot = F)$acf[lag +1], incl.date= incl.date)}

epoch.quantile <- function(obj, epoch.size = 10, quantiles= c(0.1, 0.25, 0.5, 0.75, 0.9), incl.date = FALSE, sqrt ){if (missing(sqrt)) sqrt = (length(dim(obj)) < 2); epoch.apply(obj, epoch.size = epoch.size, FUN = function(x) quantile(svm(x, sqrt), prob = quantiles, name = F), incl.date= incl.date)}

bapply.basic <- function(X, k, FUN) { res = rep(0, floor(length(X) / k)); for (i in 1:floor(length(X)/k)) res[i] = FUN(X[ (i-1)*k + 1:k]); return(res)}

bapply <- function(X, k, FUN) { dimout = length(FUN(X[1:k])); res = matrix(0, dimout, floor(length(X) / k)); for (i in 1:floor(length(X)/k)) res[(i-1)* dimout + 1:dimout] = FUN(X[ (i-1)*k + 1:k]); return(t(res))}




