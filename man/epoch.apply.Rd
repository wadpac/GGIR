\name{epoch.apply}
\alias{epoch.apply}
\alias{epoch.mean}
\alias{epoch.sd}
\alias{epoch.median}
\alias{epoch.mad}
\alias{epoch.autocor}
\alias{epoch.quantile}
\alias{svm}
\title{
Compute epochal summary statistics.
}
\description{
Function copied from depricated package GENEAread. 
Computes epochal summary statistics for an "AccData" object, matrix, or
vector, and collates into a matrix or vector.
}
\usage{
epoch.apply(obj, epoch.size=10, incl.date = FALSE, FUN)

epoch.mean(obj, epoch.size=10, incl.date = FALSE, sqrt )
epoch.sd(obj, epoch.size=10, incl.date = FALSE, sqrt )
epoch.median(obj, epoch.size=10, incl.date = FALSE, sqrt )
epoch.mad(obj, epoch.size=10, incl.date = FALSE, sqrt )
epoch.autocor(obj, epoch.size=10, lag = 1, type = 
    c("correlation", "covariance", "partial"), incl.date = FALSE, sqrt)
epoch.quantile(obj, epoch.size = 10, 
    quantiles= c(0.1, 0.25, 0.5, 0.75, 0.9), incl.date = FALSE, sqrt )

svm(obj, sqrt )
}
\arguments{
  \item{obj}{The object to compute statistics for. Can be an "AccData"
  object, a matrix, or a vector.}
  \item{epoch.size}{Numeric giving intervals to consider and aggregate.
  For "AccData" \code{obj} taken as seconds. Otherwise, considered as 
  rows, or as individual readings.}
  \item{incl.date}{logical. If TRUE, include a column of times or 
  original indices with the results.}
  \item{FUN}{A function to be applied to each epoch.}
  \item{sqrt}{logical. If TRUE, the square rooted svm will be used in
  computations instead.}
  \item{lag}{Autocorrelation lag to compute.}
  \item{type}{Type of autocorrelation, as used in \code{\link{acf}}.}
  \item{quantiles}{Sample quantiles of SVM to compute.}
}
\details{
These functions compute epochal summary statistics for "AccData" 
objects, matrices and vectors.

\code{epoch.apply} is the general function - according to the size of
\code{epoch.size}, it splits up the obj into collections of consecutive 
rows, each with the same size. These are then successively supplied to 
\code{FUN} as its first argument. If the result of FUN is a single value, 
then the results are concatenated into a vector output. Otherwise, 
an array is formed with each row corresponding to a single epochal 
group. For AccData, the sampling frequency of the dataset is used to
interpret the epoch size in seconds. Otherwise, the raw record indices
are used. If incl.date is set, the original timestamp vector of the data, 
or the original indices, are downsampled and included as the first
column of the output.

The remaining functions are wrappers that compute various commonly useful
statistics -- in particular, applied to "AccData" objects and arrays,
they by default compute the epochal SVM mean, standard deviation, median,
median absolute deviation, and autocorrelation, and sample quantiles 
respectively. (Arrays are treated as each column representing the x, y,
and z components respectively.) Applied to vector input, processing will
occur without the SVM calculation. This behaviour may be overridden by 
the sqrt setting, which will force the function to use the squared
(default for arrays and "AccData") or original unit (default for vectors)
values in the statistical analysis.

\code{svm} acts identically to 'epoch.mean', with the epoch set to the 
sampling period. In other words, it computes the instantaneous sum of vector
magnitudes of the acceleration at each record point. The function takes 
"AccData", array and vector input. Note that if provided with an array with
4 or more columns, columns 2 to 4 are used -- the first column is regard 
as a timestamp and hence ignored.
}
\value{
A vector or array giving the computed epochal summaries. With \code{incl.date 
= TRUE}, the result is given as a data.frame suitable for plotting.
}

\seealso{
\code{\link{plot.AccData}}, \code{\link{summary.AccData}}, \code{\link{aggregate}},
\code{\link{acf}}
}
\examples{

dat <- read.bin(system.file("binfile/TESTfile.bin", package = "GGIR")[1]
    , calibrate = TRUE)

#look for the epochs that exceed a certain threshold 50% of the time
plot(epoch.apply( dat, epoch.size = 3 , 
    FUN = function(t) mean(abs(svm(t) -1)>0.2)> 0.5 ), type = "l")

plot(dat[,1], svm(dat), log = "y", pch = ".")
lines(epoch.mean(dat, incl.date = TRUE), lwd = 2)
lines(epoch.mean(dat, epoch.size = 30, incl.date = TRUE), col = 2, lwd = 2)
#this should give all the same results, but by a different way
lines(epoch.apply(dat, epoch.size = 30, 
    FUN = function(A) mean(svm(A, FALSE)), incl.date = TRUE), col = 3)
epsize = 30; lines(epoch.apply(dat, epoch.size = epsize, 
    FUN = function(t) median(t[,1])), epoch.apply(dat, epoch.size = epsize, 
    FUN = function(A) mean(svm(A, FALSE))), col = 4)
#note this is different
lines(epoch.apply(dat, epoch.size = epsize, 
    FUN = function(t) median(t[,1])),epoch.apply(dat, epoch.size = epsize, 
    FUN = function(A) mean(svm(A, sqrt = TRUE)))^2, col = 5)

#plot some statistics
par(mfrow = c(5,1), mar = c(1,4.5,1,1))
plot(epoch.sd(dat), type="l")
plot(epoch.median(dat), type= "l")
plot(epoch.mad(dat), type= "l")
plot(epoch.autocor(dat), type= "l")
tmp = epoch.quantile(dat, quantiles= c(0.1, 0.25, 0.5, 0.75, 0.9)); 
matplot(tmp, type = "l")


}
\keyword{ts}
