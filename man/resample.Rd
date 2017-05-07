\name{resample}
\alias{resample}
\docType{package}
\title{
Simple function using Rcpp
}
\description{
Simple function using Rcpp
}
\usage{
resample(raw, rawTime, time, stop)	
}
\arguments{
  \item{raw}{
stop-by-3 matrix with raw values of x, y and z.
}
  \item{rawTime}{
 vector with stop elements of raw time.
}
  \item{time}{
array with required time points.
}
  \item{stop}{
number of the last known point in raw and rawTime
}
  }
  
\examples{
\dontrun{
resample()
}
}
