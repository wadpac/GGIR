\name{MXLX}
\alias{MXLX}
\title{
  MX LX analysis
}
\description{
  Identifies the most and least active X hours
}
\usage{
   MXLX(Y = NULL, X = 5, epochSize = 1, tseg = c(0, 24), resolutionMin = 10, shift = 0)
}
\arguments{
  \item{Y}{
    Vector with numeric time series
  }
  \item{X}{
    Single numeric value of X in hours
  }
  \item{epochSize}{
    Numeric epoch size in seconds of Y
  }
  \item{tseg}{
    Numeric vector of length two reflecting the time window in real clock hours
  }
  \item{resolutionMin}{
    Numeric value to indicate the resolution in minutes of the MX and LX search
  }
  \item{shift}{
    Numeric value to indicate the shift in hours of the start of vector Y relative 
    to the beginning of the day. For example, if Y represent noon-midnight then
    \code{shift} should be set to 12 and \code{tseg} to c(12, 24).
  }
}
\value{
  A data.frame with the:
    \itemize{
      \item LX the average value of Y for LX
      \item LXhr the hour in the day at which LX starts
      \item start_LX the index of Y where LX starts
      \item end_LX the index of Y where LX ends
      \item MX the average value of Y for MX
      \item MXhr the hour in the day at which MX starts
      \item start_MX the index of Y where MX starts
      \item end_MX the index of Y where MX ends
    }
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}