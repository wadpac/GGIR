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
  resample(raw, rawTime, time, stop, type=1)	
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
    Number of rows in raw
  }
  \item{type}{
    integer to indicate type of interpolation, 1=linear, 2=nearest neighbour
  }
}
\examples{
  \dontrun{
    resample()
  }
}
