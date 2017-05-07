\name{GGIR_resample}
\alias{GGIR_resample}
\docType{package}
\title{
function using Rcpp
}
\description{
Function using Rcpp. Not designed for direct interaction by package user
}
\usage{
GGIR_resample(raw, rawTime, time, stop)
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
    GGIR_resample()
  }
}
