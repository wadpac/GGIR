\name{g.getM5L5}
\alias{g.getM5L5}
\title{
  Extract M5 and L5 from time series
}
\description{
  Extract M5 and L5 from time series, function used by \link{g.analyse} and
  not intended for direct use by package user. Please see \link{g.analyse}
  for further clarification on functionalities
}
\usage{
  g.getM5L5(varnum, epochSize, t0_LFMF, t1_LFMF, M5L5res, winhr, qM5L5 = c(), 
            iglevels = c(), MX.ig.min.dur = 10, UnitReScale = 1000)	
}
\arguments{
  \item{varnum}{
    Numeric vector of epoch values
  }
  \item{epochSize}{
    Small epoch size in seconds
  }
  \item{t0_LFMF}{
    Start hour of the day for the M5L5 analyses, e.g. 0 for midnight
  }
  \item{t1_LFMF}{
    End hour of the day for the M5L5 analyses, e.g. 24 for midnight
  }
  \item{M5L5res}{
    Resolution of hte M5L5 analyses in minutes
  }
  \item{winhr}{
    windowsize of M5L5 analyses, e.g. 5 hours
  }
  \item{qM5L5}{
    Percentiles (quantiles) to be calculated over L5 and M5 window.
  }
  \item{iglevels}{
    See  \link{g.analyse}. If provided then the intensity gradient will be calculated
    for all MX windows larger or equal than argument MX.ig.min.dur
  }
  \item{MX.ig.min.dur}{
    Minimum MX duration needed in order for intensity gradient to be calculated
  }
  \item{UnitReScale}{
    Numeric value with which acceleration values are multiple to rescale
  }
}
\value{
  \itemize{
  \item DAYL5HOUR = Starting time in hours of L5
  \item DAYL5VALUE = average acceleration during L5
  \item DAYM5HOUR = Starting time in hours of M5
  \item DAYM5VALUE = average acceleration during M5
  \item V5NIGHT = average acceleration between 1am and 6am
  }
}
\examples{
  \dontrun{
    data(data.getmeta)
    g.getM5L5(varnum=data.getmeta,ws3=5,t0_LFMF=0,t1_LFMF=24,M5L5res=10,winhr=5)
  }
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
