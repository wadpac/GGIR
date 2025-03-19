\name{markerButtonForRest}
\alias{markerButtonForRest}
\title{
  Improve the labelling of rest with marker button data
}
\description{
  Improve the labelling of rest with marker button data.
  Currently only functional for Actiwatch and Philips Healthband.
}
\usage{
  markerButtonForRest(sibreport = NULL, ts = NULL, params_sleep = NULL)
}
\arguments{
  \item{sibreport}{
    sibreport data.frame produced by \link{g.sibreport}
  }
  \item{ts}{
    Data.frame with time series object passed on from \link{g.part5_analyseSegment} and \link{g.part5}.
  }
  \item{params_sleep}{
    See \link{GGIR}
  }
}
\value{
  Updated sibreport
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}