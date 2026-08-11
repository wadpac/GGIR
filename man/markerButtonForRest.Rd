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
  markerButtonForRest(sibreport, params_sleep, ts)
}
\arguments{
  \item{sibreport}{
    sibreport data.frame produced by \link{g.sibreport}
  }
  \item{params_sleep}{
    See \link{GGIR}
  }
  \item{ts}{
    Data.frame with time series object passed on from \link{g.part5_analyseSegment} and \link{g.part5}.
  }
}
\value{
  Updated sibreport
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}