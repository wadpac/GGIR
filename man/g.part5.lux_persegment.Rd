\name{g.part5.lux_persegment}
\alias{g.part5.lux_persegment}
\title{
  Extract key lux variables per segment of the data.
}
\description{
  Extracts per segment of the day: mean lux, time above 1000 lux, time awake,
  and time LUX imputed. Function not intended
  for direct use by package user.
}
\usage{
  g.part5.lux_persegment(ts, sse, LUX_day_segments, epochSize, desiredtz = "")
}
\arguments{
  \item{ts}{
    Data.frame with time series
  }
  \item{sse}{
    Indices corresponding to the current time window (e.g. MM or WW)
  }
  \item{LUX_day_segments}{
    See \link{GGIR}
  }
  \item{epochSize}{
    Numeric epoch size in seconds
  }
  \item{desiredtz}{
    See \link{GGIR}
  }
}
\value{
 List with values (vector) of the derived variables and corresponding names (vector).
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
