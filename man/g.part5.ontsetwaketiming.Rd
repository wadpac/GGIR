\name{g.part5.onsetwaketiming}
\alias{g.part5.onsetwaketiming}
\title{
  Identify wake and sleepperiod window timing
}
\description{
  Not intended for direct use by GGIR users.
  Labels timing of wakeing up and sleep onset as part of \link{g.part5}.
}
\usage{
  g.part5.onsetwaketiming(qqq, ts, min, sec, hour, timewindowi)
}
\arguments{
  \item{qqq}{
    Start and end index of window to analyses
  }
  \item{ts}{
    Data.frame with time series as created in \link{g.part5}
  }
  \item{min}{
    Numeric vector with minute values
  }
  \item{sec}{
    Numeric vector with second values
  }
  \item{hour}{
    Numeric vector with hour values
  }
  \item{timewindowi}{
    Character to indicate what timewindow definition is used either "MM" or "WW"
  }
}
\value{
  A list with objects: wake, onset, wakei, onseti, skiponset, and skipwake.
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}