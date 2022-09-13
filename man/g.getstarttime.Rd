\name{g.getstarttime}
\alias{g.getstarttime}
\title{
  Extract start time of a measurement
}
\description{
  Extract start time of a measurement. GGIR calculates all timestamps by
  using the first timestamp and sample frequency. Not intended
  for direct use by package user
}
\usage{
  g.getstarttime(datafile, P, header, mon, dformat, desiredtz,
  configtz = NULL)
}
\arguments{
  \item{datafile}{
   Full path to data file
  }
  \item{P}{
    Object extracted with \link{g.readaccfile}
  }
  \item{header}{
    File header extracted with \link{g.inspectfile}
  }
  \item{mon}{
    Same as in \link{g.dotorcomma}
  }
  \item{dformat}{
    Same as in \link{g.dotorcomma}
  }
  \item{desiredtz}{
    Same as in \link{g.dotorcomma}
  }
  \item{configtz}{
    Same as in \link{g.dotorcomma}
  }
}
\value{
  The starttime
}
\author{
Vincent T van Hees <v.vanhees@accelting.com>
}