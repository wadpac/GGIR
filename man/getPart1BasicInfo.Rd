\name{getPart1BasicInfo}
\alias{getPart1BasicInfo}
\title{
  Get basic info form the part 1 milestone file
}
\description{
  Internal function to get basic info form the part 1 milestone file
}
\usage{
  getPart1BasicInfo(fn, idloc, tz)
}
\arguments{
  \item{fn}{
    Character with full file path to RData file as stored by \link{g.part1}
  }
  \item{idloc}{
    See \link{GGIR}
  }
  \item{tz}{
    As passed by \link{GGIR} and equal to parameter desiredtz as documented in \link{GGIR}
  }
}
\value{
  Data.frame with ID, start and end time of the recording,
  filename and brand name.
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}