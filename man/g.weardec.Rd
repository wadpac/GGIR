\name{g.weardec}
\alias{g.weardec}
\title{
  Detects whether accelerometer is worn
}
\description{
  Uses the object produced by \link{g.part1} to assess
  whether the accelerometer was worn
}
\usage{
  g.weardec(M, wearthreshold, ws2, nonWearEdgeCorrection = TRUE)
}
\arguments{
  \item{M}{
    Object produced by \link{g.getmeta}
  }
  \item{wearthreshold}{
    Number of axis that at least need to meet the non-wear criteria
  }
  \item{ws2}{
    Large windowsize used in seconds to apply non-wear detection
    Small window size not needed, because this is inherent to the object M
  }
  \item{nonWearEdgeCorrection}{
    Boolean to indicated whether the EdgeCorrection as described in 2013
    should be applied (default = TRUE, which is consistent with how code has been)
  }
  
}
\value{
  \itemize{
    \item \code{r1} Participant id extracted from file
    \item \code{r2} Night number
    \item \code{r3} Detected onset of sleep expressed as hours 
    since the previous midnight
    \item \code{LC} fraction of 15 minute windows with more than 5 percent
    clipping
    \item \code{LC2} fraction of 15 minute windows with more than 80
    percent clipping
  }
}
\examples{
  data(data.getmeta)
  output = g.weardec(M = data.getmeta, wearthreshold = 2, ws2 = 900)
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}