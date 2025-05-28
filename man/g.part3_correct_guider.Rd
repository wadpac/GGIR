\name{g.part3_correct_guider}
\alias{g.part3_correct_guider}
\title{
  Correct guider estimates
}
\description{
  After guider has been derived for all nights in the recording
  this function is optionally applied to correct individual nights based
  on knowledge about other nights in the recording.
}
\usage{
   g.part3_correct_guider(SLE, desiredtz, epochSize)
}
\arguments{
  \item{SLE}{
    Object extracted with \link{g.sib.det}
  }
  \item{desiredtz}{
    See \link{GGIR}
  }
  \item{epochSize}{
    First value of parameter \code{windowsizes} as discussed in See \link{GGIR}.
  }
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}