\name{g.part5.addNaps}
\alias{g.part5.addNaps}
\title{
  Add naps to part 5 time series (internal function)
}
\description{
  Add naps to part 5 time series (internal function)
  Internal function not intended for direct use by GGIR end-user.
}
\usage{
    g.part5.addNaps(sibreport = NULL, ts = NULL, params_general = NULL,
                           params_sleep = NULL)
}
\arguments{
  \item{sibreport}{
    sibreport data.frame produced by \link{g.sibreport}
  }
  \item{ts}{
    Data.frame with time series object passed on from \link{g.part5_analyseSegment} and \link{g.part5},
  }
  \item{params_general}{
    See \link{GGIR}
  }
  \item{params_sleep}{
    See \link{GGIR}
  }
}
\value{
  List with updated ts and sibreport objects and vector of row indices of sibreport
  that meet nap conditions
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}