\name{cosinorAnalyses}
\alias{cosinorAnalyses}
\title{
  Apply cosinor anlaysis and extended cosinor analysis
}
\description{
  Applies cosinor anlaysis from the ActCR package to the time series, as well
  as IV, IS and phi estimates.
}
\usage{
  cosinorAnalyses(Xi, epochsize = 60, timeOffsetHours = 0, threshold = NULL)
}
\arguments{
  \item{Xi}{
    Vector with time series of movement indicators
  }
  \item{epochsize}{
    Numeric epochsize in seconds
  }
  \item{timeOffsetHours}{
    Numeric time in hours relative to next midnight
  }
  \item{threshold}{
    Numeric value to use as threshold to distinguish inactivity from active behaviour
    for the IV and IS analysis. GGIR uses parameter threshold.lig to set this threshold.
  }
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}