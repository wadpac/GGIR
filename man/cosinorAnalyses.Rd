\name{cosinorAnalyses}
\alias{cosinorAnalyses}
\title{
  Apply cosinor anlaysis and extended cosinor analysis
}
\description{
  Applies cosinor anlaysis from the ActCR package to the time series
}
\usage{
  cosinorAnalyses(Xi, epochsize = 60, timeOffsetHours = 0)
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
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}