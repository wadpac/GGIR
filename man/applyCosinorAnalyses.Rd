\name{applyCosinorAnalyses}
\alias{applyCosinorAnalyses}
\title{
  Apply Cosinor Analyses to time series
}
\description{
  Wrapper function around \link{cosinorAnalyses} that first prepares the time series
  before applying the cosinorAnlayses
}
\usage{
  applyCosinorAnalyses(ts, qcheck, midnightsi, shortEpoch, longEpoch)
}
\arguments{
  \item{ts}{
    Data.frame with timestamps and acceleration metric.
  }
  \item{qcheck}{
    Vector of equal length as number of rows in ts with value 1 for invalid
    timestamps, 0 otherwise.
  }
  \item{midnightsi}{
    Indices for midnights in the time series
  }
  \item{shortEpoch}{
    First element of argument windowsizes, the resolution of the ts and qcheck values.
  }
  \item{longEpoch}{
    Second element of argument windowsizes, the resolution of the midnightsi 
    values in seconds.
  }
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}