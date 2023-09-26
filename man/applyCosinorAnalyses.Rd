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
  applyCosinorAnalyses(ts, qcheck, midnightsi, epochsizes)
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
  \item{epochsizes}{
    Epoch size for ts and qcheck respectively
  }
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}