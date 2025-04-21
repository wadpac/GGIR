\name{apply_cosinor_IS_IV_Analyses}
\alias{apply_cosinor_IS_IV_Analyses}
\title{
  Apply Cosinor Analyses to time series
}
\description{
  Wrapper function around \link{cosinor_IS_IV_Analyses} that first prepares the time series
  before applying the cosinorAnlayses
}
\usage{
  apply_cosinor_IS_IV_Analyses(ts, qcheck, midnightsi, epochsizes, threshold = NULL)
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
  \item{threshold}{
    See \link{cosinor_IS_IV_Analyses}
  }
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}