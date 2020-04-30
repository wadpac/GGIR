\name{g.part5.savetimeseries}
\alias{g.part5.savetimeseries}
\title{
  Saves oart 5 time series to csv files
}
\description{
  Not intended for direct use by GGIR users.
  Saves oart 5 time series to csv files as part of \link{g.part5}.
}
\usage{
  g.part5.savetimeseries(ts, LEVELS, desiredtz, rawlevels_fname,
  save_ms5raw_format="csv",
  save_ms5raw_without_invalid=TRUE)
}
\arguments{
  \item{ts}{
  }
  \item{LEVELS}{
  }
  \item{desiredtz}{
  }
  \item{rawlevels_fname}{
  }
  \item{save_ms5raw_format}{
  See \link{g.part5}
  }
  \item{save_ms5raw_without_invalid}{
  See \link{g.part5}
  }
}
\value{
  Function does not provide output, it only prepare data for saving
  and saves it to a file.
}
\author{
  Vincent T van Hees <vincentvanhees@gmail.com>
}