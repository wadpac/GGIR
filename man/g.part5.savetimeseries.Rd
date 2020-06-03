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
  save_ms5raw_without_invalid=TRUE,
  DaCleanFile=c(), includedaycrit.part5=2/3, ID=c())
}
\arguments{
  \item{ts}{
  }
  \item{LEVELS}{
  }
  \item{desiredtz}{
     See \link{g.getmeta}.
  }
  \item{rawlevels_fname}{
  }
  \item{save_ms5raw_format}{
  See \link{g.part5}
  }
  \item{save_ms5raw_without_invalid}{
  See \link{g.part5}
  }
  \item{DaCleanFile}{
  Content of data_cleaning_file as documented in \link{g.report.part5}.
  Only used in this function if save_ms5rawlevels is TRUE,  and it 
  only affects the time 
  series files stored.
  }
  \item{includedaycrit.part5}{
  See \link{g.report.part5}. Only used in this function if
  save_ms5rawlevels is TRUE,  and it only affects the time 
  series files stored.
  }
  \item{ID}{
  If data_cleaning_file is used then this argument specifies
  which participant ID the data correspond with.
  }
}
\value{
  Function does not provide output, it only prepare data for saving
  and saves it to a file.
}
\author{
  Vincent T van Hees <vincentvanhees@gmail.com>
}