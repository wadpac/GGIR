\name{g.part5.savetimeseries}
\alias{g.part5.savetimeseries}
\title{
  Saves part 5 time series to csv files
}
\description{
  Not intended for direct use by GGIR users.
  Saves part 5 time series to csv files as part of \link{g.part5}.
}
\usage{
  g.part5.savetimeseries(ts, LEVELS, desiredtz, rawlevels_fname,
                              DaCleanFile = NULL,
                              includedaycrit.part5 = 2/3,
                              ID = NULL, params_output,
                              params_247 = NULL,
                              filename = "",
                              timewindow = NULL)
}
\arguments{
  \item{ts}{
    Data.frame with time series
  }
  \item{LEVELS}{
    As produced as one of the objects in the output of \link{identify_levels}
  }
  \item{desiredtz}{
     See \link{GGIR}.
  }
  \item{rawlevels_fname}{
    Path to the file where the output (time series) should be stored
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
  \item{params_output}{
    Parameters object, see \link{GGIR}
  }
  \item{params_247}{
    See \link{GGIR}
  }
  \item{filename}{
    Character (default = "") indicating the name of the accelerometer data file 
    that was used as input. This name will be stored inside the time series output file.
  }
  \item{timewindow}{
    See \link{GGIR}
  }
}
\value{
  Function does not provide output, it only prepare data for saving
  and saves it to a file. For documention on columns see main vignette.
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
