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
                              save_ms5raw_format = "csv",
                              save_ms5raw_without_invalid = TRUE,
                              DaCleanFile = NULL,
                              includedaycrit.part5 = 2/3,
                              ID = NULL, sep_reports = ",",
                              params_247 = NULL)
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
    Path to the file where the output should be stored
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
  \item{sep_reports}{
    Character (default = ","). Value used as sep argument in \link[utils]{write.csv}
    for writing csv files with the GGIR reports.
  }
  \item{params_247}{
    See \link{GGIR}
  }
}
\value{
  Function does not provide output, it only prepare data for saving
  and saves it to a file.
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}