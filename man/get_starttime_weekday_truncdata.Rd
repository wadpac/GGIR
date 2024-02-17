\name{get_starttime_weekday_truncdata }
\alias{get_starttime_weekday_truncdata}
\title{
  Get starttime (adjusted), weekday, and adjust data accordingly.
}
\description{
  Function not intended for direct use by user.
  Used inside \link{g.getmeta} as an intermediate step between
  loading the raw data and calibrating it. This step includes extracting
  the starttime and adjusting it to nearest integer number of long epoch window
  lengths in an hour, truncating the data accordingly, and extracting the 
  corresponding weekday.
}
\usage{
  get_starttime_weekday_truncdata(monc, 
  dformat, data, header, desiredtz, sf,
  datafile, ws2, configtz = NULL)
}
\arguments{
  \item{monc}{
    See \link{g.inspectfile}
  }
  \item{dformat}{
    See \link{g.dotorcomma}
  }
  \item{data}{
    Data part of \link{g.readaccfile} output
  }
  \item{header}{
    Header part of \link{g.readaccfile} output
  }
  \item{desiredtz}{
    See \link{g.getmeta}
  }
  \item{sf}{
    Numeric, sample frequency in Hertz
  }
  \item{datafile}{
    See \link{g.getmeta}
  }
  \item{ws2}{
    Long epoch length
  }
   \item{configtz}{
    See \link{g.getmeta}
  }
}
\keyword{internal}
\author{
Vincent T van Hees <v.vanhees@accelting.com>
}
