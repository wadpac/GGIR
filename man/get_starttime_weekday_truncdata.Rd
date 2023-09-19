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
  lengths in an hour, and truncating the data accordingly.
}
\usage{
  get_starttime_weekday_truncdata(monc, 
  dformat, data, header, desiredtz, sf, i,
  datafile, ws2, starttime, wday, weekdays, wdayname, configtz = NULL)
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
  \item{i}{
    Integer index of passed on from \link{g.getmeta}
    to indicate what data block is being read.
  }
  \item{datafile}{
    See \link{g.getmeta}
  }
  \item{ws2}{
    Long epoch length
  }
  \item{starttime}{
    Once calculate it is remembered and fed into this function again,
    such that it does not have to be recalulated.
  }
  \item{wday}{
    Once calculate it is remembered and fed into this function again,
    such that it does not have to be recalulated.
  }
  \item{weekdays}{
    Once calculate it is remembered and fed into this function again,
    such that it does not have to be recalulated.
  }
  \item{wdayname}{
    Once calculate it is remembered and fed into this function again,
    such that it does not have to be recalulated.
  }
   \item{configtz}{
    See \link{g.getmeta}
  }
}
\keyword{internal}
\author{
Vincent T van Hees <v.vanhees@accelting.com>
}
