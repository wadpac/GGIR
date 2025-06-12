\name{g.part5.definedays}
\alias{g.part5.definedays}
\title{
  Fix missing night in part 4 output
}
\description{
  Not intended for direct use by GGIR users.
  Defines when day windows start and end as part of \link{g.part5}.
}
\usage{
  g.part5.definedays(nightsi, wi, indjump, nightsi_bu, 
                     epochSize, qqq_backup = c(), ts,
                     timewindowi, Nwindows, qwindow, ID = NULL,
                     dayborder = 0)
}
\arguments{
  \item{nightsi}{
    Vector with indices for the midnights
  }
  \item{wi}{
    Numeric to indicate window number
  }
  \item{indjump}{
    Number of indices to jump
  }
  \item{epochSize}{
    Numeric epoch size in seconds
  }
  \item{qqq_backup}{
    Backup of qqq object, which holds the start and end indices of a window
  }
  \item{ts}{
    Data.frame with time series
  }
  \item{timewindowi}{
    Timewindow definition either "MM" or "WW"
  }
  \item{Nwindows}{
    Number of windows in the data
  }
  \item{qwindow}{
    qwindow argument
  }
  \item{ID}{
    ID of participant
  }
  \item{dayborder}{
    dayborder argument
  }
}
\value{
  List of qqq and qqq_backup 
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
