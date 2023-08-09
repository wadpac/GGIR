\name{g.part5.wakesleepwindows}
\alias{g.part5.wakesleepwindows}
\title{
  Label wake and sleepperiod window
}
\description{
  Not intended for direct use by GGIR users.
  Label wake and sleepperiod window as part of \link{g.part5}.
}
\usage{
  g.part5.wakesleepwindows(ts, part4_output, desiredtz,
                    nightsi, sleeplog, epochSize,ID, Nepochsinhour)
}
\arguments{
  \item{ts}{
    data.frame with time series
  }
  \item{summarysleep_tmp2}{
    cleaned output from part 4
  }
  \item{desiredtz}{
    \link{GGIR}
  }
  \item{nightsi}{
    vector with indices for the midnights
  }
  \item{sleeplog}{
    Data.frame with sleeplog information as loaded by \link{g.loadlog}
  }
  \item{epochSize}{
    Short epochsize in seconds
  }
  \item{ID}{
    ID of the participant
  }
  \item{Nepochsinhour}{
    Number of epochs in an hour
  }
}
\value{
  Object ts
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}