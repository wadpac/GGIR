\name{g.loadlog}
\alias{g.loadlog}
\title{
  Load and clean sleeplog information
}
\description{
  Loads sleeplog from a csv input file and applies sanity checks
  before storing the output in a dataframe
}
\usage{
  g.loadlog(loglocation = c(), coln1 = c(), colid = c(),
            sleeplogsep = ",", meta.sleep.folder = c(),
            desiredtz = "", sleepwindowType = c())
}
\arguments{
  \item{loglocation}{
    Location of the spreadsheet (csv) with sleep log information.
    See package vignette for explanation on expected format
  }
  \item{coln1}{
    Column number in the sleep log spreadsheet where the onset of the first
  night starts
  }
  \item{colid}{
      Column number in the sleep log spreadsheet in which the participant
  ID code is stored (default = 1)

  }
  \item{sleeplogsep}{
    Value used as sep argument for reading sleeplog csv file, usually "," or ";".
    This argument has been deprecated.
  }
  \item{meta.sleep.folder}{
    Path to part3 milestone data, only specify if sleeplog is in advanced format.
  }
  \item{desiredtz}{
    See \link{GGIR}
  }
  \item{sleepwindowType}{
    See \link{GGIR}
  }
}
\value{
  Data frame with sleeplog, which can be either in basic format or in advanced
  format. See GGIR package vignette for discussion of these two formats.
}
\examples{
\dontrun{
  sleeplog = g.loadlog(loglocation="C:/mysleeplog.csv",coln1=2,
  colid=1)
}
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
