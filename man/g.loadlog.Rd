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
  g.loadlog(loglocation=c(),coln1=c(),colid=c(),nnights=c(),
    sleeplogidnum=TRUE, sleeplogsep=",", meta.sleep.folder = c(),
  desiredtz="")
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
  \item{nnights}{
    Number of nights for which sleep log information should be available.
  It assumes that this is constant within a study. If sleep log information
  is missing for certain nights then leave these blank
  
  }
  \item{sleeplogidnum}{
    Should the participant identifier as stored in the sleeplog be
    interpretted as a number (TRUE=default) or a character (FALSE)?
  }
  \item{sleeplogsep}{
    Value used as sep argument for reading sleeplog csv file, usually "," or ";".
  }
  \item{meta.sleep.folder}{
    Path to part3 milestone data, only specify if sleeplog is in advanced format.
  }
  \item{desiredtz}{
    See \link{g.part4}
  }
}
\value{
  Data frame with sleeplog, which can be either in basic format or in advanced 
  format. See GGIR package vignette for discussion of these two formats.
}
\examples{
\dontrun{
  sleeplog = g.loadlog(loglocation="C:/mysleeplog.csv",coln1=2,
  colid=1,nnights=5,sleeplogidnum=TRUE)
}
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}