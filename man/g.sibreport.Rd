\name{g.sibreport}
\alias{g.sibreport}
\title{
  Generate sustiained inactivty bouts report
}
\description{
  Generate sustained inactivity bout report. Function not intended
  for direct use by package user
}
\usage{
  g.sibreport(ts, ID, epochlength, logs_diaries=c(), desiredtz="")
}
\arguments{
  \item{ts}{
    Data frame with time series as created inside function \link{g.part5}
  }
  \item{ID}{
   Recording identifier (character or numeric)
  }
  \item{epochlength}{
    Numeric to indicate epoch length in seconds in the ts object
  }
  \item{logs_diaries}{
  Object produced by \link{g.loadlog} function
  }
  \item{desiredtz}{
    See \link{g.getmeta}
  }
}
\value{
  Dataframe with one row per sustained inactivity bout and corresponding 
  properties stored in the data.frame columns.
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}