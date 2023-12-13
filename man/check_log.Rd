\name{check_log}
\alias{check_log}
\title{
  Function to revise format of user-provided logs
}
\description{
  Function to revise the format for missing values and dates in user-provided
  
}
\usage{
  check_log(log, dateformat, colid = 1, datecols = c(), 
            logPath, logtype)	
}
\arguments{
  \item{log}{
    Data frame with the log as read by \code{data.table::fread}.
  }
  \item{dateformat}{
    Character specifying the expected date format used in the log.
  }
  \item{colid}{
    Numeric with the column containing the file ID.
  }
  \item{datecols}{
    Numeric with the column/s containing the dates.
  }
  \item{logPath}{
    Character containing the full path to the activity log that is being checked.
  }
  \item{logtype}{
    Character which accepts "activity log" or "study dates log" at the moment.
    Only used for warning messages.
  }
}
\value{
  Data.frame containing the revised log.
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
  Jairo H Migueles <jairo@jhmigueles.com>
}
