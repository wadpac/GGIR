\name{getStartEnd}
\alias{getStartEnd}
\title{
Generate start and end time of a day
}
\description{
Generate start and end time of a day when working with argument selectdaysfile
in \link{g.part1}. The user provides a date and a start hour
which is used to generate the timestamps of the start hour minutes 5 minutes and the
start hour plus 24 hours. Function not designed for direct use by package user.
}
\usage{
getStartEnd(d, startHour, outputFormat = "\%d/\%m/\%Y \%H:\%M:\%S",
  tz = "Europe/London")
}
\arguments{
  \item{d}{
  character with date (without time) format
  }
   \item{startHour}{
  Hour that analysis starts at
  }
  \item{outputFormat}{
  Characterstring indicating outputFormat
  }
  \item{tz}{
  Same as desiredtz in \link{g.part1}
  }
}
\value{
Data.frame with two columns: a start time five minutes before startHour on day d
  and an endtime 24 hours after startHour
}

\examples{
startandendtime = getStartEnd(d="20/5/2017", startHour=4)
}
\author{
  Joe Heywood <j.heywood@ucl.ac.uk>
}