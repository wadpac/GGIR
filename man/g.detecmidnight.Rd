\name{g.detecmidnight}
\alias{g.detecmidnight}
\title{
  Detect all midnights in a time series
}
\description{
  Detect all midnights in a time series
}
\usage{
  g.detecmidnight(time,desiredtz, dayborder)
}
\arguments{
  \item{time}{
    Vector of timestamps, either in iso8601 or in POSIX format
  }
  \item{desiredtz}{
    See \link{g.part2}
  }
    \item{dayborder}{
  see \link{g.analyse}
  }
  
}
\value{
Output of the function is list containing the following objects:\cr
\itemize{
\item firstmidnight = timestamp of first midnight
\item firstmidnighti = index of first midnight
\item lastmidnight = timestamp of last midnight
\item lastmidnighti = index of last midnight
\item midnights = timestamps of midnights
\item midnightsi = indeces of midnights
}
}
\author{
Vincent T van Hees <v.vanhees@accelting.com>
}