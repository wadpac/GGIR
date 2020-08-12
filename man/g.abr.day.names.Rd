\name{g.abr.day.names}
\alias{g.abr.day.names}
\title{
  Abbreviates daynames to numbers, needed for report generation in
  \link{g.plot5}
}
\description{
  Abbreviates daynames Monday becomes MON and Sunday becomes SUN
}
\usage{
  g.abr.day.names(daynames)	
}
\arguments{
  \item{daynames}{
    Vector of daynames in character format
  }
}

\examples{
  daynames = c("Monday","Friday")
  daynames_converted = g.abr.day.names(daynames)
}
\author{
Vincent T van Hees <v.vanhees@accelting.com>
}
