\name{getFirstTimestamp}
\alias{getFirstTimestamp}
\title{
  Extract first timestamp from GENEActiv file
}
\description{
  Extract first timestamp from GENEActiv file, only used
  when using the selectdaysfile argument. Function not designed for
  direct use by package user.
}
\usage{
getFirstTimestamp(f, p1)
}
\arguments{
  \item{f}{
    GENEActiv filename
  }
  \item{p1}{
    First value of timestamps object
  }
}
\value{
  POSIX object withstarttime
}
\author{
  Joe Heywood <j.heywood@ucl.ac.uk>
}