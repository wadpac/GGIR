\name{is_this_a_dst_night}
\alias{is_this_a_dst_night}
\title{
 Check whether the night starting on a calendar date has DST.
}
\description{
Tests whether the night that follows the input calendar date is a
night with day saving time (DST) and on what hour the time moved.
}
\usage{
  is_this_a_dst_night(calendar_date=c(),tz="Europe/London")
}
\arguments{
  \item{calendar_date}{
    Character in the format dd/mm/yyyy 
  }
  \item{tz}{
    Time zone in "Europe/London" format.
  }
}
\value{
  \item{dst_night_or_not}{If value=0 no DST, if value=1 time moved forward,
  if value=-1 time moved forward}
  \item{dsthour}{Either the double hour or the hour that was skipped, 
  this differs between countries}
}
  
\examples{
  test4dst = is_this_a_dst_night("23/03/2014",tz="Europe/London")
}
