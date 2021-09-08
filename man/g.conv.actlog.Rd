\name{g.conv.actlog}
\alias{g.conv.actlog}
\title{
  Function to read activity log and make it useful for the rest of GGIR.
}
\description{
 Function to read activity log and convert it into data.frame
 that has for each ID and date a different qwindow vector
  
}
\usage{
  g.conv.actlog(qwindow, qwindow_dateformat="\%d-\%m-\%Y")	
}
\arguments{
  \item{qwindow}{
    Path to csv files with activity log
  }
  \item{qwindow_dateformat}{
    Character specifying the date format used in the activity log.
  }
}
\value{
Data.frame with column ID, date and qwindow, where each
qwindow value is a qwindow vector
}
\author{
Vincent T van Hees <v.vanhees@accelting.com>
}
