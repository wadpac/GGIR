\name{g.conv.actlog}
\alias{g.conv.actlog}
\title{
  Function to read activity log and convert it into data.frame that 
  has for each date a different qwindow vector.
}
\description{
  Downsamples a vector of numeric values at three time resolutions:
  1 seconds, ws3 seconds, and ws2 second. Function is not intended
  for direct interaction by package end user
  
}
\usage{
  g.conv.actlog(qwindow)	
}
\arguments{
  \item{qwindow}{
    Path to csv files with activity log
  }
}
\value{
Data.frame with column ID, date and qwindow, where each
qwindow value is a qwindow vector
}
\author{
Vincent T van Hees <vincentvanhees@gmail.com>
}
