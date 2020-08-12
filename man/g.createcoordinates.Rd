\name{g.createcoordinates}
\alias{g.createcoordinates}
\title{
  Create coordinates for \link{g.plot}
}
\description{
  Function creates the coordinates for the blocks \link{g.plot}
  Function not designed for direct use by package user.
}
\usage{
g.createcoordinates(r,timeline)
}
\arguments{
  \item{r}{
    Vector of zeros and ones reflecting the moments in time when there
    should be a block (1)
  }
  \item{timeline}{
    Vector of time indicators, this can be numbers or actual timestamps
    The length of timeline needs to match the length of argument r
  }
}
\value{
  List with two objects: x0 with all the coordinates correspoding to the
  start of each blocks on the timelines and x1 with all the coordinates
  corresponding to the end of each block on the timeline
}
\author{
  Vincent van Hees <v.vanhees@accelting.com>
}