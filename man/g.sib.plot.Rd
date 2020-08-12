\name{g.sib.plot}
\alias{g.sib.plot}
\title{
  Create plot of sustained inactivity bouts
}
\description{
  Function create plot of sustained inactivity bouts for quality
  check purposes as part of \link{g.part3}. Not intended for direct use by package user
}
\usage{
  g.sib.plot(SLE, M, I, plottitle, nightsperpage=7, desiredtz="")
}
\arguments{
  \item{SLE}{
    Output from \link{g.sib.det}
  }
  \item{M}{
    Output from \link{g.getmeta}
  }
  \item{I}{
    Output from \link{g.inspectfile}
  }
  \item{plottitle}{
    Title to be used in the plot
  }
  \item{nightsperpage}{
     Number of nights to show per page
  }
  \item{desiredtz}{
    See \link{g.part3}
  }
  
}
\value{
Function has no output other than the plot
}
\author{
Vincent T van Hees <v.vanhees@accelting.com>
}