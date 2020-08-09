\name{g.metric}
\alias{g.metric}
\title{
  Extract metrics from acceleration signals
}
\description{
  Function to extract metrics from acceleration signal. Not intended
  for direct use by package user
}
\usage{
  g.metric(data,n=c(),sf,ii,TW=c(),lb=c(),hb=c(),gravity = 1)
}
\arguments{
  \item{data}{
    Three column matrix with x, y, and z acceleration data
  }
  \item{n}{
    filter order, only needed if a metric is selected
    that involves a frequency filter
  }
  \item{sf}{
     sample frequency
  }
  \item{ii}{
    Integer to indicate which metric should be derived
  }
  \item{TW}{
    Time window size in samples used if the metric 
    involves a time window
  }
  \item{lb}{
    Cut-off frequency corresponding to the lower boundary
    of frequency filter
  }
  \item{hb}{
    Cut-off frequency corresponding to the higher boundary
    of frequency filter
  }
  \item{gravity}{
  Size of gravity, default = 1
    
  }
}
\value{
Vector of metric values at the same time resolution as the input data
}

\examples{
  Gx = runif(n=10000,min=0,max=2)
  Gy = runif(n=10000,min=1,max=3)
  Gz = runif(n=10000,min=0,max=2)
  data = cbind(Gx, Gy, Gz)
  EuclideanNorm = g.metric(data,sf=40,ii=3,gravity = 1)	
}

\author{
Vincent T van Hees <v.vanhees@accelting.com>
}