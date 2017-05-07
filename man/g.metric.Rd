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
  g.metric(Gx,Gy,Gz,n=c(),sf,ii,TW=c(),lb=c(),hb=c(),gravity = 1)
}
\arguments{
  \item{Gx}{
    y acceleration signal
  }
  \item{Gy}{
    y acceleration signal
  }
  \item{Gz}{
    z acceleration signal
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
  EuclideanNorm = g.metric(Gx,Gy,Gz,sf=40,ii=3,gravity = 1)	
}

\author{
Vincent T van Hees <vincentvanhees@gmail.com>
}