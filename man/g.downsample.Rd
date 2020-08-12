\name{g.downsample}
\alias{g.downsample}
\title{
  Downsample a vector of numeric values at three time resolutions
}
\description{
  Downsamples a vector of numeric values at three time resolutions:
  1 seconds, ws3 seconds, and ws2 second. Function is not intended
  for direct interaction by package end user
  
}
\usage{
  g.downsample(sig,fs,ws3,ws2)	
}
\arguments{
  \item{sig}{
    Vector of numeric values
  }
  \item{fs}{
    Sample frequency
  }
  \item{ws3}{
    ws3 epoch size, e.g. 5 seconds
  }
  \item{ws2}{
    ws2 epoch size, e.g. 90 seconds
  }
}
\value{
List with three object: var1, var2, and var3
corresponding to downsample time series at
1 seconds, ws2 seconds, and ws3 seconds resoluton, respectively
}
\examples{
  sig = runif(n=10000,min=1,max=10)
 downsampled_sig = g.downsample(sig,fs=20,ws3=5,ws2=15)
}
\author{
Vincent T van Hees <v.vanhees@accelting.com>
}
