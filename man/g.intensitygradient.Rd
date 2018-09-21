\name{g.intensitygradient}
\alias{g.intensitygradient}
\title{
Calculate intensity gradient by Rowlands
}
\description{
Calculates the intensity gradient as described in Rowlands et al. 2018
}
\usage{
g.intensitygradient(x,y)
}
\arguments{
  \item{x}{
  Numeric vector of mid-points of the bins (mg)
  }
  \item{y}{
  Numeric vector of time spent in bins (minutes)
  }
}
\value{
 \item{y_intercept}{y-intercept of a linear regression line in log-log space}
 \item{gradient}{Beta coefficient of a linear regression line in log-log space}
 \item{rsquared}{R squared of x and y values in log-log space}
}
\author{
Vincent T van Hees <vincentvanhees@gmail.com>
}
\references{
Rowlands A, Edwardson CL, et al. (2018) Beyond Cut Points: Accelerometer Metrics 
that Capture the Physical Activity Profile. MSSE 50(6):1. 
doi:10.1249/MSS.0000000000001561
}
