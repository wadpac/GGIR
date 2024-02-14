\name{g.IVIS}
\alias{g.IVIS}
\title{
  Calculates IV and IS
}
\description{
  To extract interdaily stability and interdaily variability as originally proposed by
  van Someren.
}
\usage{
  g.IVIS(Xi, epochSize = 60, threshold = NULL)
}
\arguments{
  \item{Xi}{
    Vector with acceleration values, e.g. ENMO metric.
  }
  \item{epochSize}{
    Epoch size of the values in Xi expressed in seconds.
  }
  \item{threshold}{
    Acceleration threshold to distinguish inactive from active
  }
}
\value{
  \item{InterdailyStability}{}
  \item{IntradailyVariability}{}
}
\examples{
  Xi = abs(rnorm(n = 10000,mean = 0.2))
  IVISvariables = g.IVIS(Xi=Xi)
}
\keyword{internal}
\author{
  Ian Meneghel Danilevicz <ian.meneghel-danilevicz@inserm.fr>
  Vincent T van Hees <v.vanhees@accelting.com>
}
\references{
  \itemize{
    \item Eus J. W. Van Someren, Dick F. Swaab, Christopher C. Colenda, 
    Wayne Cohen, W. Vaughn McCall & Peter B. Rosenquist. Bright Light Therapy: 
    Improved Sensitivity to Its Effects on Rest-Activity Rhythms in Alzheimer 
    Patients by Application of Nonparametric Methods  Chronobiology
    International. 1999. Volume 16, issue 4.
  }
}
