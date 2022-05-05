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
  g.IVIS(Xi, epochsizesecondsXi = 5, IVIS_epochsize_seconds = c(),
  IVIS_windowsize_minutes = 60, IVIS.activity.metric = 1)
}
\arguments{
  \item{Xi}{
  Vector with acceleration values, e.g. ENMO metric.
  }
  \item{epochsizesecondsXi}{
  Epoch size of the values in Xi expressed in seconds.
  }
  \item{IVIS_epochsize_seconds}{ 
   This argument has been depricated.
  }
  \item{IVIS_windowsize_minutes}{
    Window size of the Intradaily Variability (IV) and Interdaily
  Stability (IS) metrics in minutes, needs to be able to add up to 24 hours.
  }
  \item{IVIS.activity.metric}{
  Metric used for activity calculation.
  Value = 1, uses continuous scaled acceleration.
  Value = 2, tries to collapse acceleration into a binary score of rest
  versus active to try to similate the original approach.
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
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
\references{
\itemize{
  \item Eus J. W. Van Someren, Dick F. Swaab, Christopher C. Colenda, Wayne Cohen, W. Vaughn McCall & Peter B. Rosenquist.
  Bright Light Therapy: Improved Sensitivity to Its Effects on Rest-Activity Rhythms in Alzheimer Patients by Application of Nonparametric Methods/
  Chronobiology International. 1999. Volume 16, issue 4.
}
}
