\name{g.getbout}
\alias{g.getbout}
\title{
  function to calculate bouts from vector of binary classes
}
\description{
  To detect bouts of behaviour in time series. The function is used by \link{g.analyse}
}
\usage{
  g.getbout(x, boutduration, boutcriter = 0.8, ws3 = 5)
}
\arguments{
  \item{x}{vector of zeros and/or ones to be screened for bouts of ones
  }
  \item{boutduration}{duration of bout in epochs
  }
  \item{boutcriter}{ Minimum percentage of boutduration for which the epoch values
    are expected to meet the threshold criterium
  }
  \item{ws3}{epoch length in seconds, only needed for bout.metric =3, because
  it needs to measure how many epochs equal 1 minute breaks
}

}
\value{
  Vector with binary numbers indicator where bouts where detected
}
\examples{
  y = g.getbout(x=round(runif(1000, 0.4, 1)), boutduration = 120, boutcriter=0.9,
    ws3 = 5)
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
  Jairo Hidalgo Migueles
}