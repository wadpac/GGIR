\name{HASIB}
\alias{HASIB}
\title{
  Heuristic algorithms for sustiained inactivty bouts detection
}
\description{
  Apply heuristic algorithms for sustiained inactivty bouts detection.
  Function not intended for direct use by package user
}
\usage{
  HASIB(HASIB.algo = "vanHees2015", timethreshold=c(), anglethreshold=c(), 
                 time=c(), anglez=c(), ws3=c(), zeroCrossingCount=c(), BrondCount = c())
}
\arguments{
  \item{HASIB.algo}{
    Character to indicator which sib algorithm should be used.
    Default value: "vanHees2015". Other options: "Sadeh1994", "Galland2012"
  }
  \item{anglethreshold}{
    See \link{g.sib.det}
  }
  \item{timethreshold}{
    See \link{g.sib.det}
  }
  \item{time}{
    Vector with time per short epoch
  }
  \item{anglez}{
    Vector with z-angle per short epoch
  }
  \item{ws3}{
    See \link{g.getmeta}
  }
  \item{zeroCrossingCount}{
    Vector with zero crossing counts per epoch as required for Sadeh algortihm
  }
  \item{BrondCount}{
    Vector with Brond counts per epoch to be used by the Sadeh algortihm
  }
}
\value{
  Vector with binary indicator of sustained inactivity bout, 1 is yes, 0 is no.
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
