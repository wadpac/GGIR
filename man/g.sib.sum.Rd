\name{g.sib.sum}
\alias{g.sib.sum}
\title{
  sustiained inactivty bouts detection
}
\description{
  Detects sustiained inactivty bouts. Function not intended
  for direct use by package user
}
\usage{
  g.sib.sum(SLE,M,ignorenonwear=TRUE,desiredtz="")
}
\arguments{
  \item{SLE}{
    Output from \link{g.sib.det}
  }
  \item{M}{
    Object produced by \link{g.getmeta}
  }
  \item{ignorenonwear}{
    If TRUE then ignore detected monitor non-wear periods to avoid
  confusion between monitor non-wear time and sustained inactivity
  (default = TRUE)
  }
  \item{desiredtz}{
    See \link{g.part3}
  }
}
\value{
Dataframe with per night and per definition of sustained inactivity bouts
the start and end time of each sustained inactivity bout
}
\author{
Vincent T van Hees <v.vanhees@accelting.com>
}