\name{g.sib.det}
\alias{g.sib.det}
\title{
  sustiained inactivty bouts detection
}
\description{
  Detects sustiained inactivty bouts. Function not intended
  for direct use by package user
}
\usage{
  g.sib.det(M,IMP,I,twd=c(-12,12),anglethreshold = 5,
                     timethreshold = c(5,10),desiredtz="Europe/London")
}
\arguments{
  \item{M}{
    Object produced by \link{g.getmeta}
  }
  \item{IMP}{
    Object produced by \link{g.impute}
  }
  \item{I}{
    Object produced by \link{g.inspectfile}
  }
  \item{twd}{
    Vector of length 2, indicating the time window to consider
    as hours relative to midnight.
  }
  \item{anglethreshold}{
    See \link{g.part3}
  }
  \item{timethreshold}{
    See \link{g.part3}
  }
  \item{desiredtz}{
  See \link{g.part3}
  }
}
\value{
\itemize{
\item output = Dataframe for every epoch a classification
\item detection.failed = Boolean whether detection failed
\item L5list = L5 for every day (defined from noon to noon)
}
}
\author{
Vincent T van Hees <vincentvanhees@gmail.com>
}