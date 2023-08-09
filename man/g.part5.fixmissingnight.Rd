\name{g.part5.fixmissingnight}
\alias{g.part5.fixmissingnight}
\title{
  Fix missing night in part 4 output
}
\description{
  Not intended for direct use by GGIR users.
  If a night is missing in the part4 output then this function
  tries to fix as part of \link{g.part5}.
}
\usage{
  g.part5.fixmissingnight(summarysleep, sleeplog = c(), ID)
}
\arguments{
  \item{summarysleep}{
    Object produced by \link{g.part4}
  }
  \item{sleeplog}{
    Sleeplog object as passed on from \link{g.part5}
  }
  \item{ID}{
    ID of participant
  }
}
\value{
  Corrected summarysleep_tmp2 object.
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}