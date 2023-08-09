\name{g.part5.addsib}
\alias{g.part5.addsib}
\title{
  Adds the sustained inactivity bout to the ts series.
}
\description{
  Not intended for direct use by GGIR users.
  Adds the sustained inactivity bout to the ts series
  as part of \link{g.part5}.
}
\usage{
  g.part5.addsib(ts, epochSize, part3_output, desiredtz,
              sibDefinition, nightsi)
}
\arguments{
  \item{ts}{
    Data.frame object as passed on from \link{g.part5}
  }
  \item{epochSize}{
    Short epoch size in seconds
  }
  \item{part3_output}{
    Segment of part 3 output relevant for current sleep definition
  }
  \item{desiredtz}{
    see \link{GGIR}
  }
  \item{sibDefinition}{
    Character to indicate definition of sib (sustained inactivity bout)
  }
  \item{nightsi}{
    Vector with indices for the midnights
  }
}
\value{
  Data.frame ts
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}