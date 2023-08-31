\name{g.part5.addfirstwake}
\alias{g.part5.addfirstwake}
\title{
  Adds first wake if it is missing in part 4 output.
}
\description{
  Not intended for direct use by GGIR users.
  Adds first wake if it is missing in part 4 output
  as part of \link{g.part5}.
}
\usage{
  g.part5.addfirstwake(ts, summarysleep, nightsi, sleeplog,
  ID, Nepochsinhour, SPTE_end)
}
\arguments{
  \item{ts}{
    Data.frame object as passed on from \link{g.part5}
  }
  \item{summarysleep}{
    Data.frame object as passed on from \link{g.part5} with
    sleep summary information from \link{g.part4}.
  }
  \item{nightsi}{
    Vector with indices for the nights
  }
  \item{sleeplog}{
    Data.frame with all sleeplog information
  }
  \item{ID}{
    Participant ID
  }
  \item{Nepochsinhour}{
    Number of epochs in an hour
  }
  \item{SPTE_end}{
    Sleep period time end index
  }
}
\value{
  Data.frame ts updated with first wakeup time
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}