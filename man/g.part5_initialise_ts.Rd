\name{g.part5_initialise_ts}
\alias{g.part5_initialise_ts}
\title{
  Initialise time series data from for part 5
}
\description{
  Initialise time series dataframe ts, part of \link{g.part5}.
}
\usage{
  g.part5_initialise_ts(IMP, M, params_247, params_general, longitudinal_axis = c())
}
\arguments{
  \item{IMP}{
    Object derived from \link{g.part2}
  }
  \item{M}{
    Object derived from \link{g.part1}.
  }
  \item{params_247}{
    See \link{GGIR}
  }
  \item{params_general}{
    See \link{GGIR} 
  }
  \item{longitudinal_axis}{
    As passed on from g.part5, which could be specified by user or estimated
    from hip data in part 2. 
  }
}
\value{
  Data.frame ts
}
\keyword{internal}
