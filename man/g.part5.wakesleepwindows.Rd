\name{g.part5.wakesleepwindows}
\alias{g.part5.wakesleepwindows}
\title{
  Label wake and sleepperiod window
}
\description{
  Not intended for direct use by GGIR users.
  Label wake and sleepperiod window as part of \link{g.part5}.
}
\usage{
  g.part5.wakesleepwindows(ts, summarysleep_tmp2, desiredtz,
  nightsi, sleeplog, ws3new, Nts, ID, Nepochsinhour)
}
\arguments{

  \item{ts}{
    data.frame with time series
  }
  \item{summarysleep_tmp2}{
    cleaned output from part 4
  }
  \item{desiredtz}{
  }
  \item{nightsi}{
  }
  \item{sleeplog}{
  }
  \item{ws3new}{
  }
  \item{Nts}{
  }
  \item{ID}{
  }
  \item{Nepochsinhour}{
  }

}
\value{
  Object ts
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}