\name{aggregate_to_epoch}
\alias{aggregate_to_epoch}
\title{
  Aggregates time series.
}
\description{
  Function to aggregate time series to different epoch lengths in \link{g.part5}.
}
\usage{
aggregate_to_epoch(ts, to, lightpeak_available, params_general)
}
\arguments{
  \item{ts}{
    Data frame containing time series as generated in \link{g.part5} with
    \link{g.part5_initialise_ts}.
  }
  \item{to}{
   Epoch length in seconds to aggregate the time series to.
  }
  \item{lightpeak_available}{
   Boolean to indicate if light columns should be added to the time series.
  }
  \item{params_general}{
   See details in \link{GGIR}.
  }
}
\value{
  The function returns a data frame with the time series aggregated
  in the defined epoch lenght.
}

\author{
  Vincent T van Hees <v.vanhees@accelting.com>
  Jairo H Migueles <jairo@jhmigueles.com>
}
