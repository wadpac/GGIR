\name{extract_nightsi}
\alias{extract_nightsi}
\title{
  Extract time series and indices of midnights.
}
\description{
  Function to extract indices for midnights in time series.
}
\usage{
extract_nightsi(ts, dayborder, desiredtz, tail_expansion_log)
}
\arguments{
  \item{ts}{
    Data frame containing time series as generated in \link{g.part5} with
    \link{g.part5_initialise_ts}.
  }
  \item{dayborder}{
   See details in \link{GGIR}.
  }
  \item{desiredtz}{
   See details in \link{GGIR}.
  }
  \item{tail_expansion_log}{
   See details in \link{GGIR}.
  }
}
\value{
  List with midnight indices and time series objects.
}

\author{
  Vincent T van Hees <v.vanhees@accelting.com>
  Jairo H Migueles <jairo@jhmigueles.com>
}
