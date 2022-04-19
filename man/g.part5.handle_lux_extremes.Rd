\name{g.part5.handle_lux_extremes}
\alias{g.part5.handle_lux_extremes}
\title{
  Check lux values for extremes and imputes or removes them
}
\description{
  Extreme values are imputed by mean of neightbours if they occur isolated or
  in a sequence of two, and removed if they occure in a sequence of 3 or longer.
}
\usage{
  g.part5.handle_lux_extremes(lux)
}
\arguments{
  \item{lux}{
  Vector with lux values
  }
}
\value{
  List of imputed lux values and a vector with matching length named
  correction_log indicating which timestamps where imputed (value=1),
  replaced by NA (value=2) or untouched (value=0).
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}