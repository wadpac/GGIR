\name{g.analyse.avy}
\alias{g.analyse.avday}
\title{
  Function supports \link{g.analyse}. Not intended for direct use by user.
}
\description{
  Generatess average day analyses and fills corresponding output
  matrix, \link{g.analyse}.
}
\usage{
  g.analyse.avday(doquan, averageday, M, IMP, t_TWDI, quantiletype,
                   ws3, doiglevels, firstmidnighti, ws2, midnightsi,
                   params_247 = c(), qcheck = c(), acc.metric = c(),
                   params_phyact = NULL, ...)
}
\arguments{
  \item{doquan}{Boolean whether quantile analysis should be done}
  \item{averageday}{ As produced by \link{g.impute}}
  \item{M}{ As produced by \link{g.getmeta}}
  \item{IMP}{ As produced by \link{g.impute}}
  \item{t_TWDI}{ Same as qwindow as described in \link{g.analyse}}
  \item{quantiletype}{see \link{g.analyse}}
  \item{ws3}{ Epoch size in seconds}
  \item{doiglevels}{Boolean to indicate whether iglevels should be calculated}
  \item{firstmidnighti}{see \link{g.detecmidnight}}
  \item{ws2}{see \link{g.weardec}}
  \item{midnightsi}{see \link{g.detecmidnight}}
  \item{params_247}{
    See \link{GGIR}
  }
  \item{qcheck}{
    Vector with indicators of when data is valid (value=0) or invalid (value=1).
  }
  \item{acc.metric}{
    Character, see \link{GGIR}. Here, it is used to decided which acceleration metric
    to use for IVIS and cosinor analyses.
  }
  \item{params_phyact}{
    See \link{GGIR}
  }
  \item{...}{
   Any argument used in the previous version of g.analyse.avday, which will now
   be used to overrule the arguments specified with the parameter objects.
  }
}
\value{
  \item{\code{igfullr_names}}{Intensity gradient variable names}
  \item{\code{igfullr}}{Intensity gradient values}
  \item{\code{QUAN}}{Quantiles}
  \item{\code{qlevels_names}}{Quantile level names}
  \item{\code{ML5AD}}{}
  \item{\code{ML5AD_names}}{}
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}