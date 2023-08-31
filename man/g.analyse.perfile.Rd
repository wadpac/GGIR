\name{g.analyse.perfile}
\alias{g.analyse.perfile}
\title{
  Function supports \link{g.analyse}. Not intended for direct use by user.
}
\description{
  Generates recording specific analyses and fills corresponding
  output matrix, \link{g.analyse}.
}
\usage{
g.analyse.perfile(I, C, metrics_nav,
                 AveAccAve24hr, doquan, doiglevels, tooshort,
                 params_247, params_cleaning, params_general,
                 output_avday, output_perday,
                 dataqual_summary, file_summary)
}

\arguments{
  \item{I}{output \link{g.inspectfile}}
  \item{C}{output \link{g.calibrate}}
  \item{metrics_nav}{List with three objects to help navigate the acceleration metrics}
  \item{AveAccAve24hr}{Average acceleration in an average 24 hour cycle}
  \item{doquan}{Boolean whether quantile analysis should be done}
  \item{doiglevels}{Boolean to indicate whether iglevels should be calculated}
  \item{tooshort}{0 (file not too short) or 1 (file too short)}
  \item{params_247}{see \link{GGIR}}
  \item{params_cleaning}{see \link{GGIR}}
  \item{params_general}{see \link{GGIR}}
  \item{output_avday}{Output from g.analyse.avday}
  \item{output_perday}{Output from g.analyse.perday}
  \item{dataqual_summary}{Data.frame with data quality summary indicators
  produced in \link{g.analyse}}
}
\value{
  \item{\code{filesummary}}{summary for the file that was analysed}
  \item{\code{daysummary}}{Summary per day for the file that was analysed}
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}