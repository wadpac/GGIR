\name{detect_nonwear_clipping}
\alias{detect_nonwear_clipping}
\title{
Detect non-wear and clipping time in the raw accelerometer data
}
\description{
Detects periods in which the accelerometer has not been worn or the
accelerometer signal was stuck very close to the dynamic range limit
of the accelerometer.
}
\usage{
detect_nonwear_clipping(data = c(), windowsizes = c(5, 900, 3600), sf = 100,
                        clipthres = 7.5, sdcriter = 0.013, racriter = 0.05,
                        nonwear_approach = "2013",
                        params_rawdata = c())
}
\arguments{
  \item{data}{
  Matrix with raw accelerometer data for X, Y, and Z axes.
}
 \item{windowsizes}{
  Numeric vector of length three, with short, long epoch and window size in seconds.
}
 \item{sf}{
  Sample frequency in Hertz.
}
 \item{clipthres}{
  Threschold to detect clipping in _g_ units. Usually 0.5 _g_ below the 
  dynamic range of the accelerometer.
}
 \item{sdcriter}{
  Criteria to define non-wear time, defined as the estimated noise
  measured in the raw accelerometer data.
}
 \item{racriter}{
  Absolute criteria below which the absolute range of the accelerations
  should be to define non-wear time.
}
 \item{nonwear_approach}{
  Whether to use the traditional version of the non-wear detection algorithm
  (nonwear_approach = "2013", default) or the new version (nonwear_approach = "2023"). 
  The 2013 version would use the longsize window (windowsizes[3], one hour as default)
  to check the conditions for nonwear identification and would flag as nonwear the
  mediumsize window (windowsizes[2], 15 min as default) in the middle. The 2023 
  version differs in which it would flag as nonwear the full longsize window. For the 2013
  method the longsize window is centered in the centre of the mediumsize window, while in 
  the 2023 method the longsizewindow is aligned with its left edge to the left edge of the 
  mediumsize window.
 }
 \item{params_rawdata}{
  See details in \link{GGIR}.
}
}
\value{
  List containing the next numeric vectors: 
  \itemize{
    \item NWav (non-wear score), from 1 to 3 indicating the number of axes that met the non wear criteria. 
    \item CWav (clipping score), binary, 0-1 indicating non-clipping and clipping, respectively.
    \item nmin is the minimum numebr of windows in this block of data.
  }
  Each number in these vectors represent a long epoch duration (i.e., ws2, 900 seconds by default).
}
\examples{
\dontrun{
    detect_nonwear_clipping(data = data, windowsizes = c(900, 3600), sf = sf,
                            clipthres = clipthres, sdcriter = sdcriter, 
                            racriter = racriter, nonwear_approach = "old")
  }
}
\author{
Vincent T van Hees <v.vanhees@accelting.com>
Jairo Hidalgo Migueles <j.h.migueles@accelting.com>
}
\references{
\itemize{
    \item van Hees  et al. 2011, doi: 10.1371/journal.pone.0022922.
    \item van Hees et al. 2013, doi: 10.1371/journal.pone.0061691 (in supplementary material).
  }
}
