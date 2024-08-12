\name{get_nw_clip_block_params}
\alias{get_nw_clip_block_params}
\title{
  Set monitor brand specific parameters
}
\description{
  Set monitor brand specific thresholds for non-wear detection, clipping
  etection, and blocksizes to be loaded.
  Not designed for direct use by user.
}
\usage{
  get_nw_clip_block_params(monc, dformat, deviceSerialNumber = "", sf,
                            params_rawdata)
}
\arguments{
  \item{monc}{
    See \link{g.inspectfile}
  }
  \item{dformat}{
    See \link{g.dotorcomma}
  }
  \item{deviceSerialNumber}{
    As produced by \link{g.extractheadervars}
  }
  \item{sf}{
    Numeric, sample frequency in Hertz
  }
  \item{params_rawdata}{
    See \link{GGIR}
  }
}
\keyword{internal}
\author{
Vincent T van Hees <v.vanhees@accelting.com>
}
