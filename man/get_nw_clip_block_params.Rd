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
  get_nw_clip_block_params(chunksize, dynrange, monc, rmc.noise=c(),
  sf, dformat, rmc.dynamic_range)
}
\arguments{
  \item{chunksize}{
    See \link{g.calibrate}
  }
  \item{dynrange}{
    See \link{g.getmeta}
  }
  \item{monc}{
    See \link{g.inspectfile}
  }
  \item{rmc.noise}{
    Optional, please see \link{read.myacc.csv}
  }
   \item{sf}{
    Numeric, sample frequency in Hertz
  }
  \item{dformat}{
    See \link{g.dotorcomma}
  }
  \item{rmc.dynamic_range}{
    Optional, please see \link{read.myacc.csv}
  }
  
}
\author{
Vincent T van Hees <v.vanhees@accelting.com>
}
