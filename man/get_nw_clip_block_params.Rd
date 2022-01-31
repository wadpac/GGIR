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
    Noise level of acceleration signal in _g_-units, used when working ad-hoc .csv data formats
    using \link{read.myacc.csv}. The \link{read.myacc.csv} does not take rmc.noise as argument,
    but when interacting with \link{g.shell.GGIR} or \link{g.part1} rmc.noise is used.
    There, rmc.noise is taken from the params_rawdata object if not explicitly specified by user.
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
