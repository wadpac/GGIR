\name{convertEpochData}
\alias{convertEpochData}
\title{
  convert external Epoch data to GGIR part 1 milestone data format
}
\description{
  convert externally created Epoch data to GGIR part 1 milestone data format. Function not
  intended for direct use by user. The aim is to allow for using GGIR on top of
  extrnally derived epoch data. See argument dataFormat in the \link{GGIR} for details on
  how to use this functionality.
}
\usage{
   convertEpochData(datadir = c(), studyname = c(), outputdir = c(),
                            params_general = c())
}
\arguments{
  \item{datadir}{
    See \link{GGIR}
  }
  \item{studyname}{
    See \link{GGIR}
  }
  \item{outputdir}{
    See \link{GGIR}
  }
  \item{params_general}{
    Parameters object see \link{GGIR}
  }
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}