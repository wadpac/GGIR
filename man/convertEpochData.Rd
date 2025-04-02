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
   convertEpochData(datadir = c(), metadatadir = c(),
                    params_general = c(), f0 = c(), f1 = c(), verbose = TRUE)
}
\arguments{
  \item{datadir}{
    See \link{GGIR}
  }
  \item{metadatadir}{
    See \link{GGIR}
  }
  \item{params_general}{
    Parameters object see \link{GGIR}
  }
  \item{f0}{
    File index to start with (default = 1). Index refers to the filenames sorted
    in alphabetical order
  }
  \item{f1}{
    File index to finish with (defaults to number of files available, i.e., f1 = 0)
  }
  \item{verbose}{
    See \link{GGIR}
  }
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
