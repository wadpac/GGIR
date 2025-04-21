\name{splitRecords}
\alias{splitRecords}
\title{
  Append GGIR milestone data from neighbouring or overlapping recordings
}
\description{
  Splits GGIR part 1 milestone data format files based on 
  params_general parameters \code{recording_split_times}, 
  \code{recording_split_timeformat}, and \code{recording_split_times}.
}
\usage{
   splitRecords(metadatadir, params_general = NULL)
}
\arguments{
  \item{metadatadir}{
    See \link{g.part2}
  }
  \item{params_general}{
    See \link{GGIR}
  }
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}