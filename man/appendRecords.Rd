\name{appendRecords}
\alias{appendRecords}
\title{
  Append GGIR milestone data from neighbouring or overlapping recordings
}
\description{
  Append GGIR part 1 milestone data format from neighbouring or overlapping recording.
  When recordings overlap we use data from newest recordings. When time gap is larger
  than 2 days the recordings are not appended. Not intended for direct use by the user.
}
\usage{
   appendRecords(metadatadir, desiredtz = "", idloc = 1, maxRecordingInterval = NULL)
}
\arguments{
  \item{metadatadir}{
    See \link{g.part2}
  }
  \item{desiredtz}{
    See \link{GGIR}
  }
  \item{idloc}{
    See \link{GGIR}
  }
  \item{maxRecordingInterval}{
    See \link{GGIR}
  }
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}