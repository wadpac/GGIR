\name{checkMilestoneFolders}
\alias{checkMilestoneFolders}
\title{
  Checks for existence of folders to process
}
\description{
  Checks whether milestone folders exist, create them if needed,
  and check whether folders are not empty. Only done for part 1 to 5
  and not part 6, which is different and handled inside \link{g.part6}.
}
\usage{
  checkMilestoneFolders(metadatadir, partNumber)
}
\arguments{
  \item{metadatadir}{
    Character, path to root of outputfolder.
  }
  \item{partNumber}{
    Numeric, number from the set 2, 3, 4 or 5.
  }
}
\value{
  No value is produced
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
