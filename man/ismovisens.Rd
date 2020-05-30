\name{ismovisens}
\alias{ismovisens}
\title{
Checks whether the files to process are collected with movisens accelerometers.
}
\description{
Checks whether the files in the datadir folder are files collected
with movisens accelerometers. Note that movisens data are stored in one folder per recording that includes multiple bin-files
(instead of one file per recording as usual in other accelerometer brands). Therefore, datadir indicates
the directory where all the recording folders are stored, then, GGIR reads the pertinent bin files from
every folder.}
\usage{
ismovisens(data)	
}
\arguments{
  \item{data}{
Full path to the recording folder (with the bin files inside) or the datadir (where all the recording folders are stored).
}
}
\value{
Boolean whether it is a movisens file (TRUE) or not (FALSE)
}
  
\examples{
\dontrun{
is.mv = ismovisens(data)
}
}
