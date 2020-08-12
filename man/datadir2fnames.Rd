\name{datadir2fnames}
\alias{datadir2fnames}
\title{
Generates vector of file names out of datadir input
argument
}
\description{
Uses input argument datadir from \link{g.part1} and
the output from \link{isfilelist} to generate vector of filenames
}
\usage{
datadir2fnames(datadir,filelist)
}
\arguments{
  \item{datadir}{
  See \link{g.part1}
  }
   \item{filelist}{
  Produced by \link{isfilelist}
  }
}
\value{
Character vector of filenames
}

\examples{
\dontrun{
datadir2fnames(datadir = "C:/mydatafolder",filelist=TRUE)
}
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}