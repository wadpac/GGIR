\name{g.report.part2}
\alias{g.report.part2}
\title{
Generate report from milestone data produced by \link{g.part2}
}
\description{
Creates report from milestone data produced by \link{g.part2}. Not intended
  for direct use by package user
}
\usage{
g.report.part2(metadatadir=c(), f0=c(), f1=c(), maxdur = 0, 
selectdaysfile=c(), store.long=FALSE)
}
\arguments{
  \item{metadatadir}{
  see \link{g.part2}
  }
  \item{f0}{
  see \link{g.part2}
  }
  \item{f1}{
  see \link{g.part2}
  }
  \item{maxdur}{
  see \link{g.part2}
  }
  \item{selectdaysfile}{
  see \link{g.part2}
  }
  \item{store.long}{
  Booelean to indicate whether output should stored in long
  format in addition to default wide format. Automatically turned
  to TRUE if using day segmentation with qwindow.
  }
}
\value{
Function does not produce data, but only writes reports
in csv format and visual reports in pdf format
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
