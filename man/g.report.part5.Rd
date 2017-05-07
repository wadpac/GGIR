\name{g.report.part5}
\alias{g.report.part5}
\title{
Generate report from milestone data produced by \link{g.part5}
}
\description{
Creates report from milestone data produced by \link{g.part5}. Not intended
  for direct use by package user
}
\usage{
g.report.part5(metadatadir=c(),f0=c(),f1=c(),loglocation=c(),
                          includenightcrit=c(),includedaycrit=c())
}
\arguments{
  \item{metadatadir}{
  see \link{g.part5}
  }
  \item{f0}{
  see \link{g.part5}
  }
  \item{f1}{
  see \link{g.part5}
  }
  \item{loglocation}{
  see \link{g.part4}
  }
  \item{includenightcrit}{
  see \link{g.part4}
  }
  \item{includedaycrit}{
  see \link{g.part2}
  }
}
\value{
Function does not produce data, but only writes reports
in csv format
}
\author{
  Vincent T van Hees <vincentvanhees@gmail.com>
}
