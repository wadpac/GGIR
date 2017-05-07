\name{g.plot5}
\alias{g.plot5}
\title{
  Generate user-friendly visual report
}
\description{
  Function called by \link{g.shell.GGIR} to generate report. Not intended
  for direct use by user
}
\usage{
  g.plot5(metadatadir=c(),dofirstpage=FALSE, viewingwindow = 1,
  f0=c(),f1=c(),overwrite=FALSE,desiredtz = "Europe/London")	
}
\arguments{
  \item{metadatadir}{
    See \link{g.part2}
  }
  \item{dofirstpage}{
    Boolean to indicate whether a first page with historgrams summarizing the whole
    measurement should be added
  }
  \item{viewingwindow}{
    See \link{g.shell.GGIR}
  }
  \item{f0}{
    See \link{g.shell.GGIR}
  }
  \item{f1}{
    See \link{g.shell.GGIR}
  }
  \item{overwrite}{
    See \link{g.shell.GGIR}
  }
  \item{desiredtz}{
    See \link{g.getmeta}
  }
}
\value{
No values, this function only generates a plot
}

\examples{
  \dontrun{
  # generate plots for the first 10 files:
  g.plot5(metadatadir="C:/output_mystudy/meta/basic",dofirstpage=TRUE,
  viewingwindow = 1,f0=1,f1=10,overwrite=FALSE,desiredtz = "Europe/London")	
  }
}
\author{
Vincent T van Hees <vincentvanhees@gmail.com>
}
