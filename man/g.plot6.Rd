\name{g.plot6}
\alias{g.plot6}
\title{
  Generate user-friendly visual report
}
\description{
  Function called by \link{g.shell.GGIR} to generate report. Not intended
  for direct use by user. do.enmo must be set to 'TRUE' for this function to work.
  Two plotting sections are generated. The first contains bar charts summarizing
  time spent in moderate or vigorous physical activity, total physical activity,
  duration monitor won, sleep duration and sleep efficiency for each day of recording.
  The second section shows ENMO and angle plots over 24hr periods with annotations
  listed for sleeponset time, wake time, sleep / wake detection during SPT window
  as well as unbouted annotations of light, moderate and vigorous physical activity.
  Detected non-wear periods are also shown.
}
\usage{
  g.plot6(metadatadir=c(),dofirstpage=FALSE, viewingwindow = 1,
  f0=c(),f1=c(),overwrite=FALSE,desiredtz = "Europe/London",
  threshold.lig=30,threshold.mod=100,threshold.vig=400)
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
  \item{threshold.lig}{
    User set threshold to differentiate between inactivity and light physical activity
  }
  \item{threshold.mod}{
    User set threshold to differentiate between light and moderate physical activity
  }
  \item{threshold.vig}{
    User set threshold to differentiate between moderate and vigorous physical activity
  }
}
\value{
No values, this function only generates a plot
}

\examples{
  \dontrun{
  # generate plots for the first 10 files:
  g.plot6(metadatadir="C:/output_mystudy/meta/basic",dofirstpage=TRUE,
  viewingwindow = 1,f0=1,f1=10,overwrite=FALSE,desiredtz = "Europe/London",
  threshold.lig=30, threshold.mod=100, threshold.vig=400)
  }
}
\author{
Vincent T van Hees <vincentvanhees@gmail.com>
Matthew R Patterson <mpatterson@shimmersensing.com>
}
