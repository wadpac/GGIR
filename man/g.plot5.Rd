\name{g.plot5}
\alias{g.plot5}
\title{
Generate user-friendly visual report. The first part of the report summarizes important daily metrics in bar plot format.
The second part of the report shows the raw data and annotations in 24-hr periods.
Angle-z is shown with sleep annotations during the SPT (sleep period time) window.
ENMO is shown with daytime inactivity and PA (physical activity) annotations in the lower
section of each 24-hr plot. The PA annotations are based on a 10 minute bout metric and
80% bout criteria. Moderate PA is a short window of time above threshold.mod that is part
of a 10 minute bout of MVPA. Vigorous PA is a short window of time above threshold.vig that
is part of a bout of MVPA. Light PA is a short window of time above threshold.lig that is
part of a bout of light PA.
}
\description{
  Function called by \link{g.shell.GGIR} to generate report. Not intended
  for direct use by user
}
\usage{
  g.plot5(metadatadir = c(), dofirstpage = FALSE, viewingwindow = 1,
  f0 = c(), f1 = c(), overwrite = FALSE, metric="ENMO",desiredtz = "Europe/London",
  threshold.lig = 30, threshold.mod = 100, threshold.vig = 400)
}
\arguments{
  \item{metadatadir}{
    Directory that holds a folder 'meta' and inside this a folder 'basic'
    which contains the milestone data produced by \link{g.part1}. The folderstructure
    is normally created by \link{g.part1} and \link{g.shell.GGIR} will recognise what
    the value of metadatadir is.
  }
  \item{dofirstpage}{
    Boolean to indicate whether a first page with historgrams summarizing the whole
    measurement should be added
  }
  \item{viewingwindow}{
    See \link{g.shell.GGIR}
  }
  \item{f0}{
    File index to start with (default = 1). Index refers to the filenames sorted
    in alphabetical order
  }
  \item{f1}{
    File index to finish with (defaults to number of files available, i.e., f1 = 0)
  }
  \item{overwrite}{
    See \link{g.shell.GGIR}
  }
  \item{metric}{
    Which one of the metrics do you want to consider to describe behaviour. The
    metric of interest need to be calculated in M (see \link{g.part1})
  }
  \item{desiredtz}{
    See \link{g.getmeta}
  }
  \item{threshold.lig}{
    See \link{g.part5}
  }
  \item{threshold.mod}{
    See \link{g.part5}
  }
  \item{threshold.vig}{
    See \link{g.part5}
  }
}
\value{
No values, this function only generates a plot
}

\examples{
  \dontrun{
  # generate plots for the first 10 files:
  g.plot5(metadatadir="C:/output_mystudy/meta/basic",dofirstpage=TRUE,
  viewingwindow = 1,f0=1,f1=10,overwrite=FALSE,desiredtz = "Europe/London",
  threshold.lig,threshold.mod,threshold.vig)
  }
}
\author{
Vincent T van Hees <v.vanhees@accelting.com>
Matthew R Patterson <mpatterson@shimmersensing.com>
}
