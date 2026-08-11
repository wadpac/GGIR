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
  g.report.part2(metadatadir = c(), f0 = c(), f1 = c(),
                  store.long = FALSE, params_output, myfun = c(), 
                  verbose = TRUE, desiredtz = "")
}
\arguments{
  \item{metadatadir}{
    Directory that holds a folder 'meta' and inside this a folder 'basic'
    which contains the milestone data produced by \link{g.part1}. The folderstructure
    is normally created by \link{g.part1} and \link{GGIR} will recognise what
    the value of metadatadir is.
  }
  \item{f0}{
    File index to start with (default = 1). Index refers to the filenames sorted
    in alphabetical order
  }
  \item{f1}{
    File index to finish with (defaults to number of files available, i.e., f1 = 0)
  }
  \item{store.long}{
    Booelean to indicate whether output should stored in long
    format in addition to default wide format. Automatically turned
    to TRUE if using day segmentation with qwindow.
  }
  \item{params_output}{
    Parameters object, see \link{GGIR}
  }
  \item{myfun}{
   (Optional) List as documented in \link{GGIR}. g.report.part2 uses this object
   to extract the name of the external function being used, such that this
   can be reused in the output filenames.
  }
  \item{verbose}{
    See details in \link{GGIR}.
  }
  \item{desiredtz}{
    See details in \link{GGIR}.
  }
}
\value{
Function does not produce data, but only writes reports
in csv format and visual reports in pdf format
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
