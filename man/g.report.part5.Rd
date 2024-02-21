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
  g.report.part5(metadatadir = c(), f0 = c(), f1 = c(), loglocation = c(),
                          params_cleaning = NULL,
                          LUX_day_segments = c(), params_output,
                          verbose = TRUE)
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
  \item{loglocation}{
  see \link{g.part4}
  }
  \item{params_cleaning}{
   See details in \link{GGIR}.
  }
  \item{LUX_day_segments}{
   see \link{g.part5}
  }
  \item{params_output}{
    Parameters object, see \link{GGIR}
  }
  \item{verbose}{
    See details in \link{GGIR}.
  }
}
\value{
  Function does not produce data, but only writes reports
  in csv format
  
  The following files are stored in the root of the results folder:
  part5_daysummary_*
  part5_personsummary_*
  
  The following files are stored in the folder results/QC:
  part5_daysummary_full_*
  
  See package vignette paragraph "Waking-waking or 24 hour time-use analysis"
  and "Output part 5" for a more elaborative description of
  the full day time-use and analysis and reporting.
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
