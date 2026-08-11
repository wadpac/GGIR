\name{visualReport}
\alias{visualReport}
\title{
  Generate visualisation of time series produced by part 5.
}
\description{
  Function called by \link{GGIR}. Not intended
  for direct use by user
}
\usage{
  visualReport(metadatadir = c(), 
              f0 = c(), f1 = c(), 
              verbose = TRUE,
              part6_threshold_combi = NULL, GGIRversion = NULL,
              params_sleep = NULL,
              params_output = NULL,
              params_general = NULL)
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
  \item{verbose}{
    See \link{GGIR}
  }
  \item{part6_threshold_combi}{
    See \link{GGIR}
  }
  \item{GGIRversion}{
    Character with GGIR version number
  }
  \item{params_sleep}{
    See \link{GGIR}
  }
  \item{params_output}{
    See \link{GGIR}
  }
  \item{params_general}{
    See \link{GGIR}
  }
}
\value{
  No values, this function only generates a plot
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}