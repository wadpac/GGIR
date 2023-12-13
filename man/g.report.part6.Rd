\name{g.report.part6}
\alias{g.report.part6}
\title{
  Generate report from milestone data produced by \link{g.part6}
}
\description{
  Creates report from milestone data produced by \link{g.part6}. Not intended
  for direct use by package user
}
\usage{
  g.report.part6(metadatadir = c(), f0 = c(), f1 = c(),
                          params_cleaning = NULL,
                          verbose = TRUE, sep_reports = ",")
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
  \item{params_cleaning}{
    See details in \link{GGIR}.
  }
  \item{verbose}{
    See details in \link{GGIR}.
  }
  \item{sep_reports}{
    Character (default = ","). Value used as sep argument in write.csv for writing csv files containing the GGIR reports.
  }  
}
\value{
  Function does not produce data, but only writes reports
  in csv format
  
  The following files are stored in the root of the results folder:
  part6_summary.csv
  
  See package vignette "HouseHoldCoanalysis".
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
