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
                          week_weekend_aggregate.part5 = FALSE,
                          LUX_day_segments = c(), 
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
  \item{loglocation}{
  see \link{g.part4}
  }
  \item{params_cleaning}{
   See details in \link{GGIR}.
  }
  \item{week_weekend_aggregate.part5}{
   Boolean to indicate whether week and weekend-days aggregates
   should be stored. This is turned off by default as it generates a 
   large number of extra columns in the output report.
  }
  \item{LUX_day_segments}{
   see \link{g.part5}
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
