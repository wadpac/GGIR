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
                          includenightcrit=c(),includedaycrit=c(),
                          data_cleaning_file=c(),
                          includedaycrit.part5=2/3,
                          minimum_MM_length.part5=23,
                          week_weekend_aggregate.part5=FALSE,
                          LUX_day_segments=c())
}
\arguments{
  \item{metadatadir}{
    Directory that holds a folder 'meta' and inside this a folder 'basic'
    which contains the milestone data produced by \link{g.part1}. The folderstructure
    is normally created by \link{g.part1} and \link{g.shell.GGIR} will recognise what
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
  \item{includenightcrit}{
    Depricated as of version 2.0, not used anymore in part 5 report
  }
  \item{includedaycrit}{
    Depricated as of version 2.0, not used anymore in part 5 report
  }
  \item{data_cleaning_file}{
  see \link{g.part4}
  }
  \item{includedaycrit.part5}{
    Inclusion criteria for number of valid hours, either
    as expressed as a ratio of 1 or as the number of hours in a 24 hour day.
  }
  \item{minimum_MM_length.part5}{
    Minimum length in hours of a MM day to be included in the cleaned
    part 5 results.
  }
   \item{week_weekend_aggregate.part5}{
   Boolean to indicate whether week and weekend-days aggregates
   should be stored. This is turned off by default as it generates a 
   large number of extra columns in the output report.
   }
   \item{LUX_day_segments}{
   see \link{g.part5}
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
