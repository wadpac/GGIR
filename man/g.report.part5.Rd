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
                          data_cleaning_file=c())
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
  \item{data_cleaning_file}{
  see \link{g.part4}
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


See package vignette paragraph "Sleep and full day time-use analysis 
in GGIR" and "Output part 5" for a more elaborative description of
the full day time-use and analysis and reporting.

}
\author{
  Vincent T van Hees <vincentvanhees@gmail.com>
}
