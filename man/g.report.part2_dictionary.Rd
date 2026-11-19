\name{g.report.part2_dictionary}
\alias{g.report.part2_dictionary}
\title{
  Generate data dictionary for reports from milestone data produced by \link{g.part2}
}
\description{
  Creates a data dictionary with the definitions of the outcomes exported in the
  reports from milestone data produced by \link{g.part2}. Not intended
  for direct use by package user.
}
\usage{
g.report.part2_dictionary(metadatadir, params_output)
}
\arguments{
  \item{metadatadir}{
    Directory that holds a folder 'meta' and inside this a folder 'basic'
    which contains the milestone data produced by \link{g.part1}. The folderstructure
    is normally created by \link{g.part1} and \link{GGIR} will recognise what
    the value of metadatadir is.
  }
  \item{params_output}{
    Parameters object, see \link{GGIR}
  }
}
\details{
  Variable names are interpreted based on the naming conventions of the part 2
  reports, which allows the dictionary to describe variables that depend on the
  parameters used (e.g., acceleration metrics, day segments, \code{winhr},
  \code{qlevels}, \code{ilevels}, and \code{mvpathreshold}).
  Variables for which no definition could be derived are stored with an empty
  definition.
}
\value{
  Function does not produce data, but only writes data dictionaries for the
  reports in csv format

  The following files are stored in the folder results/variableDictionary:\cr
  part2_dictionary_summary.csv\cr
  part2_dictionary_daysummary.csv\cr
  part2_dictionary_daysummary_longformat.csv (only if day segments are used)\cr
  part2_dictionary_eventsummary.csv and part2_dictionary_dayeventsummary.csv
  (only if an external function with reporttype "event" is used)\cr
  part2_dictionary_typesummary.csv and part2_dictionary_daytypesummary.csv
  (only if an external function with reporttype "type" is used)\cr
  part2_dictionary_data_quality_report.csv
}
\author{
  Jairo Hidalgo Migueles <jairo@jhmigueles.com>
  Vincent T van Hees <v.vanhees@accelting.com>
}
