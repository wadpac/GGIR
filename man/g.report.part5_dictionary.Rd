\name{g.report.part5_dictionary}
\alias{g.report.part5_dictionary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Generate data dictionary for reports from milestone data produced by \link{g.part5}
}
\description{
  Creates a data dictionary with the definitions of the outcomes exported in the
  reports from milestone data produced by \link{g.part5}. Not intended
  for direct use by package user.
}
\usage{
g.report.part5_dictionary(metadatadir, params_output)
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
\value{
  Function does not produce data, but only writes data dictionaries for the
  reports in csv format
  
  The following files are stored in the folder results/variableDictionary:\cr
  part5_dictionary_daysummary.csv\cr
  part5_dictionary_personsummary.csv\cr
  part5_dictionary_extfuntype_<output>_daysummary.csv and
  part5_dictionary_extfuntype_<output>_personsummary.csv
  (only if an external function with reporttype "type" is used)
}

\author{
  Vincent T van Hees <v.vanhees@accelting.com>
  Jairo Hidalgo Migueles <jairo@jhmigueles.com>
}