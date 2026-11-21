\name{g.report.part6_dictionary}
\alias{g.report.part6_dictionary}
\title{
  Generate data dictionary for reports from milestone data produced by \link{g.part6}
}
\description{
  Creates a data dictionary with the definitions of the outcomes exported in the
  reports from milestone data produced by \link{g.part6}. Not intended
  for direct use by package user.
}
\usage{
g.report.part6_dictionary(metadatadir, params_output)
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

  Variables for which no definition could be derived are stored with an empty
  definition.
}
\value{
  Function does not produce data, but only writes data dictionaries for the
  reports in csv format

  The following files are stored in the folder results/variableDictionary:\cr
  part6_dictionary_summary.csv
}
\author{
  Jairo Hidalgo Migueles <jairo@jhmigueles.com>
  Vincent T van Hees <v.vanhees@accelting.com>
}
