\name{part6AlignIndividuals}
\alias{part6AlignIndividuals}
\title{part6AlignIndividuals}
\description{
  Align individual time series per household where households are identified
  by the character or number string between the first and second '-' in the filename.
}
\usage{
  part6AlignIndividuals(GGIR_ts_dir = NULL, outputdir = NULL,
                      path_ggirms = NULL, desiredtz = "", verbose = TRUE)
}
\arguments{
  \item{GGIR_ts_dir}{
    Character, path to time series directory in the GGIR output
  }
  \item{outputdir}{
    Directory where you would like to store the output
  }
  \item{path_ggirms}{
    path to GGIR created folder named meta, with the milestone data files
  }
  \item{desiredtz}{
    Character, specifying the timezone database name of the timezone the 
    data was collected in.
  }
  \item{verbose}{
    See details in \link{GGIR}.
  }
}
\value{
  no object is returned, only files are created in the output directory
}
