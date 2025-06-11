\name{g.part6}
\alias{g.part6}
\title{
  Perform temporal pattern analyses
}
\description{
  This function aims to facilitate time-pattern analysis building on the labelled time
  series derived in GGIR part 5
}
\usage{
g.part6(datadir = c(), metadatadir = c(), f0 = c(), f1 = c(),
                   params_general = c(), params_phyact = c(), params_247 = c(),
                   params_cleaning = c(), verbose = TRUE, ...)
}
\arguments{
  \item{datadir}{
    Directory where the accelerometer files are stored, e.g. 
    "C:/mydata", or list of accelerometer filenames and directories, e.g. 
    c("C:/mydata/myfile1.bin", "C:/mydata/myfile2.bin").
  }
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
  \item{params_general}{
   See details in \link{GGIR}.
  }
  \item{params_phyact}{
   See details in \link{GGIR}.
  }
  \item{params_247}{
   See details in \link{GGIR}.
  }
  \item{params_cleaning}{
   See details in \link{GGIR}.
  }
  \item{verbose}{
    See details in \link{GGIR}.
  }
  \item{...}{
    To ensure compatibility with R scripts written for older GGIR versions,
    the user can also provide parameters listed in the params_ objects as direct argument.
  }
}
\value{
  The function does not produce values but generates an RData file
  in the milestone subfolder ms6.out which incudes ... (TO BE COMPLETED).
  This dataframe is used in g.report.part6 to create
  reports. See package vignette
  paragraph (TO BE COMPLETED) for description of all the variables.
}
\examples{
  \dontrun{
    metadatadir = "C:/myfolder/meta"
    g.part6(metadatadir=metadatadir)
  }
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
