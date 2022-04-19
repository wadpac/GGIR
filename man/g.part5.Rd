\name{g.part5}
\alias{g.part5}
\title{
  Merge output from physical activity and sleep analysis into one report
}
\description{
  Function to merge the output from g.part2 and g.part4 into one report enhanced with
  profiling of sleep and physical activity stratified across intensity levels and
  based on bouted periods as well as non-bouted periods.
}
\usage{
g.part5(datadir = c(), metadatadir = c(), f0=c(), f1=c(),
                   params_sleep = c(), params_metrics = c(),
                   params_247 = c(), params_phyact = c(), 
                   params_cleaning = c(), params_output = c(),
                   params_general = c(), ...)
}
\arguments{
  \item{datadir}{
    Directory where the accelerometer files are stored or list of accelerometer
    filenames and directories
  }
  \item{metadatadir}{
    Directory that holds a folders 'meta' and inside this a folder 'basic' which
    contains the milestone data produced by g.part1. The folderstructure
    is normally created by g.part1 and g.shell.GGIR will recognise what the value
    of metadatadir is.
  }
  \item{f0}{
    File index to start with (default = 1). Index refers to the filenames sorted
    in increasing order
  }
  \item{f1}{
    File index to finish with (defaults to number of files available)
  }
  \item{params_sleep}{
    List of parameters used for sleep analysis (GGIR part 3, 4, and 5): see documentation \link{g.part3}.
  }
  \item{params_metrics}{
    List of parameters used for metrics extraction (GGIR part 1): see documentation \link{g.part1}.
  }
  \item{params_247}{
    List, see \link{g.part2}
  }
  \item{params_phyact}{
    List, see \link{g.part2}
  }
  \item{params_cleaning}{
    List, see \link{g.part1}
  }
  \item{params_output}{
    List, see \link{g.part2}
  }
  \item{params_general}{
    List, see \link{g.part1}
  }
  \item{...}{
    To ensure compatibility with R scripts written for older GGIR versions,
    the user can also provide parameters listed in the params_ objects as direct argument.
  }
}
\value{
  The function does not produce values but generates an RData file
  in the milestone subfolder ms5.out which incudes a dataframe
  named \code{output}. This dataframe is used in g.report.part5 to create
  two reports one per day and one per person. See package vignette
  paragraph "Output part 5" for description of all the variables.
}
\examples{
  \dontrun{
    metadatadir = "C:/myfolder/meta"
    g.part5(metadatadir=metadatadir)
  }
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
