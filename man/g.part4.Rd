\name{g.part4}
\alias{g.part4}
\title{
  Labels detected sustained inactivity periods by g.part3 as either
  part of the Sleep Period Time window or not
}
\description{
  Combines output from \link{g.part3} and guider information to estimate
  sleep variables. See vignette paragraph "Sleep and full day
  time-use analysis in GGIR" for an elaborate descript of the sleep detection.
}
\usage{
 g.part4(datadir = c(), metadatadir = c(), f0 = f0, f1 = f1, params_sleep = c(), 
    params_metrics = c(),  params_cleaning = c(), params_output = c(),
                   params_general = c(), ...)
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
  \item{params_sleep}{
    List of parameters used for sleep analysis (GGIR part 3, 4, and 5): 
    see documentation \link{g.part3}.
  }
  \item{params_metrics}{
    List of parameters used for metrics extraction (GGIR part 1): 
    see documentation \link{g.part1}.
  }
  \item{params_cleaning}{
   See details in \link{g.shell.GGIR}.
  }
  \item{params_output}{
   See details in \link{g.shell.GGIR}.
  }
  \item{params_general}{
   See details in \link{g.shell.GGIR}.
  }
  \item{...}{
    To ensure compatibility with R scripts written for older GGIR versions,
    the user can also provide parameters listed in the params_ objects as direct argument.
  }
}
\value{
  The function does not produce values but generates an RData file
  in the milestone subfolder ms4.out which incudes a dataframe
  named \code{nightsummary}. This dataframe is used in g.report.part4 to create
  two reports one per night and one per person. See package vignette 
  paragraph "Output part 4" for description of all the variables.
}

\examples{
  \dontrun{
    metadatadir = "C:/myfolder/meta" # assumes that there is a subfolder in
    # metadatadir named 'ms3.out' containing the output from g.part3
    g.part4(metadatadir=metadatadir)
  }
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
\references{
  \itemize{
    \item van Hees VT, Sabia S, et al. (2018) AEstimating sleep parameters
    using an accelerometer without sleep diary, Scientific Reports.
    \item van Hees VT, Sabia S, et al. (2015) A novel, open access method
    to assess sleep duration using a wrist-worn accelerometer, PLoS ONE.
  }
}
