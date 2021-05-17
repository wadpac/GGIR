\name{g.part3}
\alias{g.part3}
\title{
 Detection of sustained inactivity periods as needed for sleep detection
 in g.part4.
}
\description{
Function called by g.shell.GGIR. It estimates the sustained inactivity
periods in each day, which are used as input for g.part4 which then labels
them as nocturnal sleep or day time sustained inactivity periods.
Typical users should work with function g.shell.GGIR only.
}
\usage{
g.part3(metadatadir=c(),f0,f1,anglethreshold = 5,timethreshold = 5,
  acc.metric="ENMO", ignorenonwear=TRUE, overwrite=FALSE,
  desiredtz="",constrain2range=TRUE,
  do.part3.pdf=TRUE, do.parallel=TRUE, myfun=c(), maxNcores=c())
}
\arguments{
  \item{metadatadir}{
  Directory that holds a folder 'meta' and inside this a folder 'basic'
  which contains the milestone data produced by g.part1. The folderstructure
  is normally created by g.part1 and g.shell.GGIR will recognise what
  the value of metadatadir is.
  }
  \item{f0}{
  File index to start with (default = 1). Index refers to the filenames
  sorted in increasing order
  }
  \item{f1}{
   File index to finish with (defaults to number of files available)
  }
  \item{anglethreshold}{
  Angle threshold (degrees) for sustained inactivity periods detection,
  default = 5
  }
  \item{timethreshold}{
  Time threshold (minutes) for sustained inactivity periods detection,
  default = 5. This can be specified as multiple thresholds, each of
  which will be implemented. For example, timethreshold = c(5,10)
  }
  \item{acc.metric}{
    Which one of the metrics do you want to consider to analyze L5.
    The metric of interest need to be calculated in M (see \link{g.part1})
  }
  \item{ignorenonwear}{
  If TRUE then ignore detected monitor non-wear periods to avoid
  confusion between monitor non-wear time and sustained inactivity
  (default = TRUE)
  }
  \item{overwrite}{
    Overwrite previously generated milestone data by this function for
    this particular dataset? If FALSE then it will skip the previously
    processed files (default = FALSE).
  }
  \item{desiredtz}{
    See \link{g.getmeta}
  }
   \item{constrain2range}{
   Whether or not to constrain the range of threshold used in the diary free
   Sleep period time window detection
  }
  \item{do.part3.pdf}{
   Whether to generate a pdf for part 3 (default is TRUE). Turning this off
   could speed up the processing.
  }
    \item{do.parallel}{
    Boolean whether to use multi-core processing (only works if at least 4 CPU cores are available.
  }
  \item{myfun}{
    External function object to be applied to raw data.
    See details \link{applyExtFunction}.
  }
  \item{maxNcores}{
    See function \link{g.part1}
  }
}
\value{
The function provides no values, it only ensures that other functions
are called and that their output is stored in .RData files.
\cr
\itemize{
\item \code{night} nightnumber
\item \code{definition} definition of sustained inactivity. For example,
T10A5 refers to 10 minute window and a 5 degree angle (see paper for
further explaination).
\item \code{start.time.day} timestamp when the day started
\item \code{nsib.periods} number of sustained inactivity bouts
\item \code{tot.sib.dur.hrs} total duration of all sustained inactivity bouts
\item \code{fraction.night.invalid} fraction of the night for which
accelerometer data was invalid, e.g. monitor not worn
\item \code{sib.period} number of sustained inactivity period
\item \code{sib.onset.time} onset time of sustained inactivity period
\item \code{sib.end.time} end time of sustained inactivity period
}
}
\examples{
\dontrun{
metadatadir = "C:/myfolder/meta" # assumes that there is a subfolder in
# metadatadir named 'basic' containing the output from g.part1
g.part3(metadatadir=metadatadir, anglethreshold=5,
timethreshold=5, overwrite=FALSE)
}
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
\references{
\itemize{
\item van Hees VT, Sabia S, et al. (2015) A novel, open access method to
assess sleep duration using a wrist-worn accelerometer, PLoS ONE, November 2015
\item van Hees VT, Sabia S, et al. (2018) Estimating sleep parameters
using an accelerometer without sleep diary. Scientific Reports.

}
}
