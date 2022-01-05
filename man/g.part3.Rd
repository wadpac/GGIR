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
g.part3(metadatadir=c(), f0, f1, acc.metric="ENMO", overwrite=FALSE, 
  desiredtz = "", do.part3.pdf = TRUE, do.parallel=TRUE, myfun=c(), maxNcores=c(),
  params_sleep = c(), params_metrics = c(), ...)
}
\arguments{
  \item{metadatadir}{
    Directory that holds a folder 'meta' and inside this a folder 'basic'
    which contains the milestone data produced by \link{g.part1}. The folderstructure
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
  \item{acc.metric}{
    Which one of the metrics do you want to consider to analyze L5.
    The metric of interest need to be calculated in M (see \link{g.part1})
  }
  \item{overwrite}{
    Overwrite previously generated milestone data by this function for
    this particular dataset? If FALSE then it will skip the previously
    processed files (default = FALSE).
  }
  \item{desiredtz}{
    See \link{g.getmeta}
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
  \item{params_sleep}{
    List of parameters used for sleep analysis:
    anglethreshold (character, see \link{g.sib.det}), timethreshold (character, see \link{g.sib.det}),
    ignorenonwear (character, see \link{g.sib.sum}), constrain2range (character, see \link{g.sib.det}),
    sensor.location (character, see \link{g.sib.det}),  HASPT.algo (character, see \link{HASPT}),
    HASIB.algo (character, see \link{HASIB}), Sadeh_axis (character, see \link{g.sib.det}),
    longitudinal_axis (integer, see \link{g.sib.det}), HASPT.ignore.invalid (boolean, see \link{HASPT}),
    do.visual (boolean, see details), outliers.only (boolean, see details), excludefirstlast (boolean, see details), 
    criterror (numeric, see details), includenightcrit (numeric, see details),
    relyonguider (boolean, see details), relyonsleeplog (boolean, see details), def.noc.sleep (vector, see details),
    excludefirst.part4 (boolean, see details), excludelast.part4 (boolean, see details), 
    sleeplogsep (character, \link{g.loadlog}), sleepwindowType (character, see details).
  }
  \item{params_metrics}{
    List of parameters used for metrics extraction (GGIR part 1): see documentation \link{g.part1}.
  }
  \item{...}{
    To enable compatibility with older GGIR version, user can also provide individual params_sleep parameters as separate argument, e.g. ignorenonwear = FALSE.
  }
}
\details{
  Explanation of sleep analysis parameters that are not documented elsewhere in the technical manual.
  \cr
  \itemize{
    \item \code{do.visual} If g.part4 is run with do.visual == TRUE then
      the function will generate a pdf with a visual representation of the
      overlap between the sleeplog entries and the accelerometer detections.
      This can be used to visualy verify that the sleeplog entries do
      not come with obvious mistakes.
    \item \code{outliers.only} Relevant for do.visual == TRUE. Outliers.only == FALSE will visualise
      all available nights in the data. Outliers.only == TRUE will visualise
      only for nights with a difference in onset or waking time
      larger than the variable of argument criterror.
    \item \code{excludefirstlast} If TRUE then the first and last night of the measurement are
      ignored for the sleep assessment.
    \item \code{criterror} Relevant for do.visual == TRUE and outliers.only == TRUE.
      criterror specifies the number of minimum number of hours difference
      between sleep log and  accelerometer estimate for the night to be
      included in the visualisation
    \item \code{includenightcrit} Minimum number of valid hours per night (24 hour window between noon and noon)
    \item \code{relyonguider} If TRUE then sleep onset and waking time are defined based on
      timestamps derived from the guider. If participants were instructed NOT to wear the accelerometer
      during waking hours then set to TRUE, in all other scenarios
      set to FALSE (default).
    \item \code{relyonsleeplog} Now replaced by relyonguider. Values provided to argument
      relyonsleeplog will be passed on to argument relyonguider to not
      preserve functionality of old R script.
    \item \code{def.noc.sleep} The time window during which sustained inactivity will be assumed
      to represent sleep, e.g. def.noc.sleep=c(21,9). This is only
      used if no sleep log entry is available. If def.noc.sleep is
      left blank 'def.noc.sleep=c()' then the 12 hour window centred
      at the least active 5 hours of the 24 hour period will be used
      instead. Here, L5 is hardcoded and will not change by changing
      argument winhr in function \link{g.part2}. If def.noc.sleep is filled
      with a single integer, e.g. def.noc.sleep=c(1) then the window
      will be detected with based on built in algorithms.
      See argument HASPT.algo from \link{HASPT} for specifying which of those
      algorithms to use.
    \item \code{excludefirst.part4} If TRUE then the first night of the measurement are
      ignored for the sleep assessment.
    \item \code{excludelast.part4}
      If TRUE then the last night of the measurement are
      ignored for the sleep assessment.
    \item \code{sleepwindowType}
      Character to indicate type of sleeplog, default "SPT".
      Set to "TimeInBed" if sleep log recorded time in bed to enable calculation
      of sleep latency and sleep efficiency.
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
