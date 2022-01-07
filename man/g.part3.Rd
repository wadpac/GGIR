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
g.part3(metadatadir=c(), f0, f1, myfun=c(), 
  params_sleep = c(), params_metrics = c(), params_output = c(), params_general = c(),
  ...)
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
  \item{myfun}{
    External function object to be applied to raw data.
    See details \link{applyExtFunction}.
  }
  \item{params_sleep}{
    List of parameters used for sleep analysis in GGIRpart3 and GGIRpart4:
    Boolean parameters to be included are: ignorenonwear (see \link{g.sib.sum}); 
    constrain2range (see \link{g.sib.det}); HASPT.ignore.invalid (see \link{HASPT});
    relyonguider, relyonsleeplog (see details); 
    Character parameters to be included are: HASPT.algo (see \link{HASPT});
    HASIB.algo (see \link{HASIB}); Sadeh_axis (see \link{g.sib.det});
    sleeplogsep (\link{g.loadlog}); sleepwindowType (see details); 
    nap_model (see \link{g.part5.classifyNaps})
    Numeric parameters to be included are:
    longitudinal_axis, anglethreshold, timethreshold (see \link{g.sib.det}),
    def.noc.sleep (see details), possible_nap_window, 
    possible_nap_dur (see \link{g.part5.classifyNaps}); colid, coln1,
    sleeplogidnum, nnights (see details).
  }
  \item{params_metrics}{
    List of parameters used for metrics extraction (GGIR part 1): see documentation \link{g.part1}.
  }
  \item{params_output}{
    List, see \link{g.part1}
  }
  \item{params_general}{
    List, see \link{g.part1}
  }
  \item{...}{
    To enable compatibility with older GGIR version, user can also provide individual params_sleep 
    parameters as separate argument, e.g. ignorenonwear = FALSE.
  }
}
\details{
  params_sleep parameters not documented elsewhere:
  \cr
  \itemize{
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
    \item \code{sleepwindowType}
      Character to indicate type of sleeplog, default "SPT".
      Set to "TimeInBed" if sleep log recorded time in bed to enable calculation
      of sleep latency and sleep efficiency.
    \item \code{nnights}  
      Number of nights for which sleep log information should be available. 
      It assumes that this is constant within a study. If sleep log information
      is missing for certain nights then leave these blank.
    \item \code{loglocation}  Location of the spreadsheet (csv) with sleep log
      information. The spreadsheet needs to have the following structure: 
      one column for participant ID, and then followed by alternatingly one
      column for onset time and one column for waking time. There can be multiple
      sleeplogs in the same spreadsheet. The first raw of the spreadsheet needs to
      be filled with column names, it does not matter what these column names are.
      Timestamps are to be stored without date as in hh:mm:ss. If onset corresponds
      to lights out or intention to fall asleep, then it is the end-users
      responsibility to account for this in the interpretation of the results.
      \item \code{colid} Column number in the sleep log spreadsheet in which
        the participant ID code is stored (default = 1)
      \item \code{coln1} Column number in the sleep log spreadsheet where 
        the onset of the first night starts
      \item \code{sleeplogidnum} Should the participant identifier as stored in
        the sleeplog be interpretted as a number (TRUE=default) or character (FALSE)?
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
