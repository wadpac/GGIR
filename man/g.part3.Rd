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
    See details
  }
  \item{params_metrics}{
    List, see \link{g.part1}.
  }
  \item{params_output}{
    List, see \link{g.part1}
  }
  \item{params_general}{
    List, see \link{g.part1}
  }
  \item{...}{
    To ensure compatibility with R scripts written for older GGIR versions,
    the user can also provide parameters listed in the params_ objects as direct argument.
  }
}
\details{
  GGIR comes with many processing parameters, which have been thematically grouped in
  parameter objects (R list). By running print(load_params()) you can
  see the default values of all the parameter objects. When g.part 3 is used via \link{g.shell.GGIR}
  you have the option to specifiy a configuration file, which will overrule the default
  parameter values. Further, as user you can set parameter values as input argument to both g.part3
  and \link{g.shell.GGIR}. Directly specified argument overrule the configuration file and default values.
  
  See the GGIR package vignette for a more elaborate overview of parameter objects and their usage across GGIR.
  
  The parameter objects used by GGIR part 3 (g.part3) that are no already discussed in
  \link{g.part1} or \link{g.part2} are:
  
  \subsection{params_sleep}{
    A list of parameters used to configure the sleep analysis as performend in
    GGIR part 3 and 4.
    \describe{
      \item{relyonguider}{Boolean. If TRUE then sleep onset and waking time are defined based on
        timestamps derived from the guider. If participants were instructed NOT to wear the accelerometer
        during waking hours then set to TRUE, in all other scenarios
        set to FALSE (default).}
      \item{relyonsleeplog}{Do not use, now replaced by argument relyonguider.
        Values provided to argument relyonsleeplog will be passed on to 
        argument relyonguider to not preserve functionality of old R scripts.}
      \item{def.noc.sleep}{Numeric. The time window during which sustained
        inactivity will be assumed to represent sleep, e.g. def.noc.sleep=c(21,9).
        This is only used if no sleep log entry is available. If def.noc.sleep is
        left blank 'def.noc.sleep=c()' then the 12 hour window centred
        at the least active 5 hours of the 24 hour period will be used
        instead. Here, L5 is hardcoded and will not change by changing
        argument winhr in function \link{g.part2}. If def.noc.sleep is filled
        with a single integer, e.g. def.noc.sleep=c(1) then the window
        will be detected with based on built in algorithms.
        See argument HASPT.algo from \link{HASPT} for specifying which of those
        algorithms to use.}
      \item{sleepwindowType}{Character to indicate type of sleeplog, default "SPT".
        Set to "TimeInBed" if sleep log recorded time in bed to enable calculation
        of sleep latency and sleep efficiency.}
      \item{nnights}{Number of nights for which sleep log information should be available. 
        It assumes that this is constant within a study. If sleep log information
        is missing for certain nights then leave these blank.}
      \item{loglocation}{Character. Path to csv file with sleep log
        information. See package vignette for how to format this file.}
      \item{colid}{Numeric. Column number in the sleep log spreadsheet in which
          the participant ID code is stored (default = 1)}
      \item{coln1}{Numeric. Column number in the sleep log spreadsheet where 
          the onset of the first night starts}
      \item{sleeplogidnum}{Boolean. Should the participant identifier as stored in
          the sleeplog be interpretted as a number (TRUE=default) or character (FALSE)?}
      \item{ignorenonwear}{Boolean, see \link{g.sib.sum}}
      \item{constrain2range}{Boolean,  Whether or not to constrain the range of
        threshold used in the diary free sleep period time window detection}
      \item{HASPT.ignore.invalid}{Boolean, see \link{HASPT}}
      \item{HASPT.algo}{Character, character to indicate what heuristic algorithm
        to use for detecting the SPT window, see \link{HASPT}}
      \item{HASIB.algo}{Character, character to indicate what heuristic algorithm to use 
        for detecting the sustained inactivity bouts (SIB), see \link{HASIB}}
      \item{Sadeh_axis}{Character, character to indicate which axis to use for
        the Sadeh1994 algorithm, and  other algortihms that related on count-based
        Actigraphy such as Galland2012.}
      \item{sleeplogsep}{Character, \link{g.loadlog}}
      \item{nap_model}{Character, see \link{g.part5.classifyNaps}}
      \item{longitudinal_axis}{Integer to indicate which axis is the longitudinal axis. 
        If not provided function will estimate longitudinal axis. Only used when
        sensor.location="hip" or HASPT.algo="HorAngle".} 
      \item{anglethreshold}{Numeric, Angle threshold (degrees) for sustained 
        inactivity periods detection, default = 5}
      \item{timethreshold}{Numeric, time threshold (minutes) for sustained
        inactivity periods detection, default = 5. This can be specified
        as multiple thresholds, each of which will be implemented. 
        For example, timethreshold = c(5,10)}
      \item{possible_nap_window}{Numeric, see \link{g.part5.classifyNaps}}
      \item{possible_nap_dur}{Numeric, see \link{g.part5.classifyNaps}}
    }
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
