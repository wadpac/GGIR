\name{g.part4}
\alias{g.part4}
\title{
 Labels detected sustained inactivity periods by g.part3 as either
 nocturnal sleep or daytime sustained inactivity
}
\description{
Loads output from g.part3 as stored in milestone data and sleep log
information (if available) and then uses these information sources
to define nocturnal sleep and daytime sustained inactivity.
}
\usage{
g.part4(datadir=c(),metadatadir=c(),f0=f0,f1=f1,idloc=1,
loglocation = c(),colid = 1,coln1 = 9,nnights = 7,
sleeplogidnum=FALSE,do.visual=FALSE,outliers.only = FALSE,
    excludefirstlast=FALSE,criterror = 1,includenightcrit=4,
    relyonsleeplog=FALSE,def.noc.sleep=c(),
    storefolderstructure=FALSE,overwrite=FALSE)
}
\arguments{
  \item{datadir}{
  Directory where the accelerometer files are stored or list
  of accelerometer filenames and directories
  }
   \item{metadatadir}{
  Directory that holds a folders 'meta' and inside this a folder
  'basic' which contains the milestone data produced by g.part1.
  The folderstructure is normally created by g.part3. When using
  g.part4 via g.shell.GGIR then g.shell.GGIR will automatically 
  recognise what the value of metadatadir is, so the user does not
  need to specify this.
  }
  \item{f0}{
  File index to start with (default = 1). Index refers to the
  filenames sorted in increasing order
  }
  \item{f1}{
   File index to finish with (defaults to number of files available)
  }
  \item{idloc}{
  If value = 1 (default) the code assumes that ID number is stored in
  the obvious header field. If value = 2 the code uses the
  character string preceding the character '_' in the filename as
  the ID number
  }
  \item{loglocation}{
  Location of the spreadsheet (csv) with sleep log information.
  The spreadsheet needs to have the following structure: one column
  for participant id, and then followed by alternatingly one column
  for onset time and one column for waking time. There can be multiple
  sleeplogs in the same spreadsheet. The first raw of the spreadsheet
  needs to be filled with column names, it does not matter what these
  column names are. Timestamps are to be
  stored without date as in hh:mm:ss. If onset corresponds to lights
  out or intention to fall asleep, then it is the end-users
  responsibility to account for this in the interpretation of the results.
  }
  \item{colid}{
  Column number in the sleep log spreadsheet in which the participant
  ID code is stored (default = 1)
  }
  \item{coln1}{
  Column number in the sleep log spreadsheet where the onset of the first
  night starts
  }
  \item{nnights}{
  Number of nights for which sleep log information should be available.
  It assumes that this is constant within a study. If sleep log information
  is missing for certain nights then leave these blank
  }
    \item{sleeplogidnum}{
  Should the participant identifier as stored in the sleeplog be
  interpretted as a number (TRUE=default) or a character (FALSE)?
  }
  \item{do.visual}{
  If g.part4 is run with do.visual == TRUE then
  the function will generate a pdf with a visual representation of the
  overlap between the sleeplog entries and the accelerometer detections.
  This can be used to visualy verify that the sleeplog entries do
  not come with obvious mistakes.
  }
   \item{outliers.only}{
  Relevant for do.visual == TRUE. Outliers.only == FALSE will visualise
  all available nights in the data. Outliers.only == TRUE will visualise
  only for nights with a difference in onset or waking time
  larger than the variable of argument criterror.
  }
  \item{excludefirstlast}{
  If TRUE then the first and last night of the measurement are
  ignored for the sleep assessment.
  }
  \item{criterror}{
  Relevant for do.visual == TRUE and outliers.only == TRUE.
  criterror specifies the number of minimum number of hours difference 
  between sleep log and  accelerometer estimate for the night to be
  included in the visualisation
  }
  \item{includenightcrit}{
  Minimum number of valid hours per night (24 hour window between noon and noon)
  }
  \item{relyonsleeplog}{
  If TRUE then sleep onset and waking time are defined based on
  timestamps derived from sleep log if FALSE (default) the sleep
  log is only used to guide the accelerometer-based detection.
  If participants were instructed NOT to wear the accelerometer
  during waking hours then set to TRUE, in all other scenarios
  set to FALSE (FALSE). 
  }
  \item{def.noc.sleep}{
  The time window during which sustained inactivity will be assumed
  to represent sleep, e.g. def.noc.sleep=c(21,9). This is only
  used if no sleep log entry is available. If def.noc.sleep is
  left blank then the 12 hour window centred at the least active
  5 hours of the 24 hour period will be used instead.
  }
  \item{storefolderstructure}{
  Store folder structure of the accelerometer data
  }
  \item{overwrite}{
    Overwrite previously generated milestone data by this function
    for this particular dataset. If FALSE then it will skip the
    previously processed files (default = FALSE).
}
}
\value{

The function does not produce values but generates an RData file
in the milestone subfolder ms4.out which incudes a dataframe
named \code{nightsummary} and comes with the following variables:\cr
\itemize{
\item \code{id} Participant id extracted from file
\item \code{night} Night number
\item \code{acc_onset} Detected onset of sleep expressed as hours 
since the previous midnight
\item \code{acc_wake} Detected waking time (after sleep period)
expressed as hours since the previous midnight
\item \code{acc_timeinbed} Difference between onset and waking time.
\item \code{acc_def} Definition of sustained inactivity by accelerometer
\item \code{sleeplog_onset} Sleep onset derived from sleep log
or specified by researcher if not sleep log is available
\item \code{sleeplog_wake} Waking time derived from sleep log or
specified by researcher if not sleep log is available
\item \code{sleeplog_timeinbed} Time in bed derived from 
sleep log or specified by researcher if not sleep log is available
\item \code{error_onset} Difference between sleep onset as
estimated by accelerometer and estimated by sleeplog/defined by researcher
\item \code{error_wake} Difference between waking time as estimated
by accelerometer and estimated by sleeplog/defined by researcher
\item \code{fraction_night_invalid} Fraction of the night for which
the data was invalid, e.g. monitor not worn or no accelerometer
measurement started/ended within the night
\item \code{acc_dur_noc} Total sleep duration, which equals the
accumulated nocturnal sustained inactivity bouts. This is not the same
as the time in bed, which only looks at time difference between falling
asleep and waking up.
\item \code{acc_dur_sibd} Accumulated sustained inactivty bouts
during the day. These are the periods we would label during the night
as sleep, but during the day they form a subclass of inactivity,
which may represent day time sleep or wakefulness while being
motionless for a sustained period of time
\item \code{acc_n_noc} Number of noturnal sleep periods
\item \code{acc_n_sibd} Number of sustained inactivity periods
during the day
\item \code{acc_onset_ts} acc_onset formatted as a timestamp
\item \code{acc_wake_ts} acc_wake formatted as a timestamp
\item \code{sleeplog_onset_ts} sleeplog_onset formatted as a timestamp
\item \code{sleeplog_wake_ts} sleeplog_wake formatted as a timestamp
\item \code{page} pdf page on which the visualisation can be found
\item \code{daysleeper} If 0 then the person is a nightsleeper
(sleep period did not overlap with noon) if value=1 then the person
is a daysleeper (sleep period did overlap with noon)
\item \code{weekday} Day of the week on which the night started
\item \code{calendardate} Calendar data which the night started
\item \code{filename} Name of the accelerometer file
\item \code{cleaningcode} 0: no problem; 1: sleeplog not available,
2: not enough valid accelerometer data, 3: no accelerometer data available,
4: there were no nights to be analysed for this person
\item \code{sleeplog_used} Whether a sleep log was used (TRUE/FALSE) 
\item \code{acc_available} Whether accelerometer data was available
(TRUE/FALSE). This dataframe is used in g.report.part4 to create
two reports one per night and one per person. 
}

Note that function g.shell.GGIR comes with the option for report
generation. In relation to function g.part4 it is important to mention
that these reports are effectively the variable names mentioned above
or derivatives. Please fine below extra clarification on a few 
of the variable names for which the meaning may not be obvious:\cr
\itemize{
\item \code{sleeplog_used} Whether a sleeplog was available (TRUE)
or not (FALSE)
\item \code{n_nights_acc} Number of nights of accelerometer data
\item \code{n_WE_nights_complete} Number of weekend nights complete
which means both accelerometer and sleeplog data
\item \code{n_WD_nights_complete} Number of weekday nights complete
which means both accelerometer and sleeplog data
\item \code{n_WEnights_daysleeper} Number of weekend nights on which
the person slept until after noon
\item \code{n_WDnights_daysleeper} Number of weekday nights on which
the person slept until after noon
\item \code{sleeplog_dur_AD_mn} Mean sleep duration according to sleeplog
accros all days
\item \code{sleeplog_dur_AD_sd} Standard deviation of sleep duration
according to sleeplog accros all days
\item \code{sleeplog_dur_WD_sd} Standard deviation of sleep duration
according to sleeplog accros weekdays
\item \code{sleeplog_dur_WE_sd} Standard deviation of sleep duration
according to sleeplog accros weekend days

}

}
\examples{
\dontrun{
metadatadir = "C:/myfolder/meta" # assumes that there is a subfolder in
# metadatadir named 'ms3.out' containing the output from g.part3
g.part4(metadatadir=metadatadir)
}
}
\author{
  Vincent T van Hees <vincentvanhees@gmail.com>
}
\references{
\itemize{
\item van Hees VT, Sabia S, et al. (2015) A novel, open access method
to assess sleep duration using a wrist-worn accelerometer, PLoS ONE, 
accepted for publication October 2015
}
}