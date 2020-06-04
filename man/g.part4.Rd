\name{g.part4}
\alias{g.part4}
\title{
 Labels detected sustained inactivity periods by g.part3 as either
 part of the Sleep Period Time window or not
}
\description{
Combines output from g.part3 and guider information to estimate
sleep variables. See vignette paragraph "Sleep and full day
time-use analysis in GGIR" for an elaborate descript of the sleep detection.
}
\usage{
g.part4(datadir=c(),metadatadir=c(),f0=f0,f1=f1,idloc=1,
loglocation = c(),colid = 1,coln1 = 9,nnights = 7,
sleeplogidnum=FALSE,do.visual=FALSE,outliers.only = FALSE,
    excludefirstlast=FALSE,criterror = 1,includenightcrit=16,
    relyonguider=FALSE, relyonsleeplog=FALSE, def.noc.sleep=1,
    storefolderstructure=FALSE,overwrite=FALSE,
    desiredtz="",data_cleaning_file=c())
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
  for participant ID, and then followed by alternatingly one column
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
  \item{relyonguider}{
  If TRUE then sleep onset and waking time are defined based on
  timestamps derived from the guider. If participants were instructed NOT to wear the accelerometer
  during waking hours then set to TRUE, in all other scenarios
  set to FALSE (default).
  }
   \item{relyonsleeplog}{
    Now replaced by relyonguider. Values provided to argument
    relyonsleeplog will be passed on to argument relyonguider to not
    preserve functionality of old R script.
  }
  \item{def.noc.sleep}{
  The time window during which sustained inactivity will be assumed
  to represent sleep, e.g. def.noc.sleep=c(21,9). This is only
  used if no sleep log entry is available. If def.noc.sleep is
  left blank 'def.noc.sleep=c()' then the 12 hour window centred
  at the least active 5 hours of the 24 hour period will be used
  instead. Here, L5 is hardcoded and will not change by changing
  argument winhr in function \link{g.part2}. If def.noc.sleep is filled
  with a single integer, e.g. def.noc.sleep=c(1) then the window
  will be detected with the method as described in van Hees et al.
  2018 Scientific Reports.
  }
  \item{storefolderstructure}{
  Store folder structure of the accelerometer data
  }
  \item{overwrite}{
    Overwrite previously generated milestone data by this function
    for this particular dataset. If FALSE then it will skip the
    previously processed files (default = FALSE).
 }
 \item{desiredtz}{
    See \link{g.getmeta}
  }
  \item{data_cleaning_file}{
  Optional path to a csv file you create that holds four
  columns: ID, day_part5, relyonguider_part4, 
  and night_part4. ID should hold the participant ID.
  Columns day_part5 and night_part4 allow you to 
  specify which day(s) and night(s) need to be excluded from
  part 5 and 4, respectively. So, this will be done regardless
  of whether the rest of GGIR thinks those day(s)/night(s)
  are valid.
  Column relyonguider_part4 allows you to specify for
  which nights part 4 should fully rely on the guider. See
  also package vignette.
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
  Vincent T van Hees <vincentvanhees@gmail.com>
}
\references{
\itemize{
\item van Hees VT, Sabia S, et al. (2018) AEstimating sleep parameters
using an accelerometer without sleep diary, Scientific Reports.
\item van Hees VT, Sabia S, et al. (2015) A novel, open access method
to assess sleep duration using a wrist-worn accelerometer, PLoS ONE.
}
}
