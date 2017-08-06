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
g.part5(datadir=c(),metadatadir=c(),f0=c(),f1=c(),strategy=1,maxdur=7,
hrs.del.start=0,hrs.del.end =0,
loglocation= c(),excludefirstlast=FALSE,windowsizes=c(5,900,3600), acc.metric="ENMO",
boutcriter.mvpa=0.8,boutcriter.in=0.9,boutcriter.lig=0.8,
storefolderstructure=FALSE,threshold.lig = c(40),threshold.mod = c(100),
threshold.vig = c(400),timewindow=c("MM","WW"),boutdur.mvpa = c(1,5,10),
boutdur.in = c(10,20,30),boutdur.lig = c(1,5,10),winhr = 5,
M5L5res = 10,overwrite=FALSE,desiredtz="Europe/London",bout.metric=4,dayborder=0)

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
 \item{strategy}{
  how to deal with knowledge about study protocol. value = 1 means select data
    based on \code{hrs.del.start}, \code{hrs.del.end}, and \code{maxdur}.
    Value = 2 makes that only the data between the first midnight and the last
    midnight is used for imputation, see \link{g.impute}
  }
 \item{maxdur}{
  how many DAYS after start of experiment did experiment
    definitely stop? (set to zero if unknown = default), see \link{g.impute}
  }
 \item{hrs.del.start}{
  how many HOURS after start of experiment did wearing of monitor start?, 
    see \link{g.impute}
  }
 \item{hrs.del.end}{
  how many HOURS before the end of the experiment did wearing of monitor
    definitely end?, see \link{g.impute}
  }
 \item{loglocation}{
  Location of the spreadsheet (csv) with sleep log information. The spreadsheet
  needs to have the following structure: one column for participant id, and then
  followed by alternatingly one column for onset time and one column for waking
  time. Timestamps are to be stored without date as in 18:20:00. If onset corresponds
  to lights out or intention to fall asleep, then it is the end-users responsibility
  to account for this in the interpretation of the results.
  }
 \item{excludefirstlast}{
    If TRUE then the first and last night of the measurement are ignored for the
  sleep assessment.
  }
 \item{windowsizes}{
  see \link{g.getmeta}
  }
 \item{acc.metric}{
  Which one of the metrics do you want to consider to describe behaviour. The metric of interest need to be calculated in M (see \link{g.part1})
  }
 \item{boutcriter.mvpa}{
  A number between 0 and 1 and defines what fraction of a bout needs to be above
  the mvpathreshold
  }
 \item{boutcriter.in}{
  A number between 0 and 1 and defines what fraction of a bout needs to be below
  the light threshold
  }
 \item{boutcriter.lig}{
  A number between 0 and 1 and defines what fraction of a bout needs to be between
  the light and moderage threshold
  }
 \item{storefolderstructure}{
  Store folder structure of the accelerometer data
  }
 \item{threshold.lig}{
   Threshold for light physical activity to separate inactivity from light. Value
   can be one number or an array of multiple numbers, e.g. threshold.lig =c(30,40).
   If multiple numbers are entered then analysis will be repliced for each
   combination of threshold values. Threshold is applied to the first metric in the
   milestone data, so if you have only specified do.ENMO == TRUE then it will be
   applied to ENMO.
  }
 \item{threshold.mod}{
   Threshold for moderate physical activity to separate light from moderate. Value
   can be one number or an array of multiple numbers, e.g. threshold.mod =c(100,110).
   If multiple numbers are entered then analysis will be repliced for each
   ombination of threshold values. Threshold is applied to the first metric in the
   milestone data, so if you have only specified do.ENMO == TRUE then it will be
   applied to ENMO.
   }
 \item{threshold.vig}{
  Threshold for vigorous physical activity to separate moderate from vigorous. Value
   can be one number or an array of multiple numbers, e.g. threshold.mod =c(400,500).
   If multiple numbers are entered then analysis will be repliced for each
   combination of threshold values. Threshold is applied to the first metric in the
   milestone data, so if you have only specified do.ENMO == TRUE then it will be
   applied to ENMO.
  }
 \item{timewindow}{
  Timewindow over which summary statistics are derived. Value can be "MM" (midnight
  to midnight), "WW" (waking time to waking time), or both c("MM","WW").
  }
 \item{boutdur.mvpa}{
  Durations of mvpa bouts in minutes to be extracted. The default values is
  c(1,5,10) and will start with the identification of 10 minute bouts, followed by
  5 minute bouts in the rest of the data, and followed by
  1 minute bouts in the rest of the data.
  }
 \item{boutdur.in}{
  Durations of inactivty bouts in minutes to be extracted. Inactivity bouts are
  detected in the segments of the data which were not labelled as sleep or MVPA
  bouts. The default duration values is c(10,20,30), this will start with the 
  identification of 30 minute bouts, followed by 20 minute bouts in the rest of
  the data, and followed by 10 minute bouts in the rest of the data.
  }
 \item{boutdur.lig}{
  Durations of light activty bouts in minutes to be extracted. Light activity
  bouts are detected in the segments of the data which were not labelled as sleep,
  MVPA, or inactivity bouts. The default duration values is c(1,5,10), this will
  start with the identification of 10 minute bouts, followed by 5 minute bouts
  in the rest of the data, and followed by 1 minute bouts in the rest of the data.  
  }
  \item{M5L5res}{
  resoltion of L5 and M5 analysis in minutes (default: 10 minutes)
  }
 \item{overwrite}{
  Overwrite previously generated milestone data by this function for this
    particular dataset. If FALSE then it will skip the previously processed files
    (default = FALSE).
  }
  \item{desiredtz}{
  see \link{g.getmeta}
  }
  \item{bout.metric}{
   See documnetion in \link{g.getbout} and 
  }
  \item{dayborder}{
   Hour at which days start and end (default = 0), value = 4 would mean 4am 
  }
  \item{winhr}{
   see \link{g.getmeta}
  }
}
\value{
 The function provides no values, it only ensures that other functions are called
 and that their output is stored in .RData files. See details.
}
\details{
This function writes all its output to an RData file
The value \code{output} is a dataframe and comes with a large range of variables
which hopefully are sufficiently intuitive or are already explained elsewhere in the
package tutorial. When g.part5 is called from \link{g.shell.GGIR} with argument 
do.report = 5 then the output of g.part5 is conventiently stored in a csv spreadsheet.
Therefore, you may not want/need to work with part5 directly.
\cr
\cr
Explaination of general terminology in the output of g.part5:\cr
\cr
\itemize{
\item acc_onset = onset of sleep according to accelerometer (+ diary) method expressed
in hours since the midnight in the night preceding the night of interest, e.g. 26 is 2am.
\item acc_wake = waking up time according to accelerometer (+ diary) method expressed
in hours since the midnight in the night preceding the night of interest, e.g. 38 is 8am
\item acc_onset_ts = onset of sleep according to accelerometer (+ diary) method expressed
as a timestamp hours:minutes:seconds
\item daysleep, if 0 then the person woke up before noon, if 1 then the person woke
up after noon
\item cleaningcode, 0: no problem; 1: sleeplog not available, 2: not enough valid
accelerometer data, 3: no accelerometer data available, 4: there were no nights
to be analysed for this person
\item window_length_in_hours, this relates to the definition of a day either from 
waking up till waking up the next day or from midnight to midnight
\item dur = duration
\item acc = (average) acceleration
\item nightwak = night waking
\item SIB=sustained inactivity bouts, are the periods of time during which the 
accelerometer does not rotate at all for at least 5 or 10 minutes. This could be
daytime sleep or the monitor not being worn for a very short period of time.
\item OIN = other inactivity
\item Nblock = number of blocks
\item D10 = bout lengths with 10 minutes and longer
\item T120 = threshold of 120 mg and higher
\item _pla = plain average across available days
\item _wei = weighted average across available days where weekend days always
weighted 2/5 relative to the contribution of week days
\item If there is no D in the variable name like in LIG50_120 then it refers to
unbouted time spent between those thresholds
\item dur_LIGB_D10T50_120 = the time spent in bouts of light activity of at
least 10 minutes
\item WW in filename refers to analyses based on the timewindow from waking
to waking up
\item MM in filename refers to analyses done on windows between midnight and midnight
\item INB are bouts of time during which acceleration is below an acceleration
threshold for at least X percent of the time.
\item If boutdur.mvpa holds two bout duration thresholds like 1 and 10 minutes when
it is set to c(1,10) then you will get D1T100 and D10T100. In this case D1T100
effectively means bouts between 1 and 9.99 minutes, while D10T100 refers to bouts of
at least 10 minutes. 
\item TMOD and TVIG are total moderate and total vigorous PA with all bouted and
unbouted time contributing. Just MOD and VIG are only the unbouted time spent in
those categories.
\item nightwak = night waking time
\item SIB= sustained inactivity bouts, can be part of inactivity bouts 
(INB variables), SIB are the sustained inactivity bouts that are not part of 
the inactivity bouts. This is why I have a TSIB and a SIB variable
\item TSIB = total of all sustained inactivity bouts (regardless of whether
they contributed or not to inactivity bouts (INB).
\item TOIN = Total other inactivity
\item TIN = Total inactivity = TOIN + SIB = Total other inactivty + Sustained
inactivity
\item Levels of behaviour from least active to most active are: Sleep or SIB,
Other inactivity, Light, Moderate, Vigorous
\item Additionally they a grouped in: MVPA bouts (Moderate or Vigorous),
Light bouts (just light), Inactivity bouts (Other inactivity or SIB),
And the non-bouted time spent in each of the five categories, And 
also as the total of each category: TSIB, TOIN, TLIG, TMOD, TVIG
\item dur_day_min_pla, dur_night_min_pla, and dur_nightandday_min_pla: duration of
a day and night time
}
Example variable explanations: dur_day_OIN30_min is the time spent in 
minutes in other inactiivty during the day with a threshold of
30 mg not part of inactivity bouts, dur_day_MOD100_400_min is the time spent
in moderate activity defined between 100 and 400mg but not part of an MVPA bout,
dur_MVPA_D1T100_min is time spent in MVPA bouts defined as 100 mg or higher
and lasting at least 1 minute and with no upper boundary if there is
no other variable that starts with dur_MVPA_D,
the existence of dur_INB_D10T30_min and dur_INB_D30T30_min indicates that
dur_INB_D10T30_min corresponds to inactivity bouts lasting between 10 and 30
minutes and are defined by the threshold 30mg.\cr
\cr
Motivation for default bout criteria for inactivity 0.9: Ssomewhat arbitrary 
decision, but the idea is that if you allow for bouts of 30 minutes it would
not make sense to allow for breaks of 20 percent (6 minutes!) this is why I
used a more stringent criteria for the highest category. Please note that
you can change these criteria via arguments boutcriter.mvpa, boutcriter.in,
and boutcriter.lig
}
\examples{
\dontrun{
metadatadir = "C:/myfolder/meta"
g.part5(metadatadir=metadatadir)
}
}
\author{
  Vincent T van Hees <vincentvanhees@gmail.com>
}
\references{
\itemize{
\item This function has not been described in a scientific journal yet
}
}
