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
loglocation= c(),excludefirstlast=FALSE,windowsizes=c(5,900,3600),
boutcriter.mvpa=0.8,boutcriter.in=0.9,boutcriter.lig=0.8,
storefolderstructure=FALSE,threshold.lig = c(40),threshold.mod = c(100),
threshold.vig = c(400),timewindow=c("MM","WW"),boutdur.mvpa = c(1,5,10),
boutdur.in = c(10,20,30),boutdur.lig = c(1,5,10),winhr = 5,
M5L5res = 10,overwrite=FALSE,desiredtz="Europe/London",bout.metric=4)

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
  \item{winhr}{
   see \link{g.getmeta}
  }
}
\value{
 The function provides no values, it only ensures that other functions are called
 and that their output is stored in .RData files.
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