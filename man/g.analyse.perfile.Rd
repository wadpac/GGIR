\name{g.analyse.perfile}
\alias{g.analyse.perfile}
\title{
  Function supports \link{g.analyse}. Not intended for direct use by user.
}
\description{
  Generates recording specific analyses and fills corresponding
  output matrix, \link{g.analyse}.
}
\usage{
g.analyse.perfile(ID, fname, deviceSerialNumber,
  sensor.location, startt, I, LC2, LD, dcomplscore,
  LMp, LWp, C, lookat, AveAccAve24hr, 
  colnames_to_lookat, QUAN,  ML5AD,
  ML5AD_names, igfullr, igfullr_names,
  daysummary, ds_names, includedaycrit, strategy, hrs.del.start,
  hrs.del.end, maxdur, windowsizes, idloc, snloc, wdayname, doquan,
  qlevels_names, doiglevels, tooshort, InterdailyStability, 
  IntradailyVariability,
  IVIS_windowsize_minutes, qwindow, longitudinal_axis_id)
}

\arguments{
\item{ID}{Person Identification number, this can be numeric or character}
\item{fname}{see \link{g.analyse.perday}}
\item{deviceSerialNumber}{As produced by \link{g.extractheadervars}}
\item{sensor.location}{as produced by \link{g.extractheadervars}}
\item{startt}{First timestamp in metalong}
\item{I}{output \link{g.inspectfile}}
\item{LC2}{see \link{g.impute}}
\item{LD}{length data in minutes}
\item{dcomplscore}{see \link{g.impute}}
\item{LMp}{length measurement based on study protocol (minutes)}
\item{LWp}{length of sensor worn based on study protocol (minutes)}
\item{C}{output \link{g.calibrate}}
\item{lookat}{indices of metashort column to analyse}
\item{AveAccAve24hr}{Average acceleration in an average 24 hour cycle}
\item{colnames_to_lookat}{Names of columns to look at, corresponding 
to argurment lookat}
\item{QUAN}{Results quantile analysis on the average day produced by \link{g.analyse.avday}}
\item{ML5AD}{Results ML5 analyses on the average day produced by \link{g.analyse.avday}}
\item{ML5AD_names}{Columns names corresponding to ML5AD}
\item{igfullr}{Results intensity gradient (ig) analysis on
the average day produced by \link{g.analyse.avday}}
\item{igfullr_names}{Columns names corresponding to igfullr}
\item{daysummary}{object produced by \link{g.analyse.perday}}
\item{ds_names}{column names corresponding to daysummary}
\item{includedaycrit}{see \link{g.analyse}}
\item{strategy}{see \link{g.analyse}}
\item{hrs.del.start}{see \link{g.analyse}}
\item{hrs.del.end}{see \link{g.analyse}}
\item{maxdur}{see \link{g.analyse}}
\item{windowsizes}{see \link{g.getmeta}}
\item{idloc}{see \link{g.analyse}}
\item{snloc}{see \link{g.analyse}}
\item{wdayname}{character with weekdayname}
\item{doquan}{Boolean whether quantile analysis should be done}
\item{qlevels_names}{object produced by \link{g.analyse.avday}}
\item{doiglevels}{Boolean to indicate whether iglevels should be calculated}
\item{tooshort}{0 (file not too short) or 1 (file too short)}
\item{InterdailyStability}{see \link{g.IVIS}}
\item{IntradailyVariability}{see \link{g.IVIS}}
\item{IVIS_windowsize_minutes}{see \link{g.IVIS}}
\item{qwindow}{see \link{g.analyse}}
\item{longitudinal_axis_id}{Index of axis for which the angle correlates most 
  strongly across 24 hoursas calculated inside g.analyse. For hip worn 
  accelerometer this helps to check which axis was the veritcal axis. 
  The estimate may not be informative for other attachment locations.}

}
\value{
\item{\code{filesummary}}{summary for the file that was analysed}
\item{\code{daysummary}}{Summary per day for the file that was analysed}
}

\author{
Vincent T van Hees <v.vanhees@accelting.com>
}