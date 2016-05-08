\name{NEWS}
\title{News for Package \pkg{GGIR}}
\newcommand{\cpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\section{Changes in version 1.2-5 (release date: 8-5-2016)}{
\itemize{
  \item Accelerometer non-wear time is now also reported in the output of part 4
  \item Part 1 is now able to only process specific days of a measurement via argument 
  selectdaysfile. This is useful when measurement lasts for a week and the participant
  is instructed to only wear the accelerometer on one or two specific days.
  \item Argument mvpa.2014 and closedbout added to function g.analyse. The calculation of
  MVPA (moderate and vigorous physical activity) has been available since 2014.
  With hindsight this included a bug in the calculation which incorrectly allocated too
  many minutes to each bout. This has now been corrected, but the user has the option
  to continue using the old calculation.
}
}
\section{Changes in version 1.2-2 (release date: 7-1-2016)}{
\itemize{
  \item Bug fixed in the loading of data files with (very) large amounts of data
  \item Bug fixed in starttime allocation for measurements starting in the 15 minutes before midnight
}
}
\section{Changes in version 1.2-1 (release date: 9-12-2015)}{
\itemize{
  \item Literature reference for sleep detection updated
  \item Argument backup.cal.coef now with improved feedback if something goes wrong
  \item Report generation for part 4 much faster now
  \item Bug fixed in part 4 in assignment of dayname when a person sleeps during the day
  \item g.shell.GGIR now capable of handling minimal input argument specifications
  \item Console output from part 3 and 4 more compact now
}
}
\section{Changes in version 1.2-0 (release date: 27-10-2015)}{
\itemize{
  \item Package expanded with functions for detecting sleep (g.part3 and g.part4)
}
}
\section{Changes in version 1.1-5 (release date: 11-05-2015)}{
\itemize{
  \item Additional bugs fixs related to dealing with csv-format data from the Actigraph accelerometer brand
  \item g.part2 now also stores its output as milestone data just like g.part1. This to facilitate parallel processing of large  amounts of data files on clusters. \item The orginal report generation functionality in g.part2 has now been moved to shell function g.shell.GGIR because part3, 4 and 5 which are scheduled for later
  this year will combine milestone data from multiple analysis parts. It therefore, is more logical to control all report generation from the top level in the function hierarchy (g.shell.GGIR).
  \item g.part1 now comes with the option to provide backup calibration coefficients in case auto-calibration is unsuccessful. This function is primarily designed for studies with  short lasting experiments.
  \item g.part2 now has the option to export epoch values to a csv-file. Note that these same epoch values are also stored in the .RData milestone file from part2.
  The export option is mainly to ease access to epoch level data outside the R environment.
  \item g.shell.GGIR now offers the option to overwrite previously generated
  milestone data with variable 'overwrite'. The default setting (FALSE) is still to skip previously analysed files, which is intended to avoid having to do the same analyses twice after an interruption. However, overwriting previously generated milestone data could be useful when modifications are being made to the input arguments.
  \item g.shell.GGIR now offers the option to record the folderstructure in which
  an accelerometer file is located, especially useful for studies where accelerometer files are stored hierarchally in line with the study design.
}
}
\section{Changes in version 1.1-4 (release date: 06-11-2014)}{
\itemize{
  \item Additional bugs fixs related to recognising data format in Actigraph data
  \item Angle variables are now extracted based on 5 second rolling median as
  opposed to 501 sample rolling median
}
}
\section{Changes in version 1.1-3 (release date: 21-10-2014)}{
\itemize{
  \item Package expanded with functions: g.part1, g.part2, and g.shell.GGIR.
  These shell functions should help movement scientists
   to utilize the package without too much prior knowledge about R
  \item Additional bugs fixs related to recognising data format in Actigraph data
  \item Package expanded with axis angle metrics
  \item Package expanded with metric for replicating teLindert2013 counts, see
  \link{g.getmeta}
  \item Package expanded with metric ENMOa, see function tutorial g.getmeta
  }
}
\section{Changes in version 1.0-6 (release date: 1-9-2014)}{
\itemize{
  \item Bug fixed related to recognising date format in csv-file header from
  Actigraph accelerometer brand
  \item Literature reference added to function g.calibrate
  \item function g.getmeta expanded with argument 'chunksize'
  }
}
\section{Changes in version 1.0-4 (release date: 29-4-2014)}{
\itemize{
  \item Implemented functionality for csv-fromat data from GENEActiv and Actigraph.
  It seems to work for the test files I have, more testing may be necessary
  \item Cleaned up some of the NaN and NA output to aim for consistent annotation
  of missing data
  }
}
\section{Changes in version 1.0-3 (release date: 27-3-2014)}{
\itemize{
  \item Fixed bug in modified \link{g.analyse} in version 1.0-2
  }
}
\section{Changes in version 1.0-2 (release date: 14-3-2014)}{
\itemize{
  \item Expanded \link{g.analyse} with estimates of time spent in moderate and
  vigorous activity (a construct popular among physical activity researchers)
  \item Re-named a number of variables in the output from \link{g.analyse} to be
  more friendly for re-use in stata or sas. The majority of variable names are now 
  shorter and do not include spaces, dots or commas
  }
}
\section{Changes in version 1.0-1 (release date: 29-1-2014)}{
\itemize{
  \item Fixed Linux-Windows sensitivty in \link{g.getmeta}. Certain damaged files 
  can only be read with mmap.load set to FALSE in package GENEAread. Function
  \link{g.getmeta} in GGIR catches this problem and turns mmap.load to FALSE if
  necessary. This catch worked  well under Linux, but not for R in Windows.
  I have now fixed this
  }
}
\section{Changes in version 1.0-00 (release date: 21-12-2013)}{
\itemize{
	\item Added examples
  \item Expanded documentation for function \link{g.analyse}
	\item Fixed bug in extraction of starttime that caused the starttime to be
  truncated to  00:00:00 in a fraction of measurements.
	\item Fixed bug in extract of temperature in function \link{g.calibrate}
  \item Deleted three explorative variables that were only extracted in
  \link{g.analyse} if argument doangle was set to TRUE in function \link{g.getmeta}.
  A number of bugs and the lack of referable journal publications made me decide
  to remove these variables while working on them. I intend on re-releasing these
  variables during the course of 2014. Please contact me if this causes you problems
}
}
\section{Changes in version 0.6-17 (release date: 8-8-2013)}{
\itemize{
  \item This was the first version  
}
}