\name{NEWS}
\title{News for Package \pkg{GGIR}}
\newcommand{\cpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\section{Changes in version 1.5-17 (release date:19-2-2017)}{
\itemize{
  \item SPT-window detection now updated with a constrained threshold to make it more robust
  against between accelerometer brand differences. This is the approach used for our PSG in
  <https://www.biorxiv.org/content/early/2018/02/01/257972>
  }
}

\section{Changes in version 1.5-16 (release date:17-1-2017)}{
\itemize{
  \item cwaread issue #57 fixed
  \item SPT-window detection included (work in progress)
  \item cpp code fixed which did not compile anymore
  \item a machine specific function test removed following feedback from CRAN maintainers
  }
}

\section{Changes in version 1.5-12 (release date:8-8-2017)}{
\itemize{
  \item Fixed bug introduced in 1.5-1: large window size of 3600 seconds was accidentally
  hardcoded when the g.readaccfile function was added to GGIR in version 1.5-1
  \item Codecov testing added and badge added to the README file
  \item Functions added to create dummy accelerometer file (csv) and dummy sleeplog (csv),
	needed for testing
	\item Bug fixed in g.wavread to recognize .wav extension file header for files with alternative
	header size
  \item Default IVIS_epochsize_seconds parameter updated from 30 to 3600
  \item g.part1 messages on the consolo are now condensed printing only the number of the
	blocks loaded separated by spaces rather than new lines
  \item Split g.part5 function into multiple smaller functions
  \item Replace hard-coded "Europe/London" in g.part5 by desiredtz, to make the function
	work for users outside the UK
  \item Data frame output from g.part5 is now tidied up by removing empty rows and columns generated
  \item Calculation of mean amplitude deviation (MAD) is now implemented in g.part1 by the argument do.mad
  \item Percentiles and levels in g.part2 are now calculated for all the acceleration metrics selected
  \item g.part3, g.part4 and g.part5 are now independent on metric ENMO to work, argument acc.metric allows
	the user to select which metric to use to describe behavior
  \item Argument dayborder is now included in g.part5 to consider the whole measurement in case the protocol
	starts after midnight
	\item Jairo Migueles added to list of contributors
  }
}

\section{Changes in version 1.5-10 (release date:12-7-2017)}{
\itemize{
  \item Date format recognition improved for Actigraph csv files
  }
}

\section{Changes in version 1.5-9 (release date:21-5-2017)}{
\itemize{
  \item g.inspectfile now also functional with cwa data
  \item option added to enforce dynamic range with argument dynrange
  \item vignette expanded
}
}
\section{Changes in version 1.5-7 (release date:9-5-2017)}{
\itemize{
  \item vignette expanded
  \item Bugs fixes in relation to new cwa-read functionality c++ routine registration
  \item Documentation added for all underlying functions
}
}
\section{Changes in version 1.5-3 (release date:29-4-2017)}{
\itemize{
  \item vignette added
  \item Bugs fixes in relation to new cwa-read functionality
  \item Bugs fixes in correct number of days recognition in part5
}
}
\section{Changes in version 1.5-1 (release date:23-4-2017)}{
\itemize{
  \item Removed teLindert2013 metric, because it was not used and not verified
  \item Split g.getmeta function into multiple smaller functions
  \item Added IS and IV variables to g.analyse (still in explorative version)
  \item bug fixed related to wav file read errors
  \item function g.cwaread added (credits to E. Mirkes) for reading
  Axivity .cwa-format data. g.shellGGIR will automatically use this function
  when input data has .cwa extension
  \item bug fixed related to for GENEactiv starttime recognition which was
  introduced in version 1.2-11
  \item g.part5 documentation added on its output
}
}
\section{Changes in version 1.4 (release date:22-1-2017)}{
\itemize{
  \item bug fixed in functionlity to process only specific days in measurement
  (credits to J Heywood)
  \item bug fixed in midnight recognition in g.part5
  \item improvement to handling of measurements that start a few minutes before
  midnight (credits to E Mirkes)
  \item bug fixed related processing files shorter than 1 day, introduced in
  previous version
  \item documentationa added for Axivity wav-format data
  \item start made with implementing code testing functionality using testthat and covr
  \item documentation improved for argument def.noc.sleep in function g.part4
}
}
\section{Changes in version 1.3-2 (release date:28-11-2016)}{
\itemize{
  \item g.part5 added. g.part5 merges the output from the first four parts
  \item Functionality added to read Axivity wav-format files with acceleration in first
  three channels. No documentation added yet until I have more confirmation that it works
  well
}
}
\section{Changes in version 1.2-11 (release date:31-8-2016)}{
\itemize{
  \item Bug fixed related to MVPA variable. The bug was a result of
  the updates in version 1.2-10
}
}
\section{Changes in version 1.2-10 (release date: 28-8-2016)}{
\itemize{
  \item Changed function argument 'mvpa.2014' into 'bout.metric' across
  the package in preparation for a central defintion of bouts for future
  GGIR version which will not only provide bout calculations for MVPA
  but also for inactivity. Further, I added function g.getbout to improve
  transparency about the bout calculations
  \item Updated documentation for function g.analyse to clarify different
  bout metric definitions
  \item Improvements to the functionality to only process specific days from
  a long accelerometer file using the argument selectdaysfile
  \item Timestamps are now in ISO 8601 format. I have updated the code such that it
  can still handle old timestamp format, but newly processed files will produce
  timestamps in the ISO 8601 format. The practical difference is that it will
  include a numeric timezone indicator.
  \item Bugs fixed in data selection in g.getmeta function. In the old code it tended
  to drop the last 30-45 minutes of a file.
  \item Added more optional features to be generated by g.getmeta, including
  rolling medians of the acceleration signals.
}
}
\section{Changes in version 1.2-8 (release date: 24-5-2016)}{
\itemize{
  \item Updated documentation for function g.analyse to clarify different
  mvpa bout definitions
  \item mvpa.2014 = TRUE turned back on again (was disable in last version)
}
}
\section{Changes in version 1.2-7 (release date: 12-5-2016)}{
\itemize{
  \item mvpa.2014 TRUE/FALSE was swapped, FALSE is now the default
  \item mvpa.2014 = TRUE disabled
}
}
\section{Changes in version 1.2-6 (release date: 10-5-2016)}{
\itemize{
  \item Modified warning message in relation to the change in MVPA bout defintion
}
}
\section{Changes in version 1.2-5 (release date: 8-5-2016)}{
\itemize{
  \item Accelerometer non-wear time is now also reported in the output of part 4
  \item Part 1 is now able to only process specific days of a measurement via argument
  selectdaysfile. This is useful when measurement lasts for a week and the participant
  is instructed to only wear the accelerometer on one or two specific days.
  \item Argument mvpa.2014 and closedbout added to function g.analyse. The calculation of
  MVPA (moderate and vigorous physical activity) has been available since 2014.
  This calculation has been improved, but the user has the option to continue using
  the old calculation.
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
