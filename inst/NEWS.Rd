\name{NEWS}
\title{News for Package \pkg{GGIR}}
\newcommand{\cpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}
\section{Changes in version 2.6-5 (GitHub-only-release date:??-??-2022)}{
\itemize{
    \item Vignette for read.myacc.csv function included.
    \item Parameter objects documentation moved to g.shell.GGIR.
    \item Part4: Advanced sleeplog handling further improved as it now also handles dates in sleeplog far beyond recording dates.
    \item Part2 report: Number of days to plot now auto scales to length of measurement.
    \item Part2 report: Fixed bug introduced in 2.6-3 making it impossible to generate a report for more than 1 file and fix bug introduced in 2.6-4 messing up the merging of files.
    \item Part1: Now auto-converts files with GT3X extension to gt3x as this is required for read.gt3x.
    \item Part4: Removed unnecessary warning while loading sleeplog.
    }
}
\section{Changes in version 2.6-4 (GitHub-only-release date:13-04-2022)}{
\itemize{
    \item Part4: Bug fixed in handling of advanced sleeplog data when start of acc recording does not match start of sleeplog
   \item Deprecated functionality for reading GENEActiv csv files through g.readaccfile
    }
}
\section{Changes in version 2.6-3 (GitHub-only-release date:08-04-2022)}{
\itemize{
    \item Part2: Non-user facing arguments and object named BodyLocation replaced by sensor.location        to be consistent with the term used in part 3.
    \item Minor documentation updates to cover brondcounts and to reflect that
      bout.metric is no longer 4 by default
    \item Minor change to parallel processing so that GGIR does not create a cluster larger than the number of
      files to process. Fixes #535
    \item Part 4: Fix bug #531 re. sleeplog being ignored if first night is missing
    \item Part 4: Now relies more on ID extraction in part 1-3 and only attempts to extract ID from filename if that fails. Previously it extracted the ID twice.
    \item General: Speeded-up implementation of rounding decimal places as introduced in release 2.6-1
    }
}
\section{Changes in version 2.6-2 (GitHub-only-release date:06-03-2022)}{
\itemize{
    \item Part 1: Can now handle .gt3x produced by CentrePoint but for now user needs to change upercase file extension to lowercase
    \item Part 1: Fixed issue #520 with handling Movisens data
    \item Part 1: Fixed issue #523 with calibration of ActiGraph csv files
    \item General: Prevent redundant warnings resulting from some unnecessary checks of params objects
    \item Report generation per window: Fix undefined function call introduced in 2.6-1
  }
}
\section{Changes in version 2.6-1 (GitHub-only-release date:22-02-2022)}{
\itemize{
    \item Part 1: Fixed bug that prohibitted processing files in subfolders.
    \item Part 1: Dynamic range can now be extracted from Axivity .cwa files when not equal to 8g.
    \item Part 2: Qwindows logically ordered in reports.
    \item Part 2: Fixed bug #516 regarding passing on of dateformat.
    \item Part 4: Fixed bug introduced in 2.6-0 relating to sleeplog guider being assined even when no sleeplog used.
    \item Part 5: Improved handling of missing sleep estimate for first night.
    \item General: Fix bug introduced in 2.6-0 relating to storage of configuration file on comma-separated machines
    \item General: GGIR reports are now saved with a maximum of 3 decimal places for numeric variables.
  }
}
\section{Changes in version 2.6-0 (release date:02-02-2022)}{
\itemize{
    \item Part 1: Fixed warning when working with a vector of filenames as argument to datadir.
    \item Part 2: Added argument max_calendar_days to allow user to control the maximum number of calendar days to include in analysis, irrespective of argument strategy value.
    \item Part 2: Argument maxdur is no longer specific to strategy 1, but applied irrespective of value of argument strategy.
    \item Part 4: Fixed issue that sleeplog is indicated as the guider even when when corresponding timestamp cannot resolve to timestamp.
    \item Part 5: Time series output filenames now auto-stripped from [.]|rdata|csv|cwa|gt3x|bin character string.
    \item Fixed issues introduced in 2.5-6 release: maxNcores was not defined in part 1, params_output and not passed on to part 3, and params objects were unneccesarily repeatedly checked.
  }
}
\section{Changes in version 2.5-6 (GitHub-only-release date:18-01-2022)}{
\itemize{
  \item Internal re-structuring: Arguments are now internally passed via parameter objects to make code and code documentation easier to navigate. This should not break backward compatibility and users should be able to continue working with their older R scripts.
  }
}
\section{Changes in version 2.5-5 (GitHub-only-release date:18-01-2022)}{
\itemize{
  \item Fixed bug #484 affecting part 5 report not being able to generate because non-matching columns in milestone data
  \item Part 2: Recently introduced BrondCounts and ZeroCrosssing Counts no long auto-scaled by 1000 like all the other metrics as these are not in gravity-units.
  \item Part 3: Fixed warning when SRI is calculated on DST day with 23 hours
  \item Part 5: sibreport and timeseries now stored per sib definition.
}
}
\section{Changes in version 2.5-4 (GitHub-only-release date:11-01-2022)}{
  \itemize{
    \item Part 1: Now able to process modern .gt3x data with thanks to R package read.gt3x.
    \item Part 1: Timegaps and zeros across all three axes in ActiGraph data (.csv and .gt3x) now automatically imputed by last recorded value normalise to vector of 1.
    \item Adding BrondCounts as optional acceleration metric and dependency on activityCounts, and enabling Sadeh and Galland algorithms to use it for sleep estimation.
  }
}
\section{Changes in version 2.5-3 (GitHub-only-release date:23-12-2021)}{
  \itemize{
    \item Part 1: Deprecating function g.metric as its functionality has been taken over by g.applymetrics (this has been announced to its users with a warning for over a year now).
    \item Part 2: Adding warning when ID cannot be extracted from file based on argument idloc.
    \item Part 4: Empty or incomplete sleeplog rows now better ignored
    \item Part 4: No longer an R warning is given when ID is missing in sleeplog, because this is common and user can already see it in csv reports.
  }
}
\section{Changes in version 2.5-2 (GitHub-only-release date:09-12-2021)}{
  \itemize{
    \item Part 3: Now makes sure that HASPT is skipped when user configures def.noc.sleep as a set time window. This
    feature that probaly few people use nowadays broke with the 2.5-0 release.
    \item g.shell.GGIR: Now gives warning when user supplies double arguments.
    \item Part 4: Now warns when none of the IDs in the sleeplog could be matched with accelerometer data.
    \item Part 1: read.myacc.csv fix bug with argument rmc.check4timegaps
    \item Part 3: Fix #472 SRI calculation not possible when complete absence of sleep in recording
    \item Part 5: Experimental nap detection added to report and time series, currently only model for 3.5 year olds available.
  }
}
\section{Changes in version 2.5-1 (GitHub-only-release date:03-11-2021)}{
  \itemize{
    \item g.shell.GGIR: Removing forced assignment of sleepwindowType argument to "SPT"
    \item g.shell.GGIR: Reviving setwindow option def.noc.sleep, which broke
  }
}
\section{Changes in version 2.5-0 (release date:25-10-2021)}{
\itemize{
  \item Minor updates to function documentation and vignettes
  \item visualreport: Month now displayed by name rather than number to avoid confusion about format.
}
}
\section{Changes in version 2.4-3 (GitHub-only-release date:20-10-2021)}{
  \itemize{
    \item Vignette expanded with documentation on non-default variables in part 4 csv report
    \item Vignette added with a tutorial on how to perform segmented day analysis
    \item Part 1: The sensor fusion functionality introduced in version 2.2-2, as a bit
    of an experimental development, has now been removed from GGIR as added value turned out
    to be limited.
    \item Part 2: Code for ID extraction tidied up, and new idloc argument options added.
    \item Part 2: Activity log can now also have empty days.
    \item Part 2: Now also exported in long format if qwindow has length longer than 2.
    \item Part 3 and 4 expanded with Sleep Regularity Index.
    \item Part 4 number of awakenings is now also in the output.
    \item Part 5 LUX per segment calculation bug fixes.
    \item visualreport code modified such that it visualises any day with at least 30 minutes of data
    unless sleep could not be estimated from the corresponding night.
  }
}
\section{Changes in version 2.4-2 (GitHub-only-release date:14-09-2021)}{
  \itemize{
    \item Part 1 now able to derive zero-crossing counts needed for Sadeh algorithm
    \item Part 2 function g.conv.actlog to use activity log to guide qwindow now able to tailor dateformat
    \item Part 3 now option to tailor SPT detection for hip data by using horizontal
      longitudinal angle as indicator of lying
    \item Part 3 code restructured to estimate SPT and SIB in separated function (HASIB and HASPT)
    such that g.sib.det is more readable.
    \item Part 3 now option to select SIB algorithm, including: vanHees2015, Sadeh1994,
    and Galland2012
    \item Part 4 now able to handle more advance sleeplog format that also contain nap
    and nonwear entries per date
    \item Part 4 report now includes guider estimates also in 'cleaned' report
    \item Part 4 WASO and NOA (Number of awakenings) added to output
    \item Part 4 Able to handle time in bed diaries/algorithms, and generates sleep
    latency and sleep efficiency estimate if available
    \item Part 5 now able to export sustained inactivity bouts and self-reported
    naps and nonwear to csv report to aid nap analysis
    \item Part 5 numeric timing of MX-metrics changed to be hour of the day
    \item Part 5 Lux per segment variables now turn LUX during SPT to zero
    \item Part 4 legend added to visualisation
    \item Part 5 bug fixed that caused part5 single row output to be stored as a single column
  }
}
\section{Changes in version 2.4-1 (GitHub-only-release date:12-07-2021)}{
  \itemize{
    \item Part 1 fixed bug related to g.getmeta reading movisens files
    \item Part 1 resampling function expanded with option to choose interpolation technique
    \item Part 3 fixed bug introduced in 2.4-0 making it impossible to process long
    recordings with many sustained inactivity bouts
  }
}
\section{Changes in version 2.4-0 (release date:03-06-2021)}{
  \itemize{
    \item Part 5 LUX variables per segment of the day improved
    \item Removing calls to closeAllConnections as requested by CRAN
  }
}
\section{Changes in version 2.3-3 (GitHub-only-release date:20-05-2021)}{
  \itemize{
    \item Part 5 fragmentation analysis added
    \item Part 5 report, added argument week_weekend_aggregate.part5
    \item Part 5 intensity gradient now also extracted here for waking hours
    \item Part 5 LUX variables added (beta-implementation)
    \item Part 2 and 5, Bout detection algorithm improved, see bout.metric 6
    \item Parts 1, 2, 3 and 5: Added argument maxNcores to control maximum number
    of cores to be used in parallel processing mode
    \item Part 1 fixed bug related with ENMOa calculation which made the metashort data frame to be empty
    \item Part 2 fixed bug to report generation in part 2
    \item Fixed bug related to visualization report when a file has not been processed in part 4
  }
}
\section{Changes in version 2.3-2 (GitHub-only-release date:21-04-2021)}{
  \itemize{
    \item Part 1 when using ad-hoc data format: now able to handle timestamp column.
    \item Part 2 report when using ad-hoc data format with header: now able to extract
    and store serial number in part2 report.
    \item Test file generation for unit tests improved. Thanks Lena Kushleyeva.
    \item Part 2 estimation of longitudinal axis and related error handling improved
  }
}
\section{Changes in version 2.3-1 (GitHub-only-release date:26-03-2021)}{
  \itemize{
    \item Part 3 bug fixed in usage of argument dayborder when not 0.
    Thanks Ruben Brondeel for spotting this.
    \item Part 3 bug fixed in recognition fraction night invalid for first night.
    Thanks Ruben Brondeel for spotting.
    \item Part 1 fixed warning related to closing connection to bin files. Thanks John Muschelli.
    \item Part 2 fixed warning that deprecated IVIS argument does not exist
  }
}
\section{Changes in version 2.3-0 (release date:17-02-2021)}{
  \itemize{
    \item Part 5 bug fixed for g.part5.fixmissingnight when night is also missing in sleeplog
    \item Part 1 Documentation for sensor fusion functionality improved
    \item Part 2 csv report generation speeded up
  }
}
\section{Changes in version 2.2-2 (GitHub-only-release date:13-02-2021)}{
  \itemize{
    \item Part 5 bug fix in waking up time for first night, in WW mode and HDCZA guider
    \item Various minor function expansions to documentation and vignette
    \item Part 2 Earlier draft implementation of IV and IS metrics now better tested and bugs fixed.
    \item Part 2 Default value for argument IVIS.activity.metric changed to 2.
    \item Migrate codecoverage testing to Github Actions
    \item Part 1 Implemented initial sensor fusion functionality for AX6 cwa data
  }
}
\section{Changes in version 2.2-1 (GitHub-only-release date:12-01-2021)}{
  \itemize{
    \item Part 3 Fix bug introduced in 2.2-0 for recordings without sustained inactivity bouts
    \item Transitioned from Travis+Appveyor CI to GitHub Actions
    \item Part 5 fixed issue with expected max number of files to process
    \item Enabled console display of GGIR version number when running g.shell.shell under Windows
    \item Embedded new GGIR logo and update layout of vignette
  }
}
\section{Changes in version 2.2-0 (release date:22-11-2020)}{
  \itemize{
    \item Part 5 improved midnights identification when the recording starts at 0am
    \item Part 5 fixed days misclassification for MM output when recordings starts with some non-wear days
    \item Documentation added to vignette for L5TIME_num and M5TIME_num
  }
}
\section{Changes in version 2.1-3 (GitHub-only-release date:20-10-2020)}{
  \itemize{
    \item Depricating sessionInfo storage as it caused problems for large scale parallel processing
    \item Part 1 fix to 1 minute time shift when recording starts at full minute.
    \item Improved compatibility with older .cwa file formats.
  }
}
\section{Changes in version 2.1-2 (GitHub-only-release date:25-09-2020)}{
  \itemize{
    \item Part 5 bug fixed with day name and date allocation for daysleepers for MM report
    \item Fix bug part 4 and 5 with missing nights which are not accounted for when assessing max night number.
    \item Fix bug part 3 introduced in version 2.1-0 re. SPT detection for daysleepers.
    \item Fix redefinition of the time windows (with "MM") in part 5 to include all recording days when the measurement starts with multiple non-wear days.
    \item GGIR version now displayed in console when running GGIR
    \item Bout metric 5 added to enable not allowing for gaps in bouts.
  }
}

\section{Changes in version 2.1-1 (GitHub-only-release date:07-09-2020)}{
  \itemize{
    \item Part2 argument qwindow now able to handle person specific activity diary
    \item Intensity gradient now also extracted from long MX windows, see MX.ig.min.dur
  }
}
\section{Changes in version 2.1-0 (release date:27-08-2020)}{
  \itemize{
    \item Part 2 now able to be guided by activity log (see qwindow)
    \item Part 2 now stores output also in long format if day is segmented
    \item Actigraph serial number now extracted from the csv fileheader
    \item Actigraph serial number now used to assign correct dynamic range per
    Actigraph generation (6 and 8 g respectively). Previously 8 assumed.
    \item Part 2 indicator added of which axis is most correlated with itself
    over 24 hours, for hip data this may be indicator of vertical axis orientation.
    \item Part 1 bug fixed in timestamp generation that caused measurements
    that start exactly at the hour and 0 minutes to have a 1 minute offset in time.
    \item Part 1 to reduce data storage size epoch level metric values are now
    rounded to 4 decimal places.
    \item Part 4 report, issue fixed with double night entries.
    \item Bug fixed with Actigraph starttime recognition when machine language
    is not English and months are expressed in b or hh format.
    \item Bug fixed in accounting for daysleeper in first night ofr recording.
  }
}
\section{Changes in version 2.0-2 (GitHub-only-release date:18-06-2020)}{
  \itemize{
    \item Visual report, fixed argument viewingwindow for g.plot5 (visualreport)
    \item Part 1 rmc.myacc.csv now raises warning when timestamps are not recognised
    \item Part 1 fixed rmc.dynamic_range not correctly passed on in 2.0-1
    \item Part 5 sub-function round_seconds_to_ws3new now handles missing values
  }
}
\section{Changes in version 2.0-1 (GitHub-only-release date:04-06-2020)}{
  \itemize{
    \item Now able to read gzipped csv files
    \item Part 2 Fixed midnight selection being off by one sample
    \item Part 1 tidied up metric calculation and added a few metrics
    \item Part 2 Added strategy 4 (ignore data before first midnight)
    \item Data from movisens accelerometers can be now processe
    \item Part 5 timestamps in timeseries output in RData format (new since 2.0)
    now correctly accounts for timezone
    \item Part 4 now has option to only exclude the first or only the last night
  }
}
\section{Changes in version 2.0-0 (release date:30-04-2020)}{
  \itemize{
    \item Now R version 4.0 compatible
    \item Part 1 Clipping detection expanded: If any value in block more than 150 percent
    dynamic range then ignore entire block.
    \item Part 2 report now able to handle changing variable count due to missing data
    \item Part 2 L5M5 better able to handle small qwindow intervals
    \item Part 3 HDCZA algorithm expanded to be able to detect daysleepers
    \item Part 3 various improvements to qc plots.
    \item Part 5 now also stores full and cleaned output
    \item Part 5 now better handles missing days in part 4 output.
    \item Part 5 behavioral class SIB removed from daytime
    \item Part 5 time series export more user-friendly.
    \item Part 5 function code split up in 7 new functions.
    \item Part 4 + 5 argument data_cleaning_file added.
    \item Part 4 + 5 output variable names improved and documented in vignette
    \item Numunpack function moved back to c++
    \item Various updates to visualreport (plot5 function)
    \item External function embedding feature added
    \item We now consistently refer to ID (not id) and calendar_date, spelling was
    inconsistent.
    \item Vignette now has documentation on sleep and time-use analysis.
  }
}
\section{Changes in version 1.11-1 (GitHub-only-release date:28-02-2020)}{
  \itemize{
    \item Metric ENMOa now facilitated for MVPA calculation
    \item Bug fixed with part4 daysleeper handling
    \item Part5 WW window calculation improved, first day now uses sleeplog or
    HDCZA algorithm estimate, and last day is ignored if no sleep estimates are
    available. This also affects csv exports by argument save_ms5rawlevels.
    \item Added explanation to vignette on how to use published cut-points.
    \item Axivity AX6 (acc + gyro) in cwa format now supported for file reading,
    actually using the gyro data for feature calculation is future work.
    In the mean time, gyro signal will be ignored by the rest of GGIR.
    \item Axivity AX3 acc data in cwa format can now also be read if dynamic
    range is not 8g. Previously this was not possible.
    \item Fixed bug related to visual report generation when qwindow is set to
    non-default value.
    \item Added way to handle Actigraph files which start with several days of
    zeros which complicates the auto-calibration.
    \item Default for desiredtz to timezone of machine.
  }
}
\section{Changes in version 1.11-0 (release date:03-12-2019)}{
\itemize{
  \item Fixed bug that emerged in previous version with GENEActiv .bin data not
  being processed by g.getmeta for some files.
  \item read.myacc.csv is now able to resample data with irregular sample rates
  and handle timestamps in character format.
  \item function resample can now handle any matrix size, previous only 3 columns.
  \item Fixed bug when using multiple non-angle metrics in part1 and trying to calculate
  1to6am average metric value in part 2.
  \item Expanded Actigraph date format recognition ability.
  \item visualisationreport (function g.plot5) enhanced with colour coding for activity
  classes.
  \item Fixed bug in sleep period time recognition for first day of measurement.
}
}
\section{Changes in version 1.10-7 (release date:06-10-2019)}{
\itemize{
  \item Fixed functionality to supply calibration coefficients file to backup.cal.coef.
  \item Fix OSx flavor not being released on CRAN in previous version.
  \item Upgrades to foreach loop to ease package maintenance
  \item Documentation part4 expanded to clarify difference between full and cleaned report.
  \item Non-wear detection now possible at 1 minute resolution, previously 5 minute.
  \item Function read.myacc.csv now able to utilize 3rd party wear detection.
}
}
\section{Changes in version 1.10-5 (release date:13-09-2019)}{
\itemize{
  \item Device serial number recognition in Axivity cwa files fixed
  \item New GitHub release, because previous version did not install.
}
}

\section{Changes in version 1.10-4 (release date:08-09-2019)}{
\itemize{
  \item Fixed bug introduced in version 1.10-1 in the conversion from numeric to character sleep times
  \item Dependencies of dependencies removed from the DESCRIPTION file
  \item Fixed 1to6am variables, which was wrong in version 1.10-1
  \item Added functionality to handle accelerometer data from any accelerometer brand
  stored in csv files via read.myacc.csv. Pass on the arguments of this function to
  g.shell.GGIR to use this functionality.
  \item Added reference to new GGIR paper to the documentation
}
}
\section{Changes in version 1.10-1 (release date:23-08-2019)}{
\itemize{
  \item Configuration file option now added to g.shell.GGIR and documented in vignette
  \item metric lfen added (low-pass filter signels followed by Euclidean norm)
  \item issues fixed with passing on of hb and lb arguments
  \item argument backup.cal.coef can now also handle data_quality_reports.csv files
  \item part 1 now automatically uses previously generated calibration coefficients if the datafile
  was previously processed, see documentation g.part1 for further details.
  \item Enabled multiple values for argument winhr, by which part2 can now calculated
  for example L3M3, L5M5, L6M6, L10M10 all in one go. Further, option added (qM5L5) to
  extract percentiles (quantiles) from the value distribution corresponding to these windows.
  \item Moved IVIS calculation to seperate function, and split up function g.analyse.
  \item Now possible to specify time windows that need to be ignored for imputation, see TimeSegments2ZeroFile.
  \item Default value for argument mode changed from mode = c(1,2) to mode = 1:5, to perform all the parts.
  \item Checks added for user write and read access permission, and subsequent warnings given..
  \item Parts 1, 2, 3, and 5 can now use multi CPU cores which speeds up the processing.
  \item Argument minimumFileSizeMB added to g.part1 to aid filtering out too small data files.
  }
  }
\section{Changes in version 1.9-2 (release date:03-07-2019)}{
\itemize{
  \item Added functionality to work with studies where accelerometer is configured in one
  timezone and used in other timezone. Only functional for AX3 cwa data at the moment.
  See argument 'configtz'.
  \item Sleep estimation is now skipped if a day only has one sustained inactivity bout
  \item Arguments ignorenonwear default value changed to TRUE and def.noc.sleep default changed
  to 1 in line with literature.
  \item Fixed AX3 csv format starttimestamp recognition
  \item Part5 csv export now also includes class labels (previously only class numbers).
  }
  }
\section{Changes in version 1.9-1 (release date:08-05-2019)}{
\itemize{
    \item Fixed part5 output midnight-midnight window when monitor not worn during first days.
    \item Fixed assumption that when using argument idloc=2 the ID has a letter at the end, and automatically
    removes the last value in the index. The code now first checks for this assumption.
    \item Update vignette with a more elaborate explanation of the optional arguments to g.shell.GGIR.
  }
}
\section{Changes in version 1.9-0 (release date:14-03-2019)}{
\itemize{
  \item functionality storefilestructure should now store filestructure in output of part 2, 4 and 5.
  \item filelocationkey.csv that was previously written by storefilestructure was redundant and removed.
  \item sessioninfo storage improved.
  \item Fixed bug that caused part2 to provide incorrect window specific estimates on first day.
  of measurement if day is incomplete (not 23, 24 or 25 hours).
  \item Calibrate function now better able to handle files with more than a week of data, where
  auto-calibration struggles to find enough sphere data in the first week.
  \item Fixed part5 output midnight-midnight window when monitor not worn during first days.
  }
}
\section{Changes in version 1.8-1 (release date:11-01-2019)}{
\itemize{
  \item Part4 handling of clocktime 9am corrected in addition to fix from version 1.7-1.
  \item Fixed bug for Actigraph csv header recognition when column 2 and 3 are NA (prevented processing before)
  \item Fixed bug in g.report.part5 in the calculation of the total number of valid days per person.
  \item Fixed bug that caused part5 to struggle with timezones west of greenwhich time.
  \item desiredtz added as explicit argument to g.inspectfile, g.cwaread, and g.dotorcomma.
  \item Fixed bug in pageindexing in g.readaccfile when machine runs out of memory.
  \item Fixed bug in pageindexing in Actigraph csv files (10 rows in raw data skipped every block (day)) of data.
  \item Added more informative warning message in g.report.part2 if file cannot be read.
  \item Fixed bug in scenario when person is daysleeper and wakingup time occurs before noon, and 12 hours too early.
  \item Fixed bug re. storefolderstructure=TRUE causing 2 variables to drop in g.report.part4 if storefolderstructure=TRUE.
  \item g.intensitygradient enabled to handle absense of data.
  \item tidied up some of the redundant or even confusiong information printer to the console
  }
}
\section{Changes in version 1.7-1 (release date:25-11-2018)}{
\itemize{
  \item Fixed order of Nbouts output in g.part5 was not consistent with bout duration.
  \item Functionality added to read AX3 Axivity csv files that have the following
  characteristics: Raw data in g-units, not resampled, and with timestamps stored in
  the first column.
  \item Fixed bug that caused the date of the last measurement day in part5 output
  to be one day ahead if argument dayborder=12.
  \item Part5 struggled to process measurements with more than 40 days, now fixed.
  \item g.getstarttime can now also handle dates in Actigraph csv-file headers that are
  dot separated, e.g. 20.05.2016, before it only handled 20-05-2016 and 20/05/2016.
  \item Part4 handling of clocktime 9am corrected.
  \item Filename identification in part5 improved when saving raw level data.
  \item Fixed ability to read wav file header when shorter than expected.
  \item Fixed issue with CWA read functionality causing some files not to be completely read
  }
}
\section{Changes in version 1.6-7 (release date:23-9-2018)}{
\itemize{
  \item Link with Zenodo for doi generation removed.
  \item Broken url fixed in vignette.
  }
}
\section{Changes in version 1.6-1 (release date:21-9-2018)}{
\itemize{
  \item Report part 4: Count of available nights with accelerometer data fixed
  \item Report part 4: NA and NaN values replaced by empty cells like in other reports
  \item Intensity gradient analysis added to part2 output according to Rowlands et al.
  MSSE 2018, doi: 10.1249/MSS.0000000000001561
  \item Documentation on part4 output variables improved.
  \item Providing incorrect value of sleeplogidnum in part4 should now provide a more
  informative error message
  \item Added functionality to handle timestamps that start with the year.
  }
}
\section{Changes in version 1.6-0 (release date:29-7-2018)}{
\itemize{
  \item Fixed timezone dependency of g.analyse (affected only order of columns), g.part5
  (affected time detection), and consequently test_chainof5parts.R
  \item Read functionality for Actigraph csv files speeded up by replacing
  read.csv by data.table::fread
  \item Argument qwindow is now able to handle input vectors longer than 2
  and will derive all part2 variables for each time window that can be defined
  from the values of qwindow, e.g. value =c(0,8,24) will analyse the windows:
  0-8, 8-24 and 0-24 hour.
  \item Argument L5M5window depricated because argument qwindow now defines the
  window over which L5M5 analysis is performed.
  \item Argument winhr is now reflected in g.analyse/g.part2 output variable
  names, previously this variable name was hardcoded as L5M5, even if winhr was not 5.
  \item Part2 output variable names updated to be more consistent in
  structure and more explicit about the timewindow over which they
  are calculated. The variables that were calculated over the full
  recording (using diurnal normalization) now have the extension
  "fullRecording", this in contrast to the variables that are only
  calculated from measurement days with 'enough' valid data.
  \item Fixed calculation of N valid WEdays and Nvalid WKdays in part2 that was wrong
  since version 1.5-21. It counted all days and did not exclude days with insufficient
  amount of data.
  \item Fixed warning message in test of g.part5.
  \item output variable acc_timeinbed renamed to acc_SptDuration to avoid confusion
  with terminology used in supporting papers, which are about SPT (sleep period time)
  detection and not about time in bed detection.
  \item output variable acc_dur_noc renamed to acc_SleepDurationInSpt to improve clarity
  of variable name relative to acc_SptDuration
  }
}
\section{Changes in version 1.5-24 (release date:9-7-2018)}{
\itemize{
  \item Variable names ENMO accidentatly disappeared from g.analyse output
  in 1.5-23, this has now been reversed.
  \item Fixed issue in g.part5 in handling the last day of measurement when using
  'MM' windows and dayborder not equal to 0 (midnight) sometimes resulting in
  the last day longer than 25 hours.
  \item Fixed error message in g.part5 that occurs when calculating L5M5 when day
  is shorter than L5M5 time window.
  }
}
\section{Changes in version 1.5-23 (release date:4-7-2018)}{
\itemize{
  \item Unit tests speeded up by using a smaller test file.
  }
}
\section{Changes in version 1.5-22 (release date:3-7-2018)}{
\itemize{
  \item g.part4 bug fixed that was introduced in version 1.5-21 regarding the handling
  of daysleepers (people who wake up after noon) causing sleep estimates to be zero.
  }
}
\section{Changes in version 1.5-21 (release date:22-4-2018)}{
\itemize{
  \item g.part5 is now able to generate summary from all measurment days (thanks Jairo).
  \item MM results in g.part5 now correctly stores onset and waking for single date per row.
  \item Various checks added to g.part4 to ensure measurements in all shapes and size can be
  processed.
  \item minor improvements to documentation g.part5
  \item size of example data and vignette images reduced to reduce package size
  }
}
\section{Changes in version 1.5-18 (release date:18-3-2018)}{
\itemize{
  \item Improved handling of day saving time in g.part3 and g.part4 within the two nights
  in a year when the clock changes (DST was already correctly handled in the rest
  of the year)
  \item Algorithm HDCZA is now the default algorithm to use, if a sleeplog
  is used and an entry is missing for a particular night.
  \item pdf generation in part 3 is now optional (argument do.part3.pdf), this may
  be useful for slightly speeding up data processing as it takes a few second to generate
  the plot.
  \item test added for g.getbout function.
  \item bug fixed in timestamp recognition for object timebb in function g.part5
  }
}

\section{Changes in version 1.5-17 (release date:19-2-2018)}{
\itemize{
  \item SPT-window detection now updated with a constrained threshold to make it more robust
  against between accelerometer brand differences. This is the approach used for our PSG in
  <https://www.biorxiv.org/content/early/2018/02/01/257972>
  }
}

\section{Changes in version 1.5-16 (release date:17-1-2018)}{
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
