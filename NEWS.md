# CHANGES IN GGIR VERSION 3.2-1

- General: Fix bug in parameter check concerning the recognition of a Boolean vector #1276

- Part 5: Fix bug in handling diary for daysleepers in the visualreport. This does not affect any other GGIR output. #1272

- Part 5 and visualreport: When wake up time at start of recording is added in part 5 no log was kept of the guider and bedend column was not recognised, this is now fixed. #1273

- General: Add functionality to split recordings based on recording specific timestamps or dates, for example useful when studying effect of interventions. This new functionality comes with new parameters recording_split_times, recording_split_overlap, recording_split_timeformat #1278.

- Part 5: Now also runs lux analysis for MM window #1271

- Part 2: Improve handling 23 hour days in day summary #1281

- Part 4: Detected duplicated ID in sleeplog and generate error to inform user #1281

- Part 4: No longer show warning when GGIR part 4 fall back on 9am-7pm guider because it is
unlikely to be used and if it is used the user will be able to see this in the output. #1281

# CHANGES IN GGIR VERSION 3.2-0

- Part 2:

  - Bug fixed in handling part 1 output with the uncommon scenario of having only 1 metric (in object metashort) #1265

  - Make sure all event variables are stored in event summary and advanced event variables are skipped until testing is finished #635

- Part 6 and new visual report: Fix bug related to determining the combination of thresholds to use for the 
  visual report and for part 6 analyses when there are several options and 
  part6_threshold_combi is NULL. #1260

- Part 6: Improve robustness of column name initialization for part6 output #1240

- Visualisations:

  - Bug fixed in handling recordings that start at midnight #1257

  - The new and old general pdf report have been renamed to clarify which one is old (eventually to be deprecated) and which one is new.

  - Report_...pdf has been renamed to old_report_...pdf.

  - Time_report...pdf has been renamed to report_...pdf
  
- Part 3: Default for parameter do.part3.pdf changed from TRUE to FALSE because this visualisation has become redundant with the addition of the new general visual report.

# CHANGES IN GGIR VERSION 3.1-11

- Part 2:

  - Fix bug related to determining the files to be included in the part 2 csv report, 
  causing some files to be unnecessarily excluded from the part 2 csv report if the number
  of files in the part 2 milestone data is smaller than than number of milestone files
  in the part 1 milestone data, because internally it uses the part 1 milestone data
  file list as reference. #1252

  - Speed up activity diary extraction. #1250

  - Make sure part 2 event report, e.g. based on step detection, is saved to csv format 
  with intended sep_reports and dec_reports parameter values rather than default.
  
  - Simplify MVPA parameter specification in part 2 (mvpathreshold, boutcriter). When not 
  specified, GGIR copies the values from the corresponding part 5 parameter values (threshold.mod and 
  boutcriter.mvpa). #1247

- Part 2 and 4: Both activity diary and sleep diary are now always reloaded if the derived 
copy of it (in .RData) is older than the diary itself or if the diary name has changed. #1250

- Visual report: Recently added visual report is now also able to handle epoch sizes not equal 
to 1 minutes, and now also able to handle recordings 
with only 1 valid day, these were previously skipped. #1246

- Part 4:

  - Improvements to handling dates in sleeplog as a follow-up to work on #1243 in the previous release.
  
  - Added warning when there are two accelerometer recordings with the same participant ID.

  - Change extraction of imputation code from sleep diary, which is now assumed to 
  correspond to preceding night. #1251
  
- Part 1, 5 and 6: Update code to be backward compatible with R 4.2.0

- Part 5: Default values for parameters `do.sibreport` and `save_ms5rawlevels` changed
to TRUE, for `save_ms5raw_format` changed to "RData" in order to ease generating visual 
report, including our explorative nap detection, and working with part 6.

# CHANGES IN GGIR VERSION 3.1-10

- Part 4:

  - Speed up the loading of advanced format sleeplog
  
  - Improve automated recognition of date format in sleeplog

# CHANGES IN GGIR VERSION 3.1-9

- Part 3: 

  - Identification of daylight saving time days in the detection of the spt is improved as it had the risk to misclassify partial last days as daylight saving time.
  
  - In days classified as daysleeper, the window over which the fraction_night_invalid is calculated now also shifts to 6pm-6pm, as it used to report the nonwear within 12pm-12pm.
  
- Vignette: Now documented that missing dates can be skipped in advanced format sleeplog.

- Step detection: I have included the Verisense step detection R script inside GitHub repository folder 'user-scripts' to ease future maintenance. Verisense does not seem to be interested in maintaining their code, causing confusion about the latest version and people reaching out to me with questions.

# CHANGES IN GGIR VERSION 3.1-8

- Part 4: 

  - Parameter sib_must_fully_overlap_with_TimeInBed added to control whether sustained inactivity bouts (sib) 
  must overlap fully with the start and/or end of time in bed to be considered 
  sleep (default TRUE), this is consistent with functionality in the past, but 
  offers the freedom to experiment with alternative settings. #1223

  - Handle unexpected combinations of sleep diary column names. For example, if only inbed and wakeup are available then treat these as the time in bed period, while if only sleeponset and outbed are available then these are treated as SPT window.
  
- Part 5: Expanded functionality for exploring possibility of nap detection, this includes the addition of new parameter possible_nap_gap.

- Visual report: Added new visualreport that is automatically generated when visualreport = TRUE (default) and intended to eventually replace the problematic legacy report. You can set parameter old_visualreport = TRUE to turn off the old visualreport generation. #1173

- Part 4 + 5 + visualreport: If sleeplog has both reported sleeplog and time in bed process both. In part 4 we will still use one of the two but in the visualreport we display both. #967

- Part 2: Add parameters nonwearFiltermaxHours and nonwearFilterWindow to give user the option to filter short lasting nighttime nonwear #1218.

- Part 5 and 6: part6_threshold_combi when not specified now defaults to first threshold as
specified for light, moderate and vigorous intensity, respectively.

# CHANGES IN GGIR VERSION 3.1-7

- Part 3: Improved handling of DST, #1225

- Part 2: Code revisions in preparation for expansion of functionality to better facilitate external function produced event data. #653 and #1228

- Part 1: Ad-hoc csv file formats (read.myacc.csv functionality) now also excepts "UNIXmsec" as optional timestamp format. #1233

- Part 5: Improved handling of partially available and non-available qwindow segments in the recording #1229

# CHANGES IN GGIR VERSION 3.1-6

- Part 6:

  - Add DFA functionality #839 (credits: Ian Meneghel Danilevicz and Victor Barreto Mesquita)

  - Add fragmentation metrics, same function as in part 5 but now applied at recording level #839
  
  - Circadian rhythm analysis now ignores invalid nights controlled with new parameter "includecrit.part6".
  
  - Circadian rhythm analysis now provides consistent results for both regardless of whether time series are stored with or without valid windows with parameter save_ms5raw_without_invalid.

- Part 2 + 6: Revised and simplified IV and IS calculation which now ignores 
invalid timestamps and also comes with phi statistic (credits: Ian Meneghel Danilevicz). 
In part 2 we used to have 2 calculations, which is now replaced by just one and 
applied to all valid data points in the recordings. In part 6 this is repeated 
for the time window as specified with parameter part6Window. Further, IVIS 
now uses argument threshold.lig as threshold to distinguish inactivity from 
active, instead of a hardcoded threshold of 20 mg.

- Part 2: Increase sensitivity clipping detection from 80% to 30% of a window. If the long epoch has more than 30% of its values close to the dynamic range of the sensor then it will be labelled as clipping.

- Part 4: Now also has copy of part 5 variable ACC_spt_mg.

- Part 5:

  - Moving LXMX from part 5 to part 6 as these are circadian rhythm analysis.
  
  - Omitted fragmentation metrics for spt window as not meaningful given that part 5 only focuses on valid daytime behaviours.
  
  - includenightcrit.part5 added to allow for controlling inclusion of windows in part 5 based on their amount of valid data during spt window. Default remains the same.

  - Fix incorrect usage of part 5 inclusion criteria, which used fraction as percentage.

# CHANGES IN GGIR VERSION 3.1-5

- Part 5: 

  - Add parameters require_complete_lastnight_part5 to control whether last window is included if last night is incomplete. #1196

  - Handle sleeplog times beyond the end of the recording. #1206

  - Fixed minor bug in g.part5.addfirstwake causing the first wake is not correctly added when no SIBs are detected in between the start of the recorded and the algorithm or diary based estimate of wakeup time. #1198

  - Revise MM window definition in daylight saving time days, as it assumed fixed day duration of 24 hours #1211 

- General: GGIR version look-up in .onattach() no longer crashes when computer is offline, fixes #1203.

- Reports: The calendar_date and filename columns in csv reports have been standardized, as %Y-%m-%d and the input accelerometer file name, respectively. #1197

- Part 1: Reverse default value for nonwear_range_threshold as changed in 3.1-3 back to 150 as more research needed to support the change. #1172

- Part 1: Tidied up code in internal function detect_nonwear_clipping.R to ease future reviews. #1212

- Part 4: Undo changes to 3 line in g.part4 function from commit 3b9e2b7 (in between 3.1-1 and 3.1-2 release in June 2024) which broke functionality for handling sleep that starts before the double DST hour in the autumn and ends inside it. #1221

# CHANGES IN GGIR VERSION 3.1-4

- Part 3: Update threshold used for HorAngle to 60 degree, and auto-setting HASPT.ignore.invalid to NA when NotWorn guider is used. #1186

- Part 4:

  - Fixed issue with merging sleeplog advanced format and acc data starting at midnight, #1188
  
  - When sleeplog column name is accidentally left empty generate error message, #1138.

  - Improved logging of what guider was used when using NotWorn and optional backup guider, #1156

  - Skip night in part 4 csv report if guider was NotWorn, #1156
  
- Part 5: Fixed bug in g.part5.wakesleepwindows causing the first SPT window in a recording incorrectly to be  defined from the first timestamp in the recording. For details see: #1192


# CHANGES IN GGIR VERSION 3.1-3

- Report part 4 and 5:

  - Fix bug at identifying days and nights specified in the data_cleaning_file #1168

- Part 5: Data dictionary generates error when no valid days are seen dictionary, fixed #1176
  
- Part 5: Fix bug in addition of first wake-up time if not present #1179

- Part 2: Correctly skip MXLX calculation when it cannot be calculated, which caused error when trying to use the output. Fixes #1180

- Part 1: Add parameter nonwear_range_threshold to control range threshold for nonwear detection,
this used to be a constant. And default changed to 50mg. #1172

- Part 4: Improved error message when a sleeplog timestamp is not in expected format. #1184

# CHANGES IN GGIR VERSION 3.1-2

- Part 1:

  - Enable timegap imputation for ad-hoc csv data. #1154

  - Reduce constraints on value for parameter chunksize #1155.

  - When appending records ignore files that have not enough data to produce meaningful metashort and metalong objects in part 1. #1162

- Parts 2-5: Give more informative error when folders with expected milestone files are empty. #1144

- Part 3: Fix handling of recordings with only 1 midnight that start before 4am. #1160

- Part 3 and 4:

  - Revise NotWorn algorithm to work with both count and raw data with varying degrees of nonwear. Further, when parameter HASPT.algo is set to NotWorn then a second guider can now optionally be specified in case the accelerometer unexpectedly worn by the participant. #1089

  - Fix bug in detection of waking up after noon, referred to as daysleeper. #1165

- Report part 5:

  - fix bug that was introduced on 2024-Feb-19 in the calculation of wear percentage #1148
  
  - Rename variable sleep_efficiency to sleep_efficiency_after_onset, #1157

- Vignette: Migrated many sections from main CRAN vignette to wadpac.github.io/GGIR/

- Visualreport: Improve handling of recordings where the accelerometer was not worn.


# CHANGES IN GGIR VERSION 3.1-1

- Part 2: Corrected calculation of LXhr and MXhr which had one hour offset when timing was after midnight, #1117

- Part 1: Fixes reversion of reading `.csv.gz` files #1131.

- Loading GGIR: Stops interactive calling of `chooseCRANmirror` on `.onAttach` if interactive and CRAN mirror not set #1141.

- Part 4:

  - Corrected documentation for parameter relyonguider.

  - Ignore empty sleeplog columns, which are sometimes accidentally created the user. GGIR can now handle these. #1138

  - report: Bug fixed causing night_part4 column to not be used #1142.

- Part 5: Fix incorrect calendar date when window starts 5 seconds before midnight, #1082

# CHANGES IN GGIR VERSION 3.1-0

- Minor issue fixes introduced in 3.0-10 related to handling nights without sustained inactivity bouts, handling recordings without midnight and storage of GGIR version in the part 4 output when there are no valid nights.

- Part 5 report: Add option to filter part 5 windows based on parameter includedaycrit originally used in part 2, for details see documentation of parameter includedaycrit and #1113

- Part 1: Fix bug in identifying and naming the vector magnitude column #1122

# CHANGES IN GGIR VERSION 3.0-10

- Part 1:
  - Change default for parameter "minloadcrit" to 168 hours, see #1101.

  - Improve handling of failure to extract sampling frequency from gt3x file #1098

  - Fix bug in handling ad-hoc csv format data expressed in mg units #1110

  - Fix bug introduced in 3.0-6 affecting handling of time gaps, #1104

- Part 1-6: GGIR release number now stored in each milestone data file and in each
report to ease tracing what release was used per recording and per GGIR part, see #1101.

- Part 3: Revert change to HDCZA in 3.0-7, see issue #1102

- Part 4: Fixes issue #1095 introduced in 3.0-7 that causes GGIR part 4 to stop when processing data without non-default sleep algorithms.

- Part 2 and 5:

  - qwindow functionality enhanced to also consider fractions of minutes, #1093

  - Fix minor bug in activity diary loading when date format is %Y-%m-%d, #1090

- Part 5 and 6: Correct algorithm for coefficient of variance (CoV) in g.fragmentation, #1105

# CHANGES IN GGIR VERSION 3.0-9

- Part 5: Temperature (if available) added to time series output #1085.

- Part 5: Fix minor bug in merging night level sleep variables into part 5 report, this does not affect the main part 5 estimates such as time spent in intensity ranges #1086.

- Part 4: Allow handling sleeplog with only one record #1083

- General: simplify installation for typical use cases by moving GGIRread, ActCR and read.gt3x to imports.

# CHANGES IN GGIR VERSION 3.0-8

- Part 1: In the handling of externally derived epoch data, the code and algorithm for nonwear detection is now simplified to better match expected behaviour #1080.

- Part 2: Fixed issue where g.convert.part2.long() was throwing an error when attempting to process data where every day had insufficient number of valid hours #1070.

- Part 5: Fix bug introduced with 3.0-7 causing WW window to not handle well scenario of zero windows #1078.

# CHANGES IN GGIR VERSION 3.0-7

- Part 1:

  - Fix bug introduced with release 3.0-6 affecting the use of external function embedding #1065

  - Need to specify rmc.firstrow.acc when working with ad-hoc csv file format now better documented and read.myacc.csv now produces an error when used directly and user forgets to specify rmc.firstrow.acc #1034.

  - Improve g.calibrate to better handle scenario when no non-movement periods are found in the entire recording #1032

- Part 3: Algorithm HDCZA simplified by replacing time series specific threshold in step 6 of the description in the 2018 paper by a constant threshold that can be set by the user. This means that
we have a new parameter HDCZA_threshold and parameter constrain2range is now deprecated #1062.

- Part 3: Added option for HSPT.ignore.invalid = NA, which would consider invalid
time segments as no movement for the Sleep Period Time definition.

- Part 5: Fix bug in MM = timewindow part specific to when first night(s) are not available in part 4 results. #1039.


# CHANGES IN GGIR VERSION 3.0-6

- Part 2, 4, 5: Add parameter dec_reports and dec_config to ease tailoring GGIR to non-default UK/US machines #1048.

- Part 2, 4, 5: Force language to "C" (UK/US English) to avoid issues on computer configured differently #1047.

- Part 5: Behaviour parameter includedaycrit.part5 changed for values above 1, these
are now interpreted as minimum number of valid waking hours during waking horus of a day.
If you prefer to keep old functionality then divide your old value by 24 #1050.

- Part 1:

  - Improved readability and maintainability of the code #1027

  - Improved processing speed for Axivity .cwa, GENEActiv .bin, and Movisens files

  - Made sure that g.readaccfile() reads timestamps in the correct timezone, configtz, for all monitor types

  - Note: there will be small differences in both metalong and metashort metrics calculated by this GGIR version, compared to prior versions. This is due to small improvements in the management of timestamps, calibration coefficients, and input data block boundaries.

  - Fix handling of ad hoc csv file header in g.inspectfile() #1057

  - Improved g.calibrate to better handle scenario when no non-movement periods are found in the entire recording #1032

  - Improved documentation for the need to specify rmc.firstrow.acc when working with ad-hoc csv file format and read.myacc.csv now produces an error when used directly while user forgets to specify rmc.firstrow.acc #1034.


# CHANGES IN GGIR VERSION 3.0-5

- Part 5: Fix bug in functionality for Sensewear data (externally derived epoch data) #1030

- Part 1: For externally derived epoch data in dataFormat actiwatch_csv, actiwatch_awd, actigraph_csv, and sensewear_xls the non-wear detection is no longer done based on a hard-coded 60 minute rolling window but the window length is now modifiable with the third value of argument windowsizes (in seconds) as also used for raw data. #1026

- Part 5: Fix issue with defining days specific to when timewindow is MM, recordings starts at midnight and both first and last days are incomplete #1029

# CHANGES IN GGIR VERSION 3.0-4

- Part 5: Improved handling of inconsistent number of columns in part 5 milestone data #1002

- Part 3: Revised NotWorn option for argument HASPT.algo as used for count data #1011

- Visualreport: Now able to handle recordings with no valid days in combination with argument visualreport_without_invalid = TRUE, fixes bug #1016

- Part 2 and 5: Timing of LX is now expressed on scale between 12 and 36 to allow for meaningful person level summary of this value, fixes #1012

- Facilitate handling of Sensewear xls file format with externally derived epoch data #974

- Part 3: If argument sensor.location is set to hip then use longitudinal axis detected in part 2 instead of estimating it again with a different algorithm #1024

- Part 5: If argument sensor.location is set to hip then store and use longitudinal angle and possible other angles in part 5 time series instead of anglez #1024.

# CHANGES IN GGIR VERSION 3.0-3

- Part 2: Fix bug where data_quality_report.csv contained incorrect filehealth[...] values whenever some of these values were supposed to be blank #1003

- Part 1: Fix bug where on machines with GMT timezone and R >= 4.3.0, for GENEActiv .bin files, the starting timestamps of M\$metalong and M\$metashort were truncated to midninght #1000

- Part 5: Fix bug where part5 output showed duplicate rows when argument dayborder is set to nondefault value #1006

# CHANGES IN GGIR VERSION 3.0-2

- Part 2: Fix bug that caused part 2 to struggle with corrupt ActiGraph .gt3x files #972

- Part 2: Redefine horizontal axis of plots_to_check_data_quality #983

- Documentation: Expanded documentation on desiredtz, configtz, and time stamp format in part 5 time series #966

- Part 1: Now also able to handle some more variations in Actigraph csv count file format #978, and automatically aggregates to lower resolution if short epoch size is longer than observed epoch size in actigraph count csv.

- Part 5: Reverting decision to prohibit segmentDAYSPTcrit.part5 to be c(0, 0). The default remains unchanged and documentation now only emphasizes the downside of using c(0, 0). #980

- Argument documentation: Fixing series of typos (thanks to Pieter-Jan Marent for pointing them out)

- Part 5: Fix bug in recently added functionality for studying overlap between sibs and self-reported behaviours #989.

- Part 1:Enable use of temperature data in adhoc-csv format throughout GGIR #991

- Part 1: Fix bug that caused a mismatch between IDs and filenames in part 1 when movisens participant folders did not contain the acc.bin file #994.


# CHANGES IN GGIR VERSION 3.0-1

- Part 2: Added parameters study_dates_file and study_dateformat to specify csv file with start and end date when accelerometer is expected to be worn #943

- Part 5: Now stores dictionary for variable names in part 5 reports #938

- Part 2: Bug fix in csv report generation when appending part 2 milestone data with differing number of columns #951

- Part 1: Bug fix in gt3x data reading by avoiding duplication of first second when appending chunks of data #952

- Part 1 + 2: Log information regarding number of time gaps and total time imputed with function g.imputeTimeGaps for ad-hoc csv and ActiGraph data in line with previously added similar functionality for Axivity cwa #571

- Part 6: Added new part 6 with for now only Household co-analysis and Cosinor analysis.

# CHANGES IN GGIR VERSION 3.0-0

- Part 1 and 2: Change default value for nonwear_approach to "2023" which labels the entire window as nonwear if conditions are met. This instead of only the middle 15 minutes as is the case for "2013" setting. Further, setting "2023" now uses a 5 Hertz version of the signals for non-wear detection, for clipping detection the code uses the original signal.

- Part 2: Move cosinor analysis code to its own function in order to ease re-using it in both part 2 and part 6.

- Part2: Expand cosinor analysis with R2

- Part 2: Arguments hrs.del.start and hrs.del.end when combined with strategy = 3 and strategy = 5 now count
relative to start and end of the most active time window as identified.  #905

- Part 5: Change default for segmentDAYSPTcrit.part5 from c(0,0) to c(0, 0.9) and now prohibiting the use of c(0, 0) as it gives biased estimates. We knew this, but some users started using the default without attempting to understand it, by which it seems necessary to force a sensible default. #940

- Add contributing guidelines for github.io documentation #923

- Part 5: Added optioned "OO" to argument timewindow, which defines windows from sleep Onset to sleep Onset #931

# CHANGES IN GGIR VERSION 2.10-4

- Part 4: Now better able to handle nights without sustained inactivity bouts (rest) #911

- Part 5: Function g.fragmentation code cleaned up, TP formulas revised following feedback Ian Danilevicz, and code now also produces sleep fragmentation #817

- Part 1: Fixed issue #914 relating to specifying timezone for processing ad-hoc csv format raw data.

- Part 1: Improved recognition of ActiGraph csv that occassionally confused for Axivity csv #918

- Part 1: Added argument rmc.scalefactor.acc to be used by read.myacc.csv #887

- Vignette: Expanded with paragraph on minimum recording duration #835

- Part 4: More informative error message when advanced sleep log has duplicate or missing dates #925

- Fix recently introduced bug where GGIR environment was not exported to cluster in GGIR part 1, 2, 3, and 5 #910

# CHANGES IN GGIR VERSION 2.10-3

- Part 1: Fixed minor bug in ismovisens that failed when datadir started with "./" #897

- Part 5: Now able to assess overlap between self-reported naps and nonwear
with sustained inactivity bouts in order to facilitate research on nap detection. Fixes #687

- Part 5: Time series now also exported if recording only includes one night, even though this is not sufficient for the main part 5 analyses. #894 Further, the time series now also come with lightpeak (LUX).

- Visualreport: Bug fixed extracting the numeric value of the days to exclude in g.plot5 #879

- Part 1: Fixed minor bug to keep calibration data in the data quality report after re-running parts 1 and 2 with overwrite = TRUE and backup.cal.coef = "retrieve" #896

- Part 5: Fix minor bug by which GGIR skipped the last day if measurement finishes before midnight and timewindow = MM #891

- Part 4: Added parameter sleepefficiency.metric to decide if in-bed time (denominator) should be guider_spt or acc_spt + latency #811

# CHANGES IN GGIR VERSION 2.10-2

- Part 1: Revision to readability of code (credits: Lena Kushleyeva)

- Part 1: Improved handling of corrupted .gt3x files #873

- Part 1 + 2: Bug fixed in logging file health of Axivity data

- Part 2: Addresses confusing error message for some users #888

# CHANGES IN GGIR VERSION 2.10-1

- Part 1 + 2: File health log captured by dependency GGIRread::readAxivity 
and incorporated in data_quality_report #866

- Part 1: Fix ID extraction for externally generated epoch files #869

- Sleep log: Argument nnigths is now deprecated and number of nights are detected automatically in sleep logs #856

- Sleep log: Minor fixed related to misplaced parenthesis in g.loadlog and automatic extension of sleeplog matrix if needed to avoid error #867

- General: When recording starts after midnight and before 4am, then the wake time for this first day is now derived in parts 3-4 and used in part 5 #859

- Part 1: Fix major bug to .gt3x data autocalibration (only ActiGraph), where autocalibration was done but not applied to the data #864

- Refactored code for functions g.analyse.perfile and various functions which name starts with "g.part5"

# CHANGES IN GGIR VERSION 2.9-6

- Part 1: Fix .gt3x file serial number and firmware extraction #850

- Part 5: Now able to segment days based on qwindow #815

- Part 5: Fix minor bug related to the definition of the MM window (it was biased by 1 epoch) #838

- Part 5: Now g.part5.lux_persegment is able to handle timestamp in iso8601 format #848

- Part 5: Fix minor bug by which GGIR skipped the last day if measurement starts at midnight and timewindow = MM #852

- Activity log: Fix minor bug that appeared when seconds in the activity log are not compatible with the epoch length used in GGIR #844

- Config file: Fix minor bug by ignoring the local function getExt from the config file #846

- Update authors' information in DESCRIPTION and CITATION files

- Added structure for github.io documentation page

# CHANGES IN GGIR VERSION 2.9-5

- Part 4: Now able to handle first night without any sustained inactivity bout detected #812

- Vignettes: Buchan et al. 2023 cut-points for adults included in the cut-points vignette #804

- Part 4: Identification of column names in advanced sleeplog is now case insensitive #812

- Part 1: Functionality to append recordings with the same ID, see argument maxRecordingInterval #972

- Part 1: Handling of external derived epoch data now able to handle empty lines, user can now specify format of the timestamps with argument extEpochData_timeformat, and ActiGraph count data also facilitated.

- Part 1: Deprecate functionality for file formats .wav (Axivity Ltd) .bin (Unilever Discover) as keeping this code up to date with newer R versions took a lot of time. If you are interestedin re-inserting this functionality and contributing to its ongoing maintenance then please contact us.

- Part 1: Fully deprecate function g.cwaread as it migrated to GGIRread a year ago.

- Address #834 by using POSIXct where possible and avoiding use of POSIXlt in data.frame.

# CHANGES IN GGIR VERSION 2.9-4

- Open-Source Software License for GGIR changed to Apache 2.0

- General: included functionality to allow the user select the separator in the csv files written by GGIR: reports in part 2, part 4 and part 5, data_quality_report.csv, and config.csv #770.

- General: increased flexibility to read csv files related to sleeplog, activity log, data cleaning file, and csv containing calibration coefficients, by automatically detecting any separator argument from the set of [,\t |;] with function data.table::fread #770.

- Part 4: Bug fixed in the determination of SPT when daysleeper #802

# CHANGES IN GGIR VERSION 2.9-3

- General: automatic detection of missing suggested packages such as GGIRread when code recognises that they will be needed #786.

- Vignettes: instruction expanded about when to install additional R packages #786.

- Part 2: minor bug fixed in strategy = 3 and strategy = 5 (now they consider the first ndaywindow which is equal to the max activity level) #796

- Visualreport: Argument visualreport_without_invalid added #780

- Adding keyword internal to many of the GGIR functions that are not intended for direct used by the user to reduce size of package pdf documentation and to ease navigation for the user #679

- Vignette and documentation: Adding motivation for not using term sedentary behaviour and warning that visualreport has not been optimised for quality checking the GGIR results.

- Visualreport: now it flags non-wear in the plot based on the part 2 milestone data.

# CHANGES IN GGIR VERSION 2.9-2

- Part 1: function and new algorithm for non-wear time detection which flags the full ws3 window as non-wear #645.

- Part 1: Can now handle ActiGraph csv where first timegap occurs not in the first day of the recording #784 and #790

- Vignettes: expansion of the cut-point vignette with information on activity counts-based cut-points #771.

- Reports: Part 5 rows now ordered numerically again #769.

# CHANGES IN GGIR VERSION 2.9-1

- General: warnings added to inform user that rmc.desiredtz and rmc.configtz will be deprecated #765.

- Part 1: Function g.calibrate now able to handle zero variation in temperature data #785.

- Part 1: Facilitate working with epoch level data produced by external software #756

- Part 2: strategy = 5 implemented as a variation of strategy 3 (it allows to select most active X calendar days) #768.

- Part 2: fixed bug in strategy = 3 and now it selects the acc.metric of interest #768.

- Part 3 + 4: Update date handling in SleepRegularityIndex to be compatible with latest R version #782.

- Part 5: minor bug #775 fixed in the separation of the first and second day when more than one set of thresholds is defined and both MM and WW timewindows are extracted.

- Part 5: intensity gradient is now also calculated for full window in part 5 and column names now indicate the window over the intensity gradient is calculated (i.e., "_day_" or "_day_spt_") #763.

# CHANGES IN GGIR VERSION 2.9-0

- Sleep log: fixed bug related to reading last day in advanced sleeplog.

- Reports: nights in part 4 reports are now correctly ordered.

- Documentation: improved documentation for the use of acceleration metrics

- Part 2: Functionality for storefolderstructure now consistent with part 5

# CHANGES IN GGIR VERSION 2.8-7

- Part 1: Deprecating dependency on GENEAread package, from now onward only GGIRread is used for reading GENEActiv binary data #627

- Part 2: Argument nonWearEdgeCorrection added to control whether this part of the nonwear detection algorithm is used #749.

- Minor edits to make GGIR code backward compatible with R version 3.3 again.

- Reports: GGIR now skips the csv and visualreport generation if required milestone data is not available.

- Part 5: Speed improvements to functions g.part5.addsib and g.part5.wakesleepwindow

- Part 5: Bug fixed in relation to long recordings with many days with non-wear data in part 5 #727

- Now able to handle OS Windows-friendly double backward slash filepaths for datadir, outputdir, qwindow, loglocation and data_cleaning_file arguments #570.

- verbose argument added to allow for turning off console message #652

- Relevant for GGIR developers only: GGIR can now be tested from source without GGIR needing to be installed #704

# CHANGES IN GGIR VERSION 2.8-6

- Part 2: Bug fixed that caused columns N valid week/weekend days to be omitted #747.

- Vignettes: Add cross-references between vignettes to ease navigation and link R introduction tutorial #720.

- Vignette: Section added on MX-metrics (thanks Alex Rowlands + Ben Maylor for input) #723.

- Part 1: Expand timegap imputation functionality with also imputation of the timestamps #742

- Part 1: Add configtz argument to function read.myacc.csv #740

- Part 1: Interpolate timestamps that lack decimal places #705

- Part 5: fixed minor bug when axivity light data is stored as factor in part 1.

# CHANGES IN GGIR VERSION 2.8-5

- Part 1: cwa is now detected ignoring case in file extension in g.inspectfile #731

- Part 2: Bug fixes in accounting for 25 hour days in part 2 day level analyses #728.

- Part 2: Seconds in activitylog timestamps are now handled by g.conv.actlog

- Part 2: Fixed #733 related to qwindows in the first recording day

- Part 4: argument sleeplogidnum deprecated, ID class now auto-detected.

- Part 1, 2, 4, 5: Fixes bug (#732 first half). Filenames are no longer ordered prior to processing which should make it more robust to work with raw data stratified across multiple subdirectories.

- Visual report: Now uses part 3 milestone data consistently as reference to avoid inconsistencies when part 3 milestone data files were not generated #732 (second half).

# CHANGES IN GGIR VERSION 2.8-4

- Part 1: Improved handling of the expand_tail_max_hours functionality and replacing by argument recordingEndSleepHour. Further, the expanded time is now ignored in reports and visualizations.

- Part 5: dayborder != 0 is now handled in g.part5.wakesleepwindow

- Part 5: excludefirstlast.part5 is now applied per window type in g.report.part5

- Part 2 and 5: arguments bout.metric or closedbout deprecated, g.getbout function simplified to a single algorithm.

- Part 2 and 5: Bout detection fixed such that it can now detect bouts that equal the epoch length, and for epochs less than 30 seconds do no overestimate bout length by 1 epoch.

# CHANGES IN GGIR VERSION 2.8-3

- Reports: storing nonwear in part4 reports fixed when number of nights >= 10.

- Fixed test for battery voltage in cwa file #678

- Reports: nonwear_perc_spt does only appear in reports from part4 when WW windows are available

- Vignettes: main vignette revised for undesired spaces and extended with the handling of nonwear_perc_spt in part 4 reports

- Part 4: Fixed bug #686 causing last sleeplog night to be ignored when a sleeplog night is missing

# CHANGES IN GGIR VERSION 2.8-2

- Timestamp conversion functions tidied up #661.

- Part 1: Deprecate brondcounts due to issues in activityCounts package

- Part 5: Fixes #655 (unable to handle when M5HOUR falls on midnight exactly).

- Part 5: Additional updates that I missed in 2.8-1 in POSIXlt to character conversion to be compatible with R-devel update svn revision r82904 (2022-09-24 19:32:52)

- Part5: Fixes #655 (unable to handle when M5HOUR falls on midnight exactly).

- Reports: Fixes #672 (improved speed of reports generation).

# CHANGES IN GGIR VERSION 2.8-1

- Changed POSIXlt to character conversion to be compatible with R-devel update svn revision r82904 (2022-09-24 19:32:52)

- Part 1: Added neishabouricounts metrics and dependency to actilifecounts package

- Part 2: Fixed time mismatch between metashort and activity log when the first day does not start at

- Part 2 report: Improving how f0 != 1 is handled when first recording has no data

- Part 4 report: now also able to handle longitudinal studies with repeated measurement for the same ID

- Documentation: minor fix in ShellDoc2Vignette that affected the parameters vignette midnight

- Documentation: Added information on Neishabouri counts in help files and main vignette

# CHANGES IN GGIR VERSION 2.8-0

- Documentation: New package vignette added for cut-points.

- Documentation: New package vignette added for arguments

- Documentation: GGIR function manual and parameter/argument vignette auto-integrate default values to avoid inconsistencies.

- Documentation: Main vignette sections on cut-points and input argument shortened with reference to the new vignettes.

- Documentation: Paragraph added on processing time.

# CHANGES IN GGIR VERSION 2.7-7

- Part 1: Long timegaps in ActiGraph data or ad-hoc .csv data as a result of idle sleep mode are now dealt with at metric level rather than raw data level.

- Part 1: Speed up to code by changing how GENEActiv data converted to matrix and by changing how rolling median is derived.

- Part 1" Deprecate option to read raw data from in RData files, which was connected to the selectdaysfile functionality as deprecated in previous release.

- Remove zenodo and .cff file to simplify package citation and avoid tracking by RSD (issue #636)

- Part 1: Add extra arguments to modify zero-crossing count calculation.

- Part 3: Adding ColeKripke sleep detection algorithm.

# CHANGES IN GGIR VERSION 2.7-6

- Part 1: Deprecated g.cwaread and internally replaced by GGIRread::readAxivity.

- Part 1: Deprecated g.binread and internally replaced by GGIRread::readGenea.

- Part 1: Deprecated resample and internally replaced by GGIRread::resample.

- Part 1: Added temporary option to choose between using GENEAread or GGIRread::readGENEActiv for reading GENEActiv .bin files.

- Part 1 + 2: Deprecated selectdaysfile functionality.

- Deprecated function g.getidfromheaderobject, and functionality merged with g.extractheadervars

# CHANGES IN GGIR VERSION 2.7-5

- Part 1: AX6 gyroscope data correctly ignored in g.getmeta.

# CHANGES IN GGIR VERSION 2.7-4

- Part 1: Arguments configtz and desiredtz now also used for .gt3x data.

- Part 1: Temporary copy of read.gt3x code replaced by direct dependency on read.gt3x

- Vignette: Cut-point documentation paragraph improved.

- Part 1: Mismatches in the reading of ActiGraph csv files handled (mainly because of header = TRUE resulted in some acceleration values disregarded as they were considered a header in each iteration).

- Part 1: Fixed issue #573 (code redundancy to impute time gaps in read.myacc.csv and g.imputeTimegaps)

- Part 1: g.imputeTimegaps extended so that now it can handle time gaps at the begining, at the end of the file, and time gaps from one chunk to the next one.

- Part 2: QC plot now with legend and minor updates to layout.

- Part 4: Person level aggregation now takes place per filename instead of per ID, which should prevent that identical IDs in repeated recordings get aggregated into a single ling.

- Part 4: ID matching between accelerometer file and sleep log improved by ignore spaces.

# CHANGES IN GGIR VERSION 2.7-3

- Part1: Added option to expand the last day of the recording to trigger analyses for that day, see argument expand_tail_max_hours in params_general.

- Part 1: Add extraction of ID and serial number from Movisens data

- Part 2: Corrected calendar date time stamps in daysummary report on last day of the recording.

- Part 2: Further, improvement to activity diary functionality to warn user when date format is inconsistent.

- Part 2: Missing values in activity diary are now skipped by which non-neighboring cells with valid data can define segments.

- Part 3: Improved handling of sleep that ends in the hour before noon on the last day of the recording.

- Part 4: Fixing #592 concerning using a set time window as guider for the sleep analyses

- Part 4: Fixed issue #592 which caused GGIR to incorrectly warn used about using the set time window guider functionality correctly

# CHANGES IN GGIR VERSION 2.7-2

- Part 4: Fixing timezone sensitivity for OSx in Pacific timezone for gloadlog function.

- Inform user (onAttach) when local GGIR version is behind CRAN version.

- Part 2: Enhancing activity diary functionality to warn user when date format is inconsistent and to treat cells with only a dot as missing values.

- Part 1: Fixing issue #577 concerning use of temperature data in ad-hoc csv file format

- Part 5: Fixing issue #582 affecting windows where a single fragment spans the entire window.

# CHANGES IN GGIR VERSION 2.7-1

- Fixing bug #566 concerning decision to not parallel process being made too late

- Part 4: Attempt to fix timezone sensitivity for OSx in Pacific timezone for gloadlog function.

# CHANGES IN GGIR VERSION 2.7-0

- Renaming function g.shell.GGIR to GGIR and adding new function g.shell.GGIR as a wrapper function around GGIR function to preserve functionality.

- Deprecating functionality to read GENEA bin files (not to be confused with GENEActiv), because dependency matlab is about to be archived.  Once matlab is back online we will reinsert this functionality.

- Cosinor analysis now accounts for DST (see vignette for details)

# CHANGES IN GGIR VERSION 2.6-6

- Part 2: Dependency added to ActCR to perform Cosinor and Extended cosinor analyses, see new argument cosinor.

- Part 2: g.IVIS function now able to handle missing data points, and used for cosinor analaysis

- Part 2: g.IVIS function now has option to specify acceleration threshold when using IVIS.activity.metric=2, and used for cosinor analaysis

- General: If there is only 1 file to be processed then code skips setting up parallel processing interface.

# CHANGES IN GGIR VERSION 2.6-5

- Vignette for read.myacc.csv function included.

- Parameter objects documentation moved to g.shell.GGIR.

- Part 4: Advanced sleeplog handling further improved as it now also handles dates in sleeplog far beyond recording dates.

- Part 2 report: Number of days to plot now auto scales to length of measurement.

- Part 2 report: Fixed bug introduced in 2.6-3 making it impossible to generate a report for more than 1 file and fix bug introduced in 2.6-4 messing up the merging of files.

- Part 1: Now auto-converts files with GT3X extension to gt3x as this is required for read.gt3x.

- Part 4: Removed unnecessary warning while loading sleeplog.

# CHANGES IN GGIR VERSION 2.6-4

- Part4: Bug fixed in handling of advanced sleeplog data when start of acc recording does not match start of sleeplog

- Deprecated functionality for reading GENEActiv csv files without the read.myacc.csv functionality.

# CHANGES IN GGIR VERSION 2.6-3

- Part2: Non-user facing arguments and object named BodyLocation replaced by sensor.location to be consistent with the term used in part 3.

- Minor documentation updates to cover brondcounts and to reflect that bout.metric is no longer 4 by default

- Minor change to parallel processing so that GGIR does not create a cluster larger than the number of files to process. Fixes #535

- Part 4: Fixes bug #531 re. sleeplog being ignored if first night is missing

- Part 4: Now relies more on ID extraction in part 1-3 and only attempts to extract ID from filename if that fails. Previously it extracted the ID twice.

- General: Speeded-up implementation of rounding decimal places as introduced in release 2.6-1

# CHANGES IN GGIR VERSION 2.6-2

- Part 1: Can now handle .gt3x produced by CentrePoint but for now user needs to change upercase file extension to lowercase

- Part 1: Fixed issue #520 with handling Movisens data

- Part 1: Fixed issue #523 with calibration of ActiGraph csv files

- General: Prevent redundant warnings resulting from some unnecessary checks of params objects

- Report generation per window: Fix undefined function call introduced in 2.6-1

# CHANGES IN GGIR VERSION 2.6-1

- Part 1: Fixed bug that prohibitted processing files in subfolders.

- Part 1: Dynamic range can now be extracted from Axivity .cwa files when not equal to 8g.

- Part 2: Qwindows logically ordered in reports.

- Part 2: Fixed bug #516 regarding passing on of dateformat.

- Part 4: Fixed bug introduced in 2.6-0 relating to sleeplog guider being assined even when no sleeplog used.

- Part 5: Improved handling of missing sleep estimate for first night.

- General: Fixes bug introduced in 2.6-0 relating to storage of configuration file on comma-separated machines

- General: GGIR reports are now saved with a maximum of 3 decimal places for numeric variables.

# CHANGES IN GGIR VERSION 2.6-0

- Part 1: Fixed warning when working with a vector of filenames as argument to datadir.

- Part 2: Added argument max_calendar_days to allow user to control the maximum number of calendar days to include in analysis, irrespective of argument strategy value.

- Part 2: Argument maxdur is no longer specific to strategy 1, but applied irrespective of value of argument strategy.

- Part 4: Fixed issue that sleeplog is indicated as the guider even when when corresponding timestamp cannot resolve to timestamp.

- Part 5: Time series output filenames now auto-stripped from [.]|rdata|csv|cwa|gt3x|bin character string.

- Fixed issues introduced in 2.5-6 release: maxNcores was not defined in part 1, params_output and not passed on to part 3, and params objects were unneccesarily repeatedly checked.

# CHANGES IN GGIR VERSION 2.5-6

- Internal re-structuring: Arguments are now internally passed via parameter objects to make code and code documentation easier to navigate. This should not break backward compatibility and users should be able to continue working with their older R scripts.

# CHANGES IN GGIR VERSION 2.5-5

- Fixed bug #484 affecting part 5 report not being able to generate because non-matching columns in milestone data

- Part 2: Recently introduced BrondCounts and ZeroCrosssing Counts no long auto-scaled by 1000 like all the other metrics as these are not in gravity-units.

- Part 3: Fixed warning when SRI is calculated on DST day with 23 hours

- Part 5: sibreport and timeseries now stored per sib definition.

# CHANGES IN GGIR VERSION 2.5-4

- Part 1: Now able to process modern .gt3x data with thanks to R package read.gt3x.

- Part 1: Timegaps and zeros across all three axes in ActiGraph data (.csv and .gt3x) now automatically imputed by last recorded value normalise to vector of 1.

- Adding BrondCounts as optional acceleration metric and dependency on activityCounts, and enabling Sadeh and Galland algorithms to use it for sleep estimation.

# CHANGES IN GGIR VERSION 2.5-3

- Part 1: Deprecating function g.metric as its functionality has been taken over by g.applymetrics (this has been announced to its users with a warning for over a year now).

- Part 2: Adding warning when ID cannot be extracted from file based on argument idloc.

- Part 4: Empty or incomplete sleeplog rows now better ignored

- Part 4: No longer an R warning is given when ID is missing in sleeplog, because this is common and user can already see it in csv reports.

# CHANGES IN GGIR VERSION 2.5-2

- Part 3: Now makes sure that HASPT is skipped when user configures def.noc.sleep as a set time window. This feature that probaly few people use nowadays broke with the 2.5-0 release.

- g.shell.GGIR: Now gives warning when user supplies double arguments.

- Part 4: Now warns when none of the IDs in the sleeplog could be matched with accelerometer data.

- Part 1: read.myacc.csv fixes bug with argument rmc.check4timegaps

- Part 3: Fixes #472 SRI calculation not possible when complete absence of sleep in recording

- Part 5: Experimental nap detection added to report and time series, currently only model for 3.5 year olds available.

# CHANGES IN GGIR VERSION 2.5-1

- g.shell.GGIR: Removing forced assignment of sleepwindowType argument to "SPT"

- g.shell.GGIR: Reviving setwindow option def.noc.sleep, which broke

# CHANGES IN GGIR VERSION 2.5-0

- Minor updates to function documentation and vignettes

- visualreport: Month now displayed by name rather than number to avoid confusion about format.

# CHANGES IN GGIR VERSION 2.4-3

- Vignette expanded with documentation on non-default variables in part 4 csv report

- Vignette added with a tutorial on how to perform segmented day analysis

- Part 1: The sensor fusion functionality introduced in version 2.2-2, as a bit of an experimental development, has now been removed from GGIR as added value turned out to be limited.

- Part 2: Code for ID extraction tidied up, and new idloc argument options added.

- Part 2: Activity log can now also have empty days.

- Part 2: Now also exported in long format if qwindow has length longer than 2.

- Part 3 and 4 expanded with Sleep Regularity Index.

- Part 4 number of awakenings is now also in the output.

- Part 5 LUX per segment calculation bug fixes.

- visualreport code modified such that it visualises any day with at least 30 minutes of data unless sleep could not be estimated from the corresponding night.

# CHANGES IN GGIR VERSION 2.4-2

- Part 1 now able to derive zero-crossing counts needed for Sadeh algorithm

- Part 2 function g.conv.actlog to use activity log to guide qwindow now able to tailor dateformat

- Part 3 now option to tailor SPT detection for hip data by using horizontal longitudinal angle as indicator of lying

- Part 3 code restructured to estimate SPT and SIB in separated function (HASIB and HASPT) such that g.sib.det is more readable.

- Part 3 now option to select SIB algorithm, including: vanHees2015, Sadeh1994, and Galland2012

- Part 4 now able to handle more advance sleeplog format that also contain nap and nonwear entries per date

- Part 4 report now includes guider estimates also in 'cleaned' report

- Part 4 WASO and NOA (Number of awakenings) added to output

- Part 4 Able to handle time in bed diaries/algorithms, and generates sleep latency and sleep efficiency estimate if available

- Part 5 now able to export sustained inactivity bouts and self-reported naps and nonwear to csv report to aid nap analysis

- Part 5 numeric timing of MX-metrics changed to be hour of the day

- Part 5 Lux per segment variables now turn LUX during SPT to zero

- Part 4 legend added to visualisation

- Part 5 bug fixed that caused part5 single row output to be stored as a single column

# CHANGES IN GGIR VERSION 2.4-1

- Part 1 fixed bug related to g.getmeta reading movisens files

- Part 1 resampling function expanded with option to choose interpolation technique

- Part 3 fixed bug introduced in 2.4-0 making it impossible to process long recordings with many sustained inactivity bouts

# CHANGES IN GGIR VERSION 2.4-0

- Part 5 LUX variables per segment of the day improved

- Removing calls to closeAllConnections as requested by CRAN

# CHANGES IN GGIR VERSION 2.3-3

- Part 5 fragmentation analysis added

- Part 5 report, added argument week_weekend_aggregate.part5

- Part 5 intensity gradient now also extracted here for waking hours

- Part 5 LUX variables added (beta-implementation)

- Part 2 and 5, Bout detection algorithm improved, see bout.metric 6

- Parts 1, 2, 3 and 5: Added argument maxNcores to control maximum number of cores to be used in parallel processing mode

- Part 1 fixed bug related with ENMOa calculation which made the metashort data frame to be empty

- Part 2 fixed bug to report generation in part 2

- Fixed bug related to visualization report when a file has not been processed in part 4

# CHANGES IN GGIR VERSION 2.3-2

- Part 1 when using ad-hoc data format: now able to handle timestamp column.

- Part 2 report when using ad-hoc data format with header: now able to extract and store serial number in part2 report.

- Test file generation for unit tests improved. Thanks Lena Kushleyeva.

- Part 2 estimation of longitudinal axis and related error handling improved

# CHANGES IN GGIR VERSION 2.3-1

- Part 3 bug fixed in usage of argument dayborder when not 0. Thanks Ruben Brondeel for spotting this.

- Part 3 bug fixed in recognition fraction night invalid for first night.  Thanks Ruben Brondeel for spotting.

- Part 1 fixed warning related to closing connection to bin files. Thanks John Muschelli.

- Part 2 fixed warning that deprecated IVIS argument does not exist

# CHANGES IN GGIR VERSION 2.3-0

- Part 5 bug fixed for g.part5.fixmissingnight when night is also missing in sleeplog

- Part 1 Documentation for sensor fusion functionality improved

- Part 2 csv report generation speeded up

# CHANGES IN GGIR VERSION 2.2-2

- Part 5 bug fix in waking up time for first night, in WW mode and HDCZA guider

- Various minor function expansions to documentation and vignette

- Part 2 Earlier draft implementation of IV and IS metrics now better tested and bugs fixed.

- Part 2 Default value for argument IVIS.activity.metric changed to 2.

- Migrate codecoverage testing to Github Actions

- Part 1 Implemented initial sensor fusion functionality for AX6 cwa data

# CHANGES IN GGIR VERSION 2.2-1

- Part 3 Fixes bug introduced in 2.2-0 for recordings without sustained inactivity bouts

- Transitioned from Travis+Appveyor CI to GitHub Actions

- Part 5 fixed issue with expected max number of files to process

- Enabled console display of GGIR version number when running g.shell.shell under Windows

- Embedded new GGIR logo and update layout of vignette

# CHANGES IN GGIR VERSION 2.2-0

- Part 5 improved midnights identification when the recording starts at 0am

- Part 5 fixed days misclassification for MM output when recordings starts with some non-wear days

- Documentation added to vignette for L5TIME_num and M5TIME_num

# CHANGES IN GGIR VERSION 2.1-3

- Deprecating sessionInfo storage as it caused problems for large scale parallel processing

- Part 1 fix to 1 minute time shift when recording starts at full minute.

- Improved compatibility with older .cwa file formats.

# CHANGES IN GGIR VERSION 2.1-2

- Part 5 bug fixed with day name and date allocation for daysleepers for MM report

- Fixes bug part 4 and 5 with missing nights which are not accounted for when assessing max night number.

- Fixes bug part 3 introduced in version 2.1-0 re. SPT detection for daysleepers.

- Fixes redefinition of the time windows (with "MM") in part 5 to include all recording days when the measurement starts with multiple non-wear days.

- GGIR version now displayed in console when running GGIR

- Bout metric 5 added to enable not allowing for gaps in bouts.

# CHANGES IN GGIR VERSION 2.1-1

- Part2 argument qwindow now able to handle person specific activity diary

- Intensity gradient now also extracted from long MX windows, see MX.ig.min.dur

# CHANGES IN GGIR VERSION 2.1-0

- Part 2 now able to be guided by activity log (see qwindow)

- Part 2 now stores output also in long format if day is segmented

- Actigraph serial number now extracted from the csv fileheader

- Actigraph serial number now used to assign correct dynamic range per Actigraph generation (6 and 8 g respectively). Previously 8 assumed.

- Part 2 indicator added of which axis is most correlated with itself over 24 hours, for hip data this may be indicator of vertical axis orientation.

- Part 1 bug fixed in timestamp generation that caused measurements that start exactly at the hour and 0 minutes to have a 1 minute offset in time.

- Part 1 to reduce data storage size epoch level metric values are now rounded to 4 decimal places.

- Part 4 report, issue fixed with double night entries.

- Bug fixed with Actigraph starttime recognition when machine language is not English and months are expressed in b or hh format.

- Bug fixed in accounting for daysleeper in first night ofr recording.

# CHANGES IN GGIR VERSION 2.0-2

- Visual report, fixed argument viewingwindow for g.plot5 (visualreport)

- Part 1 rmc.myacc.csv now raises warning when timestamps are not recognised

- Part 1 fixed rmc.dynamic_range not correctly passed on in 2.0-1

- Part 5 sub-function round_seconds_to_ws3new now handles missing values

# CHANGES IN GGIR VERSION 2.0-1

- Now able to read gzipped csv files

- Part 2 Fixed midnight selection being off by one sample

- Part 1 tidied up metric calculation and added a few metrics

- Part 2 Added strategy 4 (ignore data before first midnight)

- Data from movisens accelerometers can be now processe

- Part 5 timestamps in timeseries output in RData format (new since 2.0) now correctly accounts for timezone

- Part 4 now has option to only exclude the first or only the last night

# CHANGES IN GGIR VERSION 2.0-0

- Now R version 4.0 compatible

- Part 1 Clipping detection expanded: If any value in block more than 150 percent dynamic range then ignore entire block.

- Part 2 report now able to handle changing variable count due to missing data

- Part 2 L5M5 better able to handle small qwindow intervals

- Part 3 HDCZA algorithm expanded to be able to detect daysleepers

- Part 3 various improvements to qc plots.

- Part 5 now also stores full and cleaned output

- Part 5 now better handles missing days in part 4 output.

- Part 5 behavioral class SIB removed from daytime

- Part 5 time series export more user-friendly.

- Part 5 function code split up in 7 new functions.

- Part 4 + 5 argument data_cleaning_file added.

- Part 4 + 5 output variable names improved and documented in vignette

- Numunpack function moved back to c++

- Various updates to visualreport (plot5 function)

- External function embedding feature added

- We now consistently refer to ID (not id) and calendar_date, spelling was inconsistent.

- Vignette now has documentation on sleep and time-use analysis.

# CHANGES IN GGIR VERSION 1.11-1

- Metric ENMOa now facilitated for MVPA calculation

- Bug fixed with part4 daysleeper handling

- Part5 WW window calculation improved, first day now uses sleeplog or HDCZA algorithm estimate, and last day is ignored if no sleep estimates are available. This also affects csv exports by argument save_ms5rawlevels.

- Added explanation to vignette on how to use published cut-points.

- Axivity AX6 (acc + gyro) in cwa format now supported for file reading, actually using the gyro data for feature calculation is future work.  In the mean time, gyro signal will be ignored by the rest of GGIR.

- Axivity AX3 acc data in cwa format can now also be read if dynamic range is not 8g. Previously this was not possible.

- Fixed bug related to visual report generation when qwindow is set to non-default value.

- Added way to handle Actigraph files which start with several days of zeros which complicates the auto-calibration.

- Default for desiredtz to timezone of machine.

# CHANGES IN GGIR VERSION 1.11-0

- Fixed bug that emerged in previous version with GENEActiv .bin data not being processed by g.getmeta for some files.

- read.myacc.csv is now able to resample data with irregular sample rates and handle timestamps in character format.

- function resample can now handle any matrix size, previous only 3 columns.

- Fixed bug when using multiple non-angle metrics in part1 and trying to calculate 1to6am average metric value in part 2.

- Expanded Actigraph date format recognition ability.

- visualisationreport (function g.plot5) enhanced with colour coding for activity classes.

- Fixed bug in sleep period time recognition for first day of measurement.

# CHANGES IN GGIR VERSION 1.10-7

- Fixed functionality to supply calibration coefficients file to backup.cal.coef.

- Fixes OSx flavor not being released on CRAN in previous version.

- Upgrades to foreach loop to ease package maintenance

- Documentation part4 expanded to clarify difference between full and cleaned report.

- Non-wear detection now possible at 1 minute resolution, previously 5 minute.

- Function read.myacc.csv now able to utilize 3rd party wear detection.

# CHANGES IN GGIR VERSION 1.10-5

- Device serial number recognition in Axivity cwa files fixed

- New GitHub release, because previous version did not install.

# CHANGES IN GGIR VERSION 1.10-4

- Fixed bug introduced in version 1.10-1 in the conversion from numeric to character sleep times

- Dependencies of dependencies removed from the DESCRIPTION file

- Fixed 1to6am variables, which was wrong in version 1.10-1

- Added functionality to handle accelerometer data from any accelerometer brand stored in csv files via read.myacc.csv. Pass on the arguments of this function to g.shell.GGIR to use this functionality.

- Added reference to new GGIR paper to the documentation

# CHANGES IN GGIR VERSION 1.10-1

- Configuration file option now added to g.shell.GGIR and documented in vignette

- metric lfen added (low-pass filter signels followed by Euclidean norm)

- issues fixed with passing on of hb and lb arguments

- argument backup.cal.coef can now also handle data_quality_reports.csv files

- part 1 now automatically uses previously generated calibration coefficients if the datafile was previously processed, see documentation g.part1 for further details.

- Enabled multiple values for argument winhr, by which part2 can now calculated for example L3M3, L5M5, L6M6, L10M10 all in one go. Further, option added (qM5L5) to extract percentiles (quantiles) from the value distribution corresponding to these windows.

- Moved IVIS calculation to seperate function, and split up function g.analyse.

- Now possible to specify time windows that need to be ignored for imputation, see TimeSegments2ZeroFile.

- Default value for argument mode changed from mode = c(1,2) to mode = 1:5, to perform all the parts.

- Checks added for user write and read access permission, and subsequent warnings given..

- Parts 1, 2, 3, and 5 can now use multi CPU cores which speeds up the processing.

- Argument minimumFileSizeMB added to g.part1 to aid filtering out too small data files.

# CHANGES IN GGIR VERSION 1.9-2

- Added functionality to work with studies where accelerometer is configured in one timezone and used in other timezone. Only functional for AX3 cwa data at the moment.  See argument 'configtz'.

- Sleep estimation is now skipped if a day only has one sustained inactivity bout

- Arguments ignorenonwear default value changed to TRUE and def.noc.sleep default changed to 1 in line with literature.

- Fixed AX3 csv format starttimestamp recognition

- Part5 csv export now also includes class labels (previously only class numbers).

# CHANGES IN GGIR VERSION 1.9-1

- Fixed part5 output midnight-midnight window when monitor not worn during first days.

- Fixed assumption that when using argument idloc=2 the ID has a letter at the end, and automatically removes the last value in the index. The code now first checks for this assumption.

- Update vignette with a more elaborate explanation of the optional arguments to g.shell.GGIR.

# CHANGES IN GGIR VERSION 1.9-0

- functionality storefilestructure should now store filestructure in output of part 2, 4 and 5.

- filelocationkey.csv that was previously written by storefilestructure was redundant and removed.

- sessioninfo storage improved.

- Fixed bug that caused part2 to provide incorrect window specific estimates on first day.  of measurement if day is incomplete (not 23, 24 or 25 hours).

- Calibrate function now better able to handle files with more than a week of data, where auto-calibration struggles to find enough sphere data in the first week.

- Fixed part5 output midnight-midnight window when monitor not worn during first days.

# CHANGES IN GGIR VERSION 1.8-1

- Part4 handling of clocktime 9am corrected in addition to fix from version 1.7-1.

- Fixed bug for Actigraph csv header recognition when column 2 and 3 are NA (prevented processing before)

- Fixed bug in g.report.part5 in the calculation of the total number of valid days per person.

- Fixed bug that caused part5 to struggle with timezones west of greenwhich time.

- desiredtz added as explicit argument to g.inspectfile, g.cwaread, and g.dotorcomma.

- Fixed bug in pageindexing in g.readaccfile when machine runs out of memory.

- Fixed bug in pageindexing in Actigraph csv files (10 rows in raw data skipped every block (day)) of data.

- Added more informative warning message in g.report.part2 if file cannot be read.

- Fixed bug in scenario when person is daysleeper and wakingup time occurs before noon, and 12 hours too early.

- Fixed bug re. storefolderstructure=TRUE causing 2 variables to drop in g.report.part4 if storefolderstructure=TRUE.

- g.intensitygradient enabled to handle absense of data.

- tidied up some of the redundant or even confusiong information printer to the console

# CHANGES IN GGIR VERSION 1.7-1

- Fixed order of Nbouts output in g.part5 was not consistent with bout duration.

- Functionality added to read AX3 Axivity csv files that have the following characteristics: Raw data in g-units, not resampled, and with timestamps stored in the first column.

- Fixed bug that caused the date of the last measurement day in part5 output to be one day ahead if argument dayborder=12.

- Part5 struggled to process measurements with more than 40 days, now fixed.

- g.getstarttime can now also handle dates in Actigraph csv-file headers that are dot separated, e.g. 20.05.2016, before it only handled 20-05-2016 and 20/05/2016.

- Part4 handling of clocktime 9am corrected.

- Filename identification in part5 improved when saving raw level data.

- Fixed ability to read wav file header when shorter than expected.

- Fixed issue with CWA read functionality causing some files not to be completely read

# CHANGES IN GGIR VERSION 1.6-7

- Link with Zenodo for doi generation removed.

- Broken url fixed in vignette.

# CHANGES IN GGIR VERSION 1.6-1

- Report part 4: Count of available nights with accelerometer data fixed

- Report part 4: NA and NaN values replaced by empty cells like in other reports

- Intensity gradient analysis added to part2 output according to Rowlands et al.  MSSE 2018, doi: 10.1249/MSS.0000000000001561

- Documentation on part4 output variables improved.

- Providing incorrect value of sleeplogidnum in part4 should now provide a more informative error message

- Added functionality to handle timestamps that start with the year.

# CHANGES IN GGIR VERSION 1.6-0

- Fixed timezone dependency of g.analyse (affected only order of columns), g.part5 (affected time detection), and consequently test_chainof5parts.R

- Read functionality for Actigraph csv files speeded up by replacing read.csv by data.table::fread

- Argument qwindow is now able to handle input vectors longer than 2 and will derive all part2 variables for each time window that can be defined from the values of qwindow, e.g. value =c(0,8,24) will analyse the windows: 0-8, 8-24 and 0-24 hour.

- Argument L5M5window deprecated because argument qwindow now defines the window over which L5M5 analysis is performed.

- Argument winhr is now reflected in g.analyse/g.part2 output variable names, previously this variable name was hardcoded as L5M5, even if winhr was not 5.

- Part2 output variable names updated to be more consistent in structure and more explicit about the timewindow over which they are calculated. The variables that were calculated over the full recording (using diurnal normalization) now have the extension "fullRecording", this in contrast to the variables that are only calculated from measurement days with 'enough' valid data.

- Fixed calculation of N valid WEdays and Nvalid WKdays in part2 that was wrong since version 1.5-21. It counted all days and did not exclude days with insufficient amount of data.

- Fixed warning message in test of g.part5.

- output variable acc_timeinbed renamed to acc_SptDuration to avoid confusion with terminology used in supporting papers, which are about SPT (sleep period time) detection and not about time in bed detection.

- output variable acc_dur_noc renamed to acc_SleepDurationInSpt to improve clarity of variable name relative to acc_SptDuration

# CHANGES IN GGIR VERSION 1.5-24

- Variable names ENMO accidentatly disappeared from g.analyse output in 1.5-23, this has now been reversed.

- Fixed issue in g.part5 in handling the last day of measurement when using 'MM' windows and dayborder not equal to 0 (midnight) sometimes resulting in the last day longer than 25 hours.

- Fixed error message in g.part5 that occurs when calculating L5M5 when day is shorter than L5M5 time window.

# CHANGES IN GGIR VERSION 1.5-23

- Unit tests speeded up by using a smaller test file.

# CHANGES IN GGIR VERSION 1.5-22

- g.part4 bug fixed that was introduced in version 1.5-21 regarding the handling of daysleepers (people who wake up after noon) causing sleep estimates to be zero.

# CHANGES IN GGIR VERSION 1.5-21

- g.part5 is now able to generate summary from all measurment days (thanks Jairo).

- MM results in g.part5 now correctly stores onset and waking for single date per row.

- Various checks added to g.part4 to ensure measurements in all shapes and size can be processed.

- minor improvements to documentation g.part5

- size of example data and vignette images reduced to reduce package size

# CHANGES IN GGIR VERSION 1.5-18

- Improved handling of day saving time in g.part3 and g.part4 within the two nights in a year when the clock changes (DST was already correctly handled in the rest of the year)

- Algorithm HDCZA is now the default algorithm to use, if a sleeplog is used and an entry is missing for a particular night.

- pdf generation in part 3 is now optional (argument do.part3.pdf), this may be useful for slightly speeding up data processing as it takes a few second to generate the plot.

- test added for g.getbout function.

- bug fixed in timestamp recognition for object timebb in function g.part5

# CHANGES IN GGIR VERSION 1.5-17

- SPT-window detection now updated with a constrained threshold to make it more robust against between accelerometer brand differences. This is the approach used for our PSG in <https://www.biorxiv.org/content/10.1101/257972v1>

# CHANGES IN GGIR VERSION 1.5-16

- cwaread issue #57 fixed

- SPT-window detection included (work in progress)

- cpp code fixed which did not compile anymore

- a machine specific function test removed following feedback from CRAN maintainers

# CHANGES IN GGIR VERSION 1.5-12

- Fixed bug introduced in 1.5-1: large window size of 3600 seconds was accidentally hardcoded when the g.readaccfile function was added to GGIR in version 1.5-1

- Codecov testing added and badge added to the README file

- Functions added to create dummy accelerometer file (csv) and dummy sleeplog (csv), needed for testing

- Bug fixed in g.wavread to recognize .wav extension file header for files with alternative header size

- Default IVIS_epochsize_seconds parameter updated from 30 to 3600

- g.part1 messages on the consolo are now condensed printing only the number of the blocks loaded separated by spaces rather than new lines

- Split g.part5 function into multiple smaller functions

- Replace hard-coded "Europe/London" in g.part5 by desiredtz, to make the function work for users outside the UK

- Data frame output from g.part5 is now tidied up by removing empty rows and columns generated

- Calculation of mean amplitude deviation (MAD) is now implemented in g.part1 by the argument do.mad

- Percentiles and levels in g.part2 are now calculated for all the acceleration metrics selected

- g.part3, g.part4 and g.part5 are now independent on metric ENMO to work, argument acc.metric allows the user to select which metric to use to describe behavior

- Argument dayborder is now included in g.part5 to consider the whole measurement in case the protocol starts after midnight

- Jairo Migueles added to list of contributors

# CHANGES IN GGIR VERSION 1.5-10

- Date format recognition improved for Actigraph csv files

# CHANGES IN GGIR VERSION 1.5-9

- g.inspectfile now also functional with cwa data

- option added to enforce dynamic range with argument dynrange

- vignette expanded

# CHANGES IN GGIR VERSION 1.5-7

- vignette expanded

- Bugs fixes in relation to new cwa-read functionality c++ routine registration

- Documentation added for all underlying functions

# CHANGES IN GGIR VERSION 1.5-3

- vignette added

- Bugs fixes in relation to new cwa-read functionality

- Bugs fixes in correct number of days recognition in part5

# CHANGES IN GGIR VERSION 1.5-1

- Removed teLindert2013 metric, because it was not used and not verified

- Split g.getmeta function into multiple smaller functions

- Added IS and IV variables to g.analyse (still in explorative version)

- bug fixed related to wav file read errors

- function g.cwaread added (credits to E. Mirkes) for reading Axivity .cwa-format data. g.shellGGIR will automatically use this function when input data has .cwa extension

- bug fixed related to for GENEactiv starttime recognition which was introduced in version 1.2-11

- g.part5 documentation added on its output

# CHANGES IN GGIR VERSION 1.4

- bug fixed in functionlity to process only specific days in measurement (credits to J Heywood)

- bug fixed in midnight recognition in g.part5

- improvement to handling of measurements that start a few minutes before midnight (credits to E Mirkes)

- bug fixed related processing files shorter than 1 day, introduced in previous version

- documentationa added for Axivity wav-format data

- start made with implementing code testing functionality using testthat and covr

- documentation improved for argument def.noc.sleep in function g.part4

# CHANGES IN GGIR VERSION 1.3-2

- g.part5 added. g.part5 merges the output from the first four parts

- Functionality added to read Axivity wav-format files with acceleration in first three channels. No documentation added yet until I have more confirmation that it works well

# CHANGES IN GGIR VERSION 1.2-11

- Bug fixed related to MVPA variable. The bug was a result of the updates in version 1.2-10

# CHANGES IN GGIR VERSION 1.2-10

- Changed function argument 'mvpa.2014' into 'bout.metric' across the package in preparation for a central defintion of bouts for future GGIR version which will not only provide bout calculations for MVPA but also for inactivity. Further, I added function g.getbout to improve transparency about the bout calculations

- Updated documentation for function g.analyse to clarify different bout metric definitions

- Improvements to the functionality to only process specific days from a long accelerometer file using the argument selectdaysfile

- Timestamps are now in ISO 8601 format. I have updated the code such that it can still handle old timestamp format, but newly processed files will produce timestamps in the ISO 8601 format. The practical difference is that it will include a numeric timezone indicator.

- Bugs fixed in data selection in g.getmeta function. In the old code it tended to drop the last 30-45 minutes of a file.

- Added more optional features to be generated by g.getmeta, including rolling medians of the acceleration signals.

# CHANGES IN GGIR VERSION 1.2-8

- Updated documentation for function g.analyse to clarify different mvpa bout definitions

- mvpa.2014 = TRUE turned back on again (was disable in last version)

# CHANGES IN GGIR VERSION 1.2-7

- mvpa.2014 TRUE/FALSE was swapped, FALSE is now the default

- mvpa.2014 = TRUE disabled

# CHANGES IN GGIR VERSION 1.2-6

- Modified warning message in relation to the change in MVPA bout defintion

# CHANGES IN GGIR VERSION 1.2-5

- Accelerometer non-wear time is now also reported in the output of part 4

- Part 1 is now able to only process specific days of a measurement via argument selectdaysfile. This is useful when measurement lasts for a week and the participant is instructed to only wear the accelerometer on one or two specific days.

- Argument mvpa.2014 and closedbout added to function g.analyse. The calculation of MVPA (moderate and vigorous physical activity) has been available since 2014.  This calculation has been improved, but the user has the option to continue using the old calculation.

# CHANGES IN GGIR VERSION 1.2-2

- Bug fixed in the loading of data files with (very) large amounts of data

- Bug fixed in starttime allocation for measurements starting in the 15 minutes before midnight

# CHANGES IN GGIR VERSION 1.2-1

- Literature reference for sleep detection updated

- Argument backup.cal.coef now with improved feedback if something goes wrong

- Report generation for part 4 much faster now

- Bug fixed in part 4 in assignment of dayname when a person sleeps during the day

- g.shell.GGIR now capable of handling minimal input argument specifications

- Console output from part 3 and 4 more compact now

# CHANGES IN GGIR VERSION 1.2-0

- Package expanded with functions for detecting sleep (g.part3 and g.part4)

# CHANGES IN GGIR VERSION 1.1-5

- Additional bugs fixs related to dealing with csv-format data from the Actigraph accelerometer brand

- g.part2 now also stores its output as milestone data just like g.part1. This to facilitate parallel processing of large amounts of data files on clusters.

- The orginal report generation functionality in g.part2 has now been moved to shell function g.shell.GGIR because part3, 4 and 5 which are scheduled for later this year will combine milestone data from multiple analysis parts. It therefore, is more logical to control all report generation from the top level in the function hierarchy (g.shell.GGIR).

- g.part1 now comes with the option to provide backup calibration coefficients in case auto-calibration is unsuccessful. This function is primarily designed for studies with short lasting experiments.

- g.part2 now has the option to export epoch values to a csv-file. Note that these same epoch values are also stored in the .RData milestone file from part2.  The export option is mainly to ease access to epoch level data outside the R environment.

- g.shell.GGIR now offers the option to overwrite previously generated milestone data with variable 'overwrite'. The default setting (FALSE) is still to skip previously analysed files, which is intended to avoid having to do the same analyses twice after an interruption. However, overwriting previously generated milestone data could be useful when modifications are being made to the input arguments.

- g.shell.GGIR now offers the option to record the folderstructure in which an accelerometer file is located, especially useful for studies where accelerometer files are stored hierarchally in line with the study design.

# CHANGES IN GGIR VERSION 1.1-4

- Additional bugs fixs related to recognising data format in Actigraph data

- Angle variables are now extracted based on 5 second rolling median as opposed to 501 sample rolling median

# CHANGES IN GGIR VERSION 1.1-3

- Package expanded with functions: g.part1, g.part2, and g.shell.GGIR.  These shell functions should help movement scientists to utilize the package without too much prior knowledge about R

- Additional bugs fixs related to recognising data format in Actigraph data

- Package expanded with axis angle metrics

- Package expanded with metric for replicating teLindert2013 counts, see g.getmeta

- Package expanded with metric ENMOa, see function tutorial g.getmeta

# CHANGES IN GGIR VERSION 1.0-6

- Bug fixed related to recognising date format in csv-file header from Actigraph accelerometer brand

- Literature reference added to function g.calibrate

- function g.getmeta expanded with argument 'chunksize'

# CHANGES IN GGIR VERSION 1.0-4

- Implemented functionality for csv-fromat data from GENEActiv and Actigraph.  It seems to work for the test files I have, more testing may be necessary

- Cleaned up some of the NaN and NA output to aim for consistent annotation of missing data

# CHANGES IN GGIR VERSION 1.0-3

- Fixed bug in modified g.analyse in version 1.0-2

# CHANGES IN GGIR VERSION 1.0-2

- Expanded g.analyse with estimates of time spent in moderate and vigorous activity (a construct popular among physical activity researchers)

- Re-named a number of variables in the output from g.analyse to be more friendly for re-use in stata or sas. The majority of variable names are now shorter and do not include spaces, dots or commas

# CHANGES IN GGIR VERSION 1.0-1

- Fixed Linux-Windows sensitivty in g.getmeta. Certain damaged files can only be read with mmap.load set to FALSE in package GENEAread. Function g.getmeta in GGIR catches this problem and turns mmap.load to FALSE if necessary. This catch worked well under Linux, but not for R in Windows.  I have now fixed this

# CHANGES IN GGIR VERSION 1.0-00

- Added examples

- Expanded documentation for function g.analyse

- Fixed bug in extraction of starttime that caused the starttime to be truncated to 00:00:00 in a fraction of measurements.

- Fixed bug in extract of temperature in function g.calibrate

- Deleted three explorative variables that were only extracted in g.analyse if argument doangle was set to TRUE in function g.getmeta.  A number of bugs and the lack of referable journal publications made me decide to remove these variables while working on them. I intend on re-releasing these variables during the course of 2014. Please contact me if this causes you problems

# CHANGES IN GGIR VERSION 0.6-17

- This was the first version

