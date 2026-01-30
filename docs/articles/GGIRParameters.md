# GGIR configuration parameters

**NOTE: If you are viewing this page via CRAN note that the main GGIR
documentation has been migrated to the [GGIR GitHub
pages](https://wadpac.github.io/GGIR/).**

The GGIR [shell
function](https://cran.r-project.org/package=GGIR/vignettes/GGIR.html#23_GGIR_shell_function)
takes the input arguments and groups them into parameter objects. The
first section below displays all optional GGIR input argument names, the
GGIR part (1, 2, 3, 4 and/or 5) they are used in, and the parameter
object they are stored in. As you will see, a few parameters are not
part of any parameter object because they are direct arguments of the
GGIR shell function.

In the [second section of this vignette](#default-argument-values) you
will find a description and default value for all the arguments.

## Input arguments/parameters overview

| Argument (parameter) | Used in GGIR part | Parameter object |
|----|----|----|
| datadir | 1, 2, 4, 5 | not in parameter objects |
| f0 | 1, 2, 3, 4, 5 | not in parameter objects |
| f1 | 1, 2, 3, 4, 5 | not in parameter objects |
| windowsizes | 1, 5 | params_general |
| desiredtz | 1, 2, 3, 4, 5 | params_general |
| overwrite | 1, 2, 3, 4, 5 | params_general |
| do.parallel | 1, 2, 3, 5 | params_general |
| maxNcores | 1, 2, 3, 5 | params_general |
| use_trycatch_serial | 1, 2, 3, 5, 6 | params_general |
| myfun | 1, 2, 3 | not in parameter objects |
| outputdir | 1 | not in parameter objects |
| studyname | 1 | not in parameter objects |
| chunksize | 1 | params_rawdata |
| do.enmo | 1 | params_metrics |
| do.lfenmo | 1 | params_metrics |
| do.en | 1 | params_metrics |
| do.bfen | 1 | params_metrics |
| do.hfen | 1 | params_metrics |
| do.hfenplus | 1 | params_metrics |
| do.mad | 1 | params_metrics |
| do.anglex | 1 | params_metrics |
| do.angley | 1 | params_metrics |
| do.angle | 1 | params_metrics |
| do.enmoa | 1 | params_metrics |
| do.roll_med_acc_x | 1 | params_metrics |
| do.roll_med_acc_y | 1 | params_metrics |
| do.roll_med_acc_z | 1 | params_metrics |
| do.dev_roll_med_acc_x | 1 | params_metrics |
| do.dev_roll_med_acc_y | 1 | params_metrics |
| do.dev_roll_med_acc_z | 1 | params_metrics |
| do.lfen | 1 | params_metrics |
| do.lfx | 1 | params_metrics |
| do.lfy | 1 | params_metrics |
| do.lfz | 1 | params_metrics |
| do.hfx | 1 | params_metrics |
| do.hfy | 1 | params_metrics |
| do.hfz | 1 | params_metrics |
| do.bfx | 1 | params_metrics |
| do.bfy | 1 | params_metrics |
| do.bfz | 1 | params_metrics |
| do.zcx | 1 | params_metrics |
| do.zcy | 1 | params_metrics |
| do.zcz | 1 | params_metrics |
| do.neishabouricounts | 1 | params_metrics |
| actilife_LFE | 1 | params_metrics |
| lb | 1 | params_metrics |
| hb | 1 | params_metrics |
| n | 1 | params_metrics |
| do.cal | 1 | params_rawdata |
| spherecrit | 1 | params_rawdata |
| minloadcrit | 1 | params_rawdata |
| printsummary | 1 | params_rawdata |
| print.filename | 1 | params_general |
| backup.cal.coef | 1 | params_rawdata |
| rmc.noise | 1 | params_rawdata |
| rmc.dec | 1 | params_rawdata |
| rmc.firstrow.acc | 1 | params_rawdata |
| rmc.firstrow.header | 1 | params_rawdata |
| rmc.col.acc | 1 | params_rawdata |
| rmc.col.temp | 1 | params_rawdata |
| rmc.col.time | 1 | params_rawdata |
| rmc.unit.acc | 1 | params_rawdata |
| rmc.unit.temp | 1 | params_rawdata |
| rmc.origin | 1 | params_rawdata |
| rmc.header.length | 1 | params_rawdata |
| rmc.format.time | 1 | params_rawdata |
| rmc.bitrate | 1 | params_rawdata |
| rmc.dynamic_range | 1 | params_rawdata |
| rmc.unsignedbit | 1 | params_rawdata |
| rmc.desiredtz | 1 | params_rawdata |
| rmc.sf | 1 | params_rawdata |
| rmc.headername.sf | 1 | params_rawdata |
| rmc.headername.sn | 1 | params_rawdata |
| rmc.headername.recordingid | 1 | params_rawdata |
| rmc.header.structure | 1 | params_rawdata |
| rmc.check4timegaps | 1 | params_rawdata |
| rmc.col.wear | 1 | params_rawdata |
| rmc.doresample | 1 | params_rawdata |
| imputeTimegaps | 1 | params_rawdata |
| dayborder | 1, 2, 5 | params_general |
| dynrange | 1 | params_rawdata |
| nonwear_range_threshold | 1 | params_rawdata |
| configtz | 1 | params_general |
| minimumFileSizeMB | 1 | params_rawdata |
| interpolationType | 1 | params_rawdata |
| expand_tail_max_hours | deprecated | params_general |
| recordingEndSleepHour | 1 | params_general |
| maxRecordingInterval | 1 | params_general |
| recording_split_times | 1 | params_general |
| recording_split_overlap | 1 | params_general |
| recording_split_ignore_edges | 1 | params_general |
| recording_split_timeformat | 1 | params_general |
| nonwear_approach | 1 | params_general |
| dataFormat | 1 | params_general |
| extEpochData_timeformat | 1 | params_general |
| metadatadir | 2, 3, 4, 5 | not in parameter objects |
| minimum_MM_length.part5 | 5 | params_cleaning |
| strategy | 2, 5 | params_cleaning |
| hrs.del.start | 2, 5 | params_cleaning |
| hrs.del.end | 2, 5 | params_cleaning |
| maxdur | 2, 5 | params_cleaning |
| max_calendar_days | 2 | params_cleaning |
| includedaycrit | 2, 5 | params_cleaning |
| nonWearEdgeCorrection | 2 | params_cleaning |
| nonwearFiltermaxHours | 2 | params_cleaning |
| nonwearFilterWindow | 2 | params_cleaning |
| L5M5window | 2 | params_247 |
| M5L5res | 2, 5 | params_247 |
| winhr | 2, 5 | params_247 |
| qwindow | 2 | params_247 |
| qlevels | 2 | params_247 |
| ilevels | 2 | params_247 |
| mvpathreshold | 2 | params_phyact |
| boutcriter | 2 | params_phyact |
| ndayswindow | 2 | params_cleaning |
| idloc | 2, 4 | params_general |
| do.imp | 2 | params_cleaning |
| storefolderstructure | 2, 4, 5 | params_output |
| epochvalues2csv | 2 | params_output |
| do.part2.pdf | 2 | params_output |
| sep_reports | 2, 4, 5 | params_output |
| dec_reports | 2, 4, 5 | params_output |
| sep_config | 1, 2, 3, 4, 5 | params_output |
| dec_config | 1, 2, 3, 4, 5 | params_output |
| mvpadur | 2 | params_phyact |
| bout.metric | 2, 5 | params_phyact |
| closedbout | 2 | params_phyact |
| IVIS_windowsize_minutes | 2 | params_247 |
| IVIS_epochsize_seconds | 2 | params_247 |
| IVIS.activity.metric | 2 | params_247 |
| iglevels | 2, 5 | params_247 |
| TimeSegments2ZeroFile | 2 | params_cleaning |
| qM5L5 | 2 | params_247 |
| MX.ig.min.dur | 2 | params_247 |
| qwindow_dateformat | 2 | params_247 |
| anglethreshold | 3 | params_sleep |
| timethreshold | 3 | params_sleep |
| ignorenonwear | 3 | params_sleep |
| HDCZA_threshold | 3 | params_sleep |
| acc.metric | 3, 5 | params_general |
| do.part3.pdf | 3 | params_output |
| sensor.location | 3, 4 | params_general |
| HASPT.algo | 3 | params_sleep |
| HASIB.algo | 3 | params_sleep |
| Sadeh_axis | 3 | params_sleep |
| longitudinal_axis | 3 | params_sleep |
| HASPT.ignore.invalid | 3 | params_sleep |
| oakley_threshold | 3 | params_sleep |
| consider_marker_button | 3 | params_sleep |
| impute_marker_button | 3 | params_sleep |
| SRI1_smoothing_wsize_hrs | 3 | params_sleep |
| SRI1_smoothing_frac | 3 | params_sleep |
| spt_min_block_dur | 3 | params_sleep |
| spt_max_gap_dur | 3 | params_sleep |
| spt_max_gap_ratio | 3 | params_sleep |
| HorAngle_threshold | 3 | params_sleep |
| guider_cor_maxgap_hrs | 3 | params_sleep |
| guider_cor_min_frac_sib | 3 | params_sleep |
| guider_cor_min_hrs | 3 | params_sleep |
| guider_cor_meme_frac_out | 3 | params_sleep |
| guider_cor_meme_frac_in | 3 | params_sleep |
| guider_cor_meme_min_hrs | 3 | params_sleep |
| guider_cor_do | 3 | params_sleep |
| guider_cor_meme_min_dys | 3 | params_sleep |
| loglocation | 4, 5 | params_sleep |
| colid | 4 | params_sleep |
| coln1 | 4 | params_sleep |
| do.visual | 4 | params_output |
| outliers.only | 4 | params_output |
| excludefirstlast | 4 | params_cleaning |
| criterror | 4 | params_output |
| includenightcrit | 4 | params_cleaning |
| relyonguider | 4 | params_sleep |
| relyonsleeplog | 4 | deprecated |
| sleepefficiency.metric | 4 | params_sleep |
| def.noc.sleep | 4 | params_sleep |
| sib_must_fully_overlap_with_TimeInBed | 4 | params_sleep |
| data_cleaning_file | 4, 5 | params_cleaning |
| excludefirst.part4 | 4 | params_cleaning |
| excludelast.part4 | 4 | params_cleaning |
| sleepwindowType | 4 | params_cleaning |
| excludefirstlast.part5 | 5 | params_cleaning |
| boutcriter.mvpa | 5 | params_phyact |
| boutcriter.in | 5 | params_phyact |
| boutcriter.lig | 5 | params_phyact |
| threshold.lig | 5 | params_phyact |
| threshold.mod | 5 | params_phyact |
| threshold.vig | 5 | params_phyact |
| boutdur.mvpa | 5 | params_phyact |
| boutdur.in | 5 | params_phyact |
| boutdur.lig | 5 | params_phyact |
| save_ms5rawlevels | 5 | params_output |
| part5_agg2_60seconds | 5 | params_general |
| includedaycrit.part5 | 5 | params_cleaning |
| includenight.part5 | 5 | params_cleaning |
| frag.metrics | 5 | params_phyact |
| LUXthresholds | 5 | params_247 |
| LUX_cal_constant | 5 | params_247 |
| LUX_cal_exponent | 5 | params_247 |
| LUX_day_segments | 5 | params_247 |
| SRI2_WASOmin | 5 | params_247 |
| timewindow | 5 | params_output |
| save_ms5raw_format | 5 | params_output |
| save_ms5raw_without_invalid | 5 | params_output |
| do.sibreport | 5 | params_output |
| possible_nap_window | 5 | params_sleep |
| possible_nap_dur | 5 | params_sleep |
| possible_nap_gap | 5 | params_sleep |
| nap_markerbutton_method | 5 | params_sleep |
| nap_markerbutton_max_distance | 5 | params_sleep |
| method_research_vars | 5 | params_sleep |
| includecrit.part6 | 6 | params_cleaning |
| part6_threshold_combi | 6 | params_phyact |
| part6CR | 6 | params_247 |
| part6HCA | 6 | params_247 |
| part6Window | 6 | params_247 |
| part6DFA | 6 | params_247 |
| require_complete_lastnight_part5 | 5 | params_output |
| visualreport_without_invalid | visualreport | params_output |
| visualreport_hrsPerRow | visualreport | params_output |
| visualreport_focus | visualreport | params_output |
| visualreport_validcrit | visualreport | params_output |
| dofirstpage | visualreport | params_output |
| visualreport | visualreport | params_output |
| viewingwindow | visualreport | params_output |

## Arguments/parameters description

All information as shown below has been auto-generated and is identical
to the information provided in the [GGIR package pdf
manual](https://CRAN.R-project.org/package=GGIR/GGIR.pdf).

### GGIR function input arguments

#### mode

Numeric (default = 1:5). Specify which of the five parts need to be run,
e.g., mode = 1 makes that g.part1 is run; or mode = 1:5 makes that the
whole GGIR pipeline is run, from g.part1 to g.part5. Optionally mode can
also include the number 6 to tell GGIR to run g.part6 which is currently
under development.

#### datadir

Character (default = c()). Directory where the accelerometer files are
stored, e.g., "C:/mydata", or list of accelerometer filenames and
directories, e.g. c("C:/mydata/myfile1.bin", "C:/mydata/myfile2.bin").

#### outputdir

Character (default = c()). Directory where the output needs to be
stored. Note that this function will attempt to create folders in this
directory and uses those folder to keep output.

#### studyname

Character (default = c()). If the datadir is a folder, then the study
will be given the name of the data directory. If datadir is a list of
filenames then the studyname as specified by this input argument will be
used as name for the study.

#### f0

Numeric (default = 1). File index to start with (default = 1). Index
refers to the filenames sorted in alphabetical order.

#### f1

Numeric (default = 0). File index to finish with (defaults to number of
files available).

#### do.report

Numeric (default = c(2, 4, 5, 6)). For which parts to generate a summary
spreadsheet: 2, 4, 5, and/or 6. Default is c(2, 4, 5, 6). A report will
be generated based on the available milestone data. When creating
milestone data with multiple machines it is advisable to turn the report
generation off when generating the milestone data, value = c(), and then
to merge the milestone data and turn report generation back on while
setting overwrite to FALSE.

#### configfile

Character (default = c()). Configuration file previously generated by
function GGIR. See details.

#### myfun

List (default = c()). External function object to be applied to raw
data. See package vignette for detailed tutorial with examples on how to
use the function embedding:
<https://wadpac.github.io/GGIR/articles/ExternalFunction.html>

### General Parameters

#### overwrite

Boolean (default = FALSE). Do you want to overwrite analysis for which
milestone data exists? If overwrite = FALSE, then milestone data from a
previous analysis will be used if available and visual reports will not
be created again.

#### acc.metric

Character (default = "ENMO"). Which one of the acceleration metrics do
you want to use for all acceleration magnitude analyses in GGIR part 5
and the visual report? For example: "ENMO", "LFENMO", "MAD",
"NeishabouriCount_y", or "NeishabouriCount_vm". Only one acceleration
metric can be specified and the selected metric needs to have been
calculated in part 1 (see g.part1) via arguments such as do.enmo = TRUE
or do.mad = TRUE.

#### maxNcores

Numeric (default = NULL). Maximum number of cores to use when argument
do.parallel is set to true. GGIR by default uses either the maximum
number of available cores or the number of files to process (whichever
is lower), but this argument allows you to set a lower maximum.

#### print.filename

Boolean (default = FALSE). Whether to print the filename before
analysing it (in case do.parallel = FALSE). Printing the filename can be
useful to investigate problems (e.g., to verify that which file is being
read).

#### do.parallel

Boolean (default = TRUE). Whether to use multi-core processing (only
works if at least 4 CPU cores are available).

#### windowsizes

Numeric vector, three values (default = c(5, 900, 3600)). To indicate
the lengths of the windows as in c(window1, window2, window3): window1
is the short epoch length in seconds, by default 5, and this is the time
window over which acceleration and angle metrics are calculated; window2
is the long epoch length in seconds for which non-wear and signal
clipping are defined, default 900 (expected to be a multitude of 60
seconds); window3 is the window length of data used for non-wear
detection and by default 3600 seconds. So, when window3 is larger than
window2 we use overlapping windows, while if window2 equals window3
non-wear periods are assessed by non-overlapping windows.

#### desiredtz

Character (default = "", i.e., system timezone). Timezone in which
device was configured and experiments took place. If experiments took
place in a different timezone, then use this argument for the timezone
in which the experiments took place and argument configtz to specify
where the device was configured. Use the "TZ identifier" as specified at
<https://en.wikipedia.org/wiki/Zone.tab> to set desiredtz, e.g.,
"Europe/London". As of GGIR 3.2-12, desiredtz is only configurable in
GGIR part 1 and passed on to the following parts via de milestone data
files stored by each part. This means that if you would run GGIR(mode =
2, desiredtz = “Europe/Berlin”) GGIR would ignore the timezone
specification and rely on the timezone specified when part 1 was
processed specific to each recording. As a result, it is now possible to
process part 1 separately with different timezones, then pool the
milestone data into one output folder, and finally process all files
jointly with the subsequent GGIR parts (2-6) which will automatically
accounts for each individual’s file timezone.

#### configtz

Character (default = "", i.e., system timezone). At the moment only
functional for GENEActiv .bin, AX3 cwa, ActiGraph .gt3x, and ad-hoc csv
file format. Timezone in which the accelerometer was configured. Only
use this argument if the timezone of configuration and timezone in which
recording took place are different. Use the "TZ identifier" as specified
at <https://en.wikipedia.org/wiki/Zone.tab> to set configtz, e.g.,
"Europe/London".

#### idloc

Numeric (default = 1). If idloc = 1 the code assumes that ID number is
stored in the obvious header field. Note that for ActiGraph data the ID
is never stored in the file header. For value set to 2, 5, 6, and 7,
GGIR looks at the filename and extracts the character string preceding
the first occurance of a "\_" (idloc = 2), " " (space, idloc = 5), "."
(dot, idloc = 6), and "-" (idloc = 7), respectively. You may have
noticed that idloc 3 and 4 are skipped, they were used for one study in
2012, and not actively maintained anymore, but because it is legacy code
not omitted.

#### dayborder

Numeric (default = 0). Hour at which days start and end (dayborder = 4
would mean 4 am).

#### part5_agg2_60seconds

Boolean (default = FALSE). Whether to use aggregate epochs to 60 seconds
as part of the GGIR g.part5 analysis. Aggregation is doen by averaging.
Note that when working with count metrics such as Neishabouri counts
this means that the threshold can stay the same as in part 2, because
again the threshold is expressed relative to the original epoch size,
even if averaged per minute. For example if we want to use a cut-point
100 count per minute then we specify mvpathreshold = 100 \* (5/60) as
well as \`threshold.mod = 100 \* (5/60) regardless of whether we set
part5_agg2_60seconds to TRUE or FALSE.

#### sensor.location

Character (default = "wrist"). To indicate sensor location, default is
wrist. If it is hip, the HDCZA algorithm for sleep detection also
requires longitudinal axis of sensor to be between -45 and +45 degrees.

#### expand_tail_max_hours

Numeric (default = NULL). This parameter has been replaced by
recordingEndSleepHour.

#### recordingEndSleepHour

Numeric (default = NULL). Time (in hours) at which the recording should
end (or later) to expand the g.part1 output with synthetic data to
trigger sleep detection for last night. Using argument
recordingEndSleepHour implies the assumption that the participant fell
asleep at or before the end of the recording if the recording ended at
or after recordingEndSleepHour hour of the last day. This assumption may
not always hold true and should be used with caution. The synthetic data
for metashort entails: timestamps continuing regularly, zeros for
acceleration metrics other than EN, one for EN. Angle columns are
created in a way that it triggers the sleep detection using the
equation: round(sin((1:length_expansion) / (900/epochsize))) \* 15. To
keep track of the tail expansion g.part1 stores the length of the
expansion in the RData files, which is then passed via g.part2, g.part3,
and g.part4 to g.part5. In g.part5 the tail expansion size is included
as an additional variable in the csv-reports. In the g.part4 csv-report
the last night is omitted, because we know that sleep estimates from the
last night will not be trustworthy. Similarly, in the g.part5 output
columns related to the sleep assessment will be omitted for the last
window to avoid biasing the averages. Further, the synthetic data are
also ignored in the visualizations and time series output to avoid
biased output.

#### dataFormat

Character (default = "raw"). To indicate what the format is of the data
in datadir. Alternatives: ukbiobank_csv, actiwatch_csv, actiwatch_awd,
actigraph_csv, sensewear_xls, phb_xlsx, and fitbit_json which correspond
to epoch level data files from, respecitively, UK Biobank in csv format,
Actiwatch in csv format, Actiwatch in awd format, ActiGraph csv format,
Sensewear in xls format (also works with xlsx), Philips Health Band in
xlsx format, and Fitbit in json format. Here, the assumed epoch size for
UK Biobank csvdata is 5 seconds. The epoch size for the other non-raw
data formats is flexible, but make sure that you set first value of
argument windowsizes accordingly. Also when working with non-raw data
formats specify argument extEpochData_timeformat as documented below.
For ukbiobank_csv nonwear is a column in the data itself, for
actiwatch_csv, actiwatch_awd, actigraph_csv, and sensewear_xls non-wear
is detected as 60 minute rolling zeros. The length of this window can be
modified with the third value of argument windowsizes expressed in
seconds.

#### maxRecordingInterval

Numeric (default = NULL). To indicate the maximum gap in hours between
repeated measurements with the same ID for the recordings to be
appended. So, the assumption is that the ID can be matched, make sure
argument idloc is set correctly. If argument maxRecordingInterval is set
to NULL (default) recordings are not appended. If recordings overlap
then GGIR will use the data from the latest recording. If recordings are
separated then the timegap between the recordings is filled with data
points that resemble monitor not worn. The maximum value of maxFile gap
is 504 (21 days). Only recordings from the same accelerometer brand are
appended. This functionality is applied after all other aspect of GGIR
part 1 are completed for all input files. The part 2 csv report will
show number of appended recordings, sampling rate for each, time overlap
or gap and the names of the filenames of the respective recording.

#### extEpochData_timeformat

Character (default = "%d-%m-%Y %H:%M:%S"). To specify the time format
used in the external epoch level data when argument dataFormat is set to
"actiwatch_csv", "actiwatch_awd", "actigraph_csv" or "sensewear_xls".
For example "%Y-%m-%d %I:%M:%S %p" for "2023-07-11 01:24:01 PM" or
"%m/%d/%Y %H:%M:%S" "2023-07-11 13:24:01". For guidance on how to
specify time formats in R see:
<https://wadpac.github.io/GGIR/articles/DateTimeFormatsInR.html>.

#### recording_split_times

Character (default = NULL). To indicate path to a csv file with a column
that has ID in the column name that holds the participant IDs, followed
by columns with either dates or full timestamps on which the recording
should be split. Here, timestamp format is specified with parameter
recording_split_timeformat. The names of these columns are used to refer
to each resulting segment. To prevent extremely long file name GGIR
ignores spaces and does not consider more than 10 characters which are
forced to lower case. If the recording starts before or ends after the
first and last split time then the start and/or end of the recording is
also used as split time and referred to as startrec and endrec,
respectively. Files are split after all other aspect of GGIR part 1 are
completed for all input files.

#### recording_split_timeformat

Character (default = "%d/%m/%Y %H:%M") To indicate timestamp format as
used in file as specified with recording_split_times. For guidance on
how to specify time formats in R see:
<https://wadpac.github.io/GGIR/articles/DateTimeFormatsInR.html>.

#### recording_split_overlap

Numeric (default = 0). Number of hours to use as buffer when splitting
recording. A possitive number means that the split recordings overlap, a
nevative number indicates a gap.

#### recording_split_ignore_edges

Boolean (default = FALSE) To indicate whether the recording time before
and after the time range defined by recording_split_times should be
considered as segments.

#### use_trycatch_serial

Boolean (default = FALSE). If set to TRUE and when using the non-default
do.parallel = FALSE, the core processing function in each GGIR part is
wrapped for each file in a tryCatch block. This allows GGIR to continue
processing remaining files even if one or more files cause an error
(e.g., due to file corruption). The error messages for failed files are
logged and printed in the console at the end of the processing. Note:
these error messages are less detailed compared to when processing data
with use_trycatch_serial == FALSE. So, to investigate an error message
it may be advisable to set use_trycatch_serial == FALSE. Further, it
should be noted that tryCatch blocks are always used when processing
do.parallel = TRUE. The functionally described here is envisioned to be
primarily useful when it not possible to process accelerometer files in
parallel and there are many files. For example, when GGIR is expected to
run only on a single thread in a computing cluster. In that scenario it
would actually be better to investigate whether GGIR can be given more
computing threads as GGIR is designed to coordinate the processing of
files across processing threads. A second scenario where
use_trycatch_serial == TRUE is useful is when processing files that are
too large to be processed even if parameter chunksize is set to a
minimal value. In that scenario you may want to set use_trycatch_serial
== TRUE and do.parallel = FALSE. Note that under normal conditions we
expect GGIR to not generate errors. If an error occurs, always
investigate and report the error to the community to help make GGIR
better. GGIR is free software and depends on you as community.

### Raw Data Parameters

#### chunksize

Numeric (default = 1). Value to specify the size of chunks to be loaded
as a fraction of an approximately 12 hour period for auto-calibration
procedure and as fraction of 24 hour period for the metric calculation,
e.g., 0.5 equals 6 and 12 hour chunks, respectively. For machines with
less than 4Gb of RAM memory or with \< 2GB memory per process when using
do.parallel = TRUE a value below 1 is recommended. The value is
constrained by GGIR to not be lower than 0.05. Please note that setting
0.05 will not produce output when 3rd value of parameter windowsizes is
3600.

#### spherecrit

Numeric (default = 0.3). The minimum required acceleration value (in g)
on both sides of 0 g for each axis. Used to judge whether the sphere is
sufficiently populated

#### minloadcrit

Numeric (default = 168). The minimum number of hours the code needs to
read for the autocalibration procedure to be effective (only sensitive
to multitudes of 12 hrs, other values will be ceiled). After loading
these hours only extra data is loaded if calibration error has not been
reduced to under 0.01 g.

#### printsummary

Boolean (default = FALSE). If TRUE will print a summary of the
calibration procedure in the console when done.

#### do.cal

Boolean (default = TRUE). Whether to apply auto-calibration or not by
g.calibrate. Recommended setting is TRUE.

#### backup.cal.coef

Character (default = "retrieve"). Option to use backed-up calibration
coefficient instead of deriving the calibration coefficients when
analysing the same file twice. Argument backup.cal.coef has two usecase.
Use case 1: If the auto-calibration fails then the user has the option
to provide back-up calibration coefficients via this argument. The value
of the argument needs to be the name and directory of a csv-spreadsheet
with the following column names and subsequent values: "filename" with
the names of accelerometer files on which the calibration coefficients
need to be applied in case auto-calibration fails; "scale.x", "scale.y",
and "scale.z" with the scaling coefficients; "offset.x", "offset.y", and
"offset.z" with the offset coefficients, and; "temperature.offset.x",
"temperature.offset.y", and "temperature.offset.z" with the temperature
offset coefficients. This can be useful for analysing short lasting
laboratory experiments with insufficient sphere data to perform the
auto-calibration, but for which calibration coefficients can be derived
in an alternative way. It is the users responsibility to compile the
csv-spreadsheet. Instead of building this file the user can also Use
case 2: The user wants to avoid performing the auto-calibration
repeatedly on the same file. If backup.cal.coef value is set to
"retrieve" (default) then GGIR will look out for the
"data_quality_report.csv" file in the outputfolder QC, which holds the
previously generated calibration coefficients. If you do not want this
happen, then deleted the data_quality_report.csv from the QC folder or
set it to value "redo".

#### dynrange

Numeric (default = NULL). Provide dynamic range of 8 gravity.

#### minimumFileSizeMB

Numeric (default = 2). Minimum File size in MB required to enter
processing. This argument can help to avoid having short uninformative
files to enter the analyses. Given that a typical accelerometer collects
several MBs per hour, the default setting should only skip the very tiny
files.

#### rmc.dec

Character (default = "."). Decimal used for numbers, same as dec
argument in \[utils\]read.csv and in \[data.table\]fread.

#### rmc.firstrow.acc

Numeric (default = NULL). First row (number) of the acceleration data.

#### rmc.firstrow.header

Numeric (default = NULL). First row (number) of the header. Leave blank
if the file does not have a header.

#### rmc.header.length

Numeric (default = NULL). If file has header, specify header length
(number of rows).

#### rmc.col.acc

Numeric, three values (default = c(1, 2, 3)). Vector with three column
(numbers) in which the acceleration signals are stored.

#### rmc.col.temp

Numeric (default = NULL). Scalar with column (number) in which the
temperature is stored. Leave in default setting if no temperature is
available. The temperature will be used by g.calibrate.

#### rmc.col.time

Numeric (default = NULL). Scalar with column (number) in which the
timestamps are stored. Leave in default setting if timestamps are not
stored.

#### rmc.unit.acc

Character (default = "g"). Character with unit of acceleration values:
"g", "mg", or "bit".

#### rmc.unit.temp

Character (default = "C"). Character with unit of temperature values:
(K)elvin, (C)elsius, or (F)ahrenheit.

#### rmc.unit.time

Character (default = "POSIX"). Character with unit of timestamps:
"POSIX", "UNIXsec" (seconds since origin, see argument rmc.origin),
"UNIXmsec" (same as UNIXsec but in milliseconds), "character", or
"ActivPAL" (exotic timestamp format only used in the ActivPAL activity
monitor).

#### rmc.format.time

Character (default = "%Y-%m-%d %H:%M:%OS"). Character giving a date-time
format as used by \[base\]strptime. Only used for rmc.unit.time:
character and POSIX.. For guidance on how to specify time formats in R
see: <https://wadpac.github.io/GGIR/articles/DateTimeFormatsInR.html>

#### rmc.bitrate

Numeric (default = NULL). If unit of acceleration is a bit then provide
bit rate, e.g., 12 bit.

#### rmc.dynamic_range

Numeric or character (default = NULL). If unit of acceleration is a bit
then provide dynamic range deviation in g from zero, e.g., +/-6g would
mean this argument needs to be 6. If you give this argument a character
value the code will search the file header for elements with a name
equal to the character value and use the corresponding numeric value
next to it as dynamic range.

#### rmc.unsignedbit

Boolean (default = TRUE). If unsignedbit = TRUE means that bits are only
positive numbers. if unsignedbit = FALSE then bits are both positive and
negative.

#### rmc.origin

Character (default = "1970-01-01"). Origin of time when unit of time is
UNIXsec, e.g., 1970-1-1.

#### rmc.desiredtz

Character (default = NULL). Timezone in which experiments took place.
This argument is scheduled to be deprecated and is now used to overwrite
desiredtz if not provided.

#### rmc.configtz

Character (default = NULL). Timezone in which device was configured.
This argument is scheduled to be deprecated and is now used to overwrite
configtz if not provided.

#### rmc.sf

Numeric (default = NULL). Sample rate in Hertz, if this is stored in the
file header then that will be used instead (see argument
rmc.headername.sf).

#### rmc.headername.sf

Character (default = NULL). If file has a header: Row name under which
the sample frequency can be found.

#### rmc.headername.sn

Character (default = NULL). If file has a header: Row name under which
the serial number can be found.

#### rmc.headername.recordingid

Character (default = NULL). If file has a header: Row name under which
the recording ID can be found.

#### rmc.header.structure

Character (default = NULL). Used to split the header name from the
header value, e.g., ":" or " ".

#### rmc.check4timegaps

Boolean (default = FALSE). To indicate whether gaps in time should be
imputed with zeros. Some sensing equipment provides accelerometer with
gaps in time. The rest of GGIR is not designed for this, by setting this
argument to TRUE the gaps in time will be filled with zeros.

#### rmc.noise

Numeric (default = 13). Noise level of acceleration signal in m-units,
used when working ad-hoc .csv data formats using read.myacc.csv. The
read.myacc.csv does not take rmc.noise as argument, but when interacting
with GGIR or g.part1 rmc.noise is used.

#### nonwear_range_threshold

Numeric (default 150) used to define maximum value range per axis for
non-wear detection, used in combination with brand specific standard
deviation per axis.

#### rmc.col.wear

Numeric (default = NULL). If external wear detection outcome is stored
as part of the data then this can be used by GGIR. This argument
specifies the column in which the wear detection (Boolean) is stored.

#### rmc.doresample

Boolean (default = FALSE). To indicate whether to resample the data
based on the available timestamps and extracted sample rate from the
file header.

#### interpolationType

Integer (default = 1). To indicate type of interpolation to be used when
resampling time series (mainly relevant for Axivity and Parmay Matrix
sensors), 1=linear, 2=nearest neighbour.

#### imputeTimegaps

Boolean (default = TRUE). To indicate whether timegaps larger than 1
sample should be imputed. Currently only used for .gt3x data and
ActiGraph .csv format, where timegaps can be expected as a result of
Actigraph’s idle sleep.mode configuration.

#### frequency_tol

Number (default = 0.1) as passed on to readAxivity from the GGIRread
package. Represents the frequency tolerance as fraction between 0 and 1.
When the relative bias per data block is larger than this fraction then
the data block will be imputed by lack of movement with gravitational
oriationed guessed from most recent valid data block. Only applicable to
Axivity .cwa data.

#### rmc.scalefactor.acc

Numeric value (default 1) to scale the acceleration signals via
multiplication. For example, if data is provided in m/s2 then by setting
this to 1/9.81 we would derive gravitational units.

### Metrics Parameters

#### do.anglex

Boolean (default = FALSE). If TRUE, calculates the angle of the X axis
relative to the horizontal: = (^-1_rollmedian(x)(acc_rollmedian(y))^2 +
(acc_rollmedian(z))^2) \* 180/

#### do.angley

Boolean (default = FALSE). If TRUE, calculates the angle of the Y axis
relative to the horizontal: = (^-1_rollmedian(y)(acc_rollmedian(x))^2 +
(acc_rollmedian(z))^2) \* 180/

#### do.anglez

Boolean (default = TRUE). If TRUE, calculates the angle of the Z axis
relative to the horizontal: = (^-1_rollmedian(z)(acc_rollmedian(x))^2 +
(acc_rollmedian(y))^2) \* 180/

#### do.zcx

Boolean (default = FALSE). If TRUE, calculates metric zero-crossing
count for x-axis. For computation specifics see source code of function
g.applymetrics

#### do.zcy

Boolean (default = FALSE). If TRUE, calculates metric zero-crossing
count for y-axis. For computation specifics see source code of function
g.applymetrics

#### do.zcz

Boolean (default = FALSE). If TRUE, calculates metric zero-crossing
count for z-axis. For computation specifics see source code of function
g.applymetrics

#### do.enmo

Boolean (default = TRUE). If TRUE, calculates the metric: = \_x^2 +
acc_y^2 + acc_z^2 - 1 (if ENMO \< 0, then ENMO = 0).

#### do.lfenmo

Boolean (default = FALSE). If TRUE, calculates the metric ENMO over the
low-pass filtered accelerations (for computation specifics see source
code of function g.applymetrics). The filter bound is defined by the
parameter hb.

#### do.en

Boolean (default = FALSE). If TRUE, calculates the Euclidean Norm of the
raw accelerations: = \_x^2 + acc_y^2 + acc_z^2

#### do.mad

Boolean (default = FALSE). If TRUE, calculates the Mean Amplitude
Deviation: = 1n\|r_i - \|

#### do.enmoa

Boolean (default = FALSE). If TRUE, calculates the metric: = \_x^2 +
acc_y^2 + acc_z^2 - 1 (if ENMOa \< 0, then ENMOa = \|ENMOa\|).

#### do.roll_med_acc_x

Boolean (default = FALSE). If TRUE, calculates the metric. For
computation specifics see source code of function g.applymetrics.

#### do.roll_med_acc_y

Boolean (default = FALSE). If TRUE, calculates the metric. For
computation specifics see source code of function g.applymetrics.

#### do.roll_med_acc_z

Boolean (default = FALSE). If TRUE, calculates the metric. For
computation specifics see source code of function g.applymetrics.

#### do.dev_roll_med_acc_x

Boolean (default = FALSE). If TRUE, calculates the metric. For
computation specifics see source code of function g.applymetrics.

#### do.dev_roll_med_acc_y

Boolean (default = FALSE). If TRUE, calculates the metric. For
computation specifics see source code of function g.applymetrics.

#### do.dev_roll_med_acc_z

Boolean (default = FALSE). If TRUE, calculates the metric. For
computation specifics see source code of function g.applymetrics.

#### do.bfen

Boolean (default = FALSE). If TRUE, calculates the metric. For
computation specifics see source code of function g.applymetrics.

#### do.hfen

Boolean (default = FALSE). If TRUE, calculates the metric. For
computation specifics see source code of function g.applymetrics.

#### do.hfenplus

Boolean (default = FALSE). If TRUE, calculates the metric. For
computation specifics see source code of function g.applymetrics.

#### do.lfen

Boolean (default = FALSE). If TRUE, calculates the metric. For
computation specifics see source code of function g.applymetrics.

#### do.lfx

Boolean (default = FALSE). If TRUE, calculates the metric. For
computation specifics see source code of function g.applymetrics.

#### do.lfy

Boolean (default = FALSE). If TRUE, calculates the metric. For
computation specifics see source code of function g.applymetrics.

#### do.lfz

Boolean (default = FALSE). If TRUE, calculates the metric. For
computation specifics see source code of function g.applymetrics.

#### do.hfx

Boolean (default = FALSE). If TRUE, calculates the metric. For
computation specifics see source code of function g.applymetrics.

#### do.hfy

Boolean (default = FALSE). If TRUE, calculates the metric. For
computation specifics see source code of function g.applymetrics.

#### do.hfz

Boolean (default = FALSE). If TRUE, calculates the metric. For
computation specifics see source code of function g.applymetrics.

#### do.bfx

Boolean (default = FALSE). If TRUE, calculates the metric. For
computation specifics see source code of function g.applymetrics.

#### do.bfy

Boolean (default = FALSE). If TRUE, calculates the metric. For
computation specifics see source code of function g.applymetrics.

#### do.bfz

Boolean (default = FALSE). If TRUE, calculates the metric. For
computation specifics see source code of function g.applymetrics.

#### do.brondcounts

Boolean (default = FALSE). this option has been deprecated (October
2022) due to issues with the activityCounts package that we used as a
dependency. If TRUE, calculated the metric via R package activityCounts.
We called them BrondCounts because there are large number of activity
counts in the physical activity and sleep research field. By calling
them *brondcounts* we clarify that these are the counts proposed by Jan
Brønd and implemented in R by Ruben Brondeel. The *brondcounts* are
intended to be an imitation of the counts produced by one of the closed
source ActiLife software by ActiGraph.

#### do.neishabouricounts

Boolean (default = FALSE). If TRUE, calculates the metric via R package
actilifecounts, which is an implementation of the algorithm used in the
closed-source software ActiLife by ActiGraph (methods published in doi:
10.1038/s41598-022-16003-x). We use the name of the first author
(instead of ActiLifeCounts) of the paper and call them NeishabouriCount
under the uncertainty that ActiLife will implement this same algorithm
over time. To use the Neishabouri counts for the physical activity
intensity classification in part 5 (i.e., metric over the threshold.lig,
threshold.mod, and threshold.vig would be applied), the acc.metric
argument needs to be set as one of the following: "NeishabouriCount_x",
"NeishabouriCount_y", "NeishabouriCount_z", "NeishabouriCount_vm" to use
the counts in the x-, y-, z-axis or vector magnitude, respectively.

#### hb

Numeric (default = 15). Higher boundary of the frequency filter (in
Hertz) as used in the filter-based metrics.

#### lb

Numeric (default = 0.2). Lower boundary of the frequency filter (in
Hertz) as used in the filter-based metrics.

#### n

Numeric (default = n). Order of the frequency filter as used in the
filter-based metrics.

#### zc.lb

Numeric (default = 0.25). Used for zero-crossing counts only. Lower
boundary of cut-off frequency filter.

#### zc.hb

Numeric (default = 3). Used for zero-crossing counts only. Higher
boundary of cut-off frequencies in filter.

#### zc.sb

Numeric (default = 0.01). Stop band used for calculation of zero
crossing counts. Value is the acceleration threshold in g units below
which acceleration will be rounded to zero.

#### zc.order

Numeric (default = 2). Used for zero-crossing counts only. Order of
frequency filter.

#### zc.scale

Numeric (default = 1) Used for zero-crossing counts only. Scaling factor
to be applied after counts are calculated (GGIR part 3).

#### actilife_LFE

Boolean (default = FALSE). If TRUE, calculates the NeishabouriCount
metric with the low-frequency extension filter as proposed in the closed
source ActiLife software by ActiGraph. Only applicable to the metric
NeishabouriCount.

### Cleaning Parameters

#### includedaycrit

Numeric (default = 16). Minimum required number of valid hours in
calendar day specific to analysis in part 2. If you specify two values
as in c(16, 16) then the first value will be used in part 2 and the
second value will be used in part 5 and applied as a criterion on the
full part 5 window. Note that this is then applied in addition to
parameter includedaycrit.part5 which only looks at valid data during
waking hours.

#### ndayswindow

Numeric (default = 7). If data_masking_strategy is set to 3 or 5, then
this is the size of the window as a number of days. For
data_masking_strategy 3 value can be fractional, e.g. 7.5, while for
data_masking_strategy 5 it needs to be an integer.

#### strategy

Deprecated and replaced by data_masking_strategy. If strategy is
specified then its value is passed on and used for
data_masking_strategy.

#### data_masking_strategy

Numeric (default = 1). How to deal with knowledge about study protocol.
data_masking_strategy = 1 means select data based on hrs.del.start and
hrs.del.end. data_masking_strategy = 2 makes that only the data between
the first midnight and the last midnight is used. data_masking_strategy
= 3 selects the most active X days in the file where X is specified by
argument ndayswindow, where the days are a series of 24-h blocks
starting any time in the day (X hours at the beginning and end of this
period can be deleted with arguments hrs.del.start and hrs.del.end)
data_masking_strategy = 4 to only use the data after the first midnight.
data_masking_strategy = 5 is similar to data_masking_strategy = 3, but
it selects X complete calendar days where X is specified by argument
ndayswindow (X hours at the beginning and end of this period can be
deleted with arguments hrs.del.start and hrs.del.end).

#### maxdur

Numeric (default = 0). How many DAYS after start of experiment did
experiment definitely stop? (set to zero if unknown).

#### hrs.del.start

Numeric (default = 0). How many HOURS after start of experiment did
wearing of monitor start? Used in GGIR g.part2 when
data_masking_strategy = 1.

#### hrs.del.end

Numeric (default = 0). How many HOURS before the end of the experiment
did wearing of monitor definitely end? Used in GGIR g.part2 when
data_masking_strategy = 1.

#### includedaycrit.part5

Numeric (default = 2/3). Inclusion criteria used in part 5 for number of
valid hours during the waking hours of a day, when value is smaller than
or equal to 1 used as fraction of waking hours, when value above 1 used
as absolute number of valid hours required. Do not confuse this argument
with argument includedaycrit which is only used in GGIR part 2 and
applies to the entire day.

#### excludefirstlast.part5

Boolean (default = FALSE). If TRUE then the first and last window
(waking-waking, midnight-midnight, or sleep onset-onset) are ignored in
g.part5.

#### TimeSegments2ZeroFile

Character (default = NULL). Takes path to a csv file that has columns
"windowstart" and "windowend" to refer to the start and end time of a
time windows in format "2024-10-12 20:00:00", and "filename" of the GGIR
milestone data file without the "meta\_" segment of the name. GGIR part
2 uses this to set all acceleration values to zero and the non-wear
classification to zero (meaning sensor worn). Motivation: When the
accelerometer is not worn during the night GGIR automatically labels
them as invalid, while the user may like to treat them as zero movement.
Disclaimer: This functionality was developed in 2019. With hindsight it
is not generic enough and in need for revision. Please contact GGIR
maintainers if you would like us to invest time in improving this
functionality.

#### do.imp

Boolean (default = TRUE). Whether to impute missing values (e.g.,
suspected of monitor non-wear or clippling) or not by g.impute in GGIR
g.part2. Recommended setting is TRUE.

#### data_cleaning_file

Character (default = NULL). Optional path to a csv file you create that
holds four columns: ID, day_part5, relyonguider_part4, and night_part4.
ID should hold the participant ID. Columns day_part5 and night_part4
allow you to specify which day(s) and night(s) need to be excluded from
g.part5 and g.part4, respectively. When including multiple
day(s)/night(s) create a new line for each day/night. So, this will be
done regardless of whether the rest of GGIR thinks those day(s)/night(s)
are valid. Column relyonguider_part4 allows you to specify for which
nights g.part4 should fully rely on the guider. See also package
vignette.

#### minimum_MM_length.part5

Numeric (default = 23). Minimum length in hours of a MM day to be
included in the cleaned g.part5 results.

#### excludefirstlast

Boolean (default = FALSE). If TRUE then the first and last night of the
measurement are ignored for the sleep assessment in g.part4.

#### includenightcrit

Numeric (default = 16). Minimum number of valid hours per night (24 hour
window between noon and noon), used for sleep assessment in g.part4.

#### excludefirst.part4

Boolean (default = FALSE). If TRUE then the first night of the
measurement are ignored for the sleep assessment in g.part4.

#### excludelast.part4

Boolean (default = FALSE). If TRUE then the last night of the
measurement are ignored for the sleep assessment in g.part4.

#### max_calendar_days

Numeric (default = 0). The maximum number of calendar days to include
(set to zero if unknown).

#### nonWearEdgeCorrection

Boolean (default = TRUE). If TRUE then the non-wear detection around the
edges of the recording (first and last 3 hours) are corrected following
description in vanHees2013 as has been the default since then. This
functionality is advisable when working with sleep clinic or exercise
lab data typically lasting less than a day.

#### nonwear_approach

Character (default = "2023"). Whether to use the traditional version of
the non-wear detection algorithm (nonwear_approach = "2013") or the new
version (nonwear_approach = "2023"). The 2013 version would use the
longsize window (windowsizes\[3\], one hour as default) to check the
conditions for nonwear identification and would flag as nonwear the
mediumsize window (windowsizes\[2\], 15 min as default) in the middle.
The 2023 version differs in which it would flag as nonwear the full
longsize window. For the 2013 method the longsize window is centered in
the centre of the mediumsize window, while in the 2023 method the
longsizewindow is aligned with its left edge to the left edge of the
mediumsize window.

#### segmentWEARcrit.part5

Numeric (default = 0.5). Fraction of qwindow segment expected to be
valid in part 5, where 0.3 indicates that at least 30 percent of the
time should be valid.

#### segmentDAYSPTcrit.part5

Numeric vector or length 2 (default = c(0.9, 0)). Inclusion criteria for
the proportion of the segment that should be classified as day (awake)
and spt (sleep period time) to be considered valid. If you are
interested in comparing time spent in behaviour then it is better to set
one of the two numbers to 0, and the other defines the proportion of the
segment that should be classified as day or spt, respectively. The
default setting would focus on waking hour segments and includes all
segments that overlap for at least 90 percent with waking hours. In
order to shift focus to the SPT you could use c(0, 0.9) which ensures
that all segments that overlap for at least 90 percent with the SPT are
included. Setting both to zero would be problematic when comparing time
spent in behaviours between days or individuals: A complete segment
would be averaged with an incomplete segments (someone going to bed or
waking up in the middle of a segment) by which it is no longer clear
whether the person is less active or sleeps more during that segment.
Similarly it is not clear whether the person has more wakefulness during
SPT for a segment or woke up or went to bed during the segment.

#### study_dates_file

Character (default = c()). Full path to csv file containing the first
and last date of the expected wear period for every study participant
(dates are provided per individual). Expected format of the activity
diary is: First column headers followed by one row per recording. There
should be three columns: first column is recording ID, which needs to
match with the ID GGIR extracts from the accelerometer file; second
column should contain the first date of the study; and third column the
last date of the study. Date columns should be by default in format
"23-04-2017", or in the date format specified by argument
study_dates_dateformat (below). If not specified (default), then GGIR
would use the first and last day of the recording as beginning and end
of the study. Note that these dates are used on top of the
data_masking_strategy selected.

#### study_dates_dateformat

Character (default = "%d-%m-%Y"). To specify the date format used in the
study_dates_file as used by \[base\]strptime. For guidance on how to
specify time formats in R see:
<https://wadpac.github.io/GGIR/articles/DateTimeFormatsInR.html>

#### includecrit.part6

Numeric (default = c(2/3, 2/3)) Vector of two with the minimum fraction
of valid data required for day and spt time, respectively. This criteria
is only used for circadian rhythm analysis.

#### includenightcrit.part5

Numeric (default = 0). Inclusion criteria used in part 5 for number of
valid hours during the sleep period hours of a day (the night), when
value is smaller than or equal to 1 used as fraction of sleep period
hours, when value above 1 used as absolute number of valid hours
required. Do not confuse this argument with argument includenightcrit
which is only used in GGIR part 4 and applies to the entire 24 hour
window from noon to noon or 6pm to 6pm.

#### nonwearFiltermaxHours

Numeric (default = NULL). If not NULL, ignore detected nonwear periods
that last shorter than nonwearFiltermaxHours during a window as defined
by parameter NonwearFilterWindow. If NonwearFilterWindow is not provided
(set to NULL as is the default) check whether qwindow is defined as
activity diary file and whether it has timestamps that allow for
defining a time in bed, SPT or lightsoff window. For this the code looks
for column names in the diary with the terms inbed, sleeponset, or
lightsout to define the start of the window. Further, to define the end
of the window the code looks for columns outbed, wakeup, or lightsoff.
If yes, it uses the largest of the windows that can be defined with
these timestamps. If not, use midnight-6am as fall back setting. You may
realise that these columns are the same as the advanced format for the
sleelog accepted by GGIR part 4. So, if you already have an advanced
format sleeplog for part 4 then you can provide this as value for the
qwindow parameter. If you want to use qwindow to guide the nonwear
filtering as described in here but do not want GGIR to use it for day
segment analysis in part 2 or 5 then make sure the diary filename
includes the word "onlyfilter" or "filteronly" to tell GGIR to only use
qwindow for nonwear filtering. The main purpose of this functionality is
to offer the option to ignore short lasting nonwear episodes during the
night when there is suspicion that these are falsely detect, e.g. in
individuals with extended motionless sleep periods caused by medication.

#### nonwearFilterWindow

Numeric (default = NULL). Vector of length 2 to specify the start and
end hour of the night to be used for the functionality as described
above for NonwearFiltermaxHours.

### Sleep Parameters

#### anglethreshold

Numeric (default = 5). Angle threshold (degrees) for sustained
inactivity periods detection. The algorithm will look for periods of
time (timethreshold) in which the angle variability is lower than
anglethreshold. This can be specified as multiple thresholds, each of
which will be implemented, e.g., anglethreshold = c(5,10).

#### timethreshold

Numeric (default = 5). Time threshold (minutes) for sustained inactivity
periods detection. The algorithm will look for periods of time
(timethreshold) in which the angle variability is lower than
anglethreshold. This can be specified as multiple thresholds, each of
which will be implemented, e.g., timethreshold = c(5,10).

#### ignorenonwear

Boolean (default = TRUE). If TRUE then ignore detected monitor non-wear
periods to avoid confusion between monitor non-wear time and sustained
inactivity.

#### HASPT.algo

Character (default = "HDCZA"). To indicate what algorithm should be used
for the sleep period time detection. Default "HDCZA" is Heuristic
algorithm looking at Distribution of Change in Z-Angle as described in
van Hees et al. 2018. Other options included: "HorAngle", which is based
on HDCZA but replaces non-movement detection of the HDCZA algorithm by
looking for time segments where the angle of the longitudinal sensor
axis has an angle relative to the horizontal plane between -45 and +45
degrees. And "NotWorn" which is also the same as HDCZA but looks for
time segments when a rolling average of acceleration magnitude is below
5 per cent of its standard deviation, see Cookbook vignette in the
Annexes of <https://wadpac.github.io/GGIR/> for more detailed guidance
on how to use "NotWorn".

#### HASIB.algo

Character (default = "vanHees2015"). To indicate which algorithm should
be used to define the sustained inactivity bouts (i.e., likely sleep).
Options: "vanHees2015", "Sadeh1994", "Galland2012", "NotWorn",
"Oakley1997", "data". For details see vignette:
<https://wadpac.github.io/GGIR/articles/chapter8_SleepFundamentalsSibs.html>

#### Sadeh_axis

Character (default = "Y"). To indicate which axis to use for the
Sadeh1994 algorithm, and other algortihms that relied on count-based
Actigraphy such as Galland2012.

#### longitudinal_axis

Integer (default = NULL). To indicate which axis is the longitudinal
axis. If not provided, the function will estimate longitudinal axis as
the axis with the highest 24 hour lagged autocorrelation. Only used when
sensor.location = "hip" or HASPT.algo = "HorAngle".

#### HASPT.ignore.invalid

Boolean (default = FALSE). To indicate whether invalid time segments
should be ignored in the heuristic guiders. If FALSE (default), the
imputed angle or activity metric during the invalid time segments are
used. If TRUE, invalid time segments are ignored (i.e., they cannot
contribute to the guider). If NA, then invalid time segments are
considered to be no movement segments and can contribute to the guider.
Further, the guider name in the output will be shown with "+invalid" its
end, e.g. "HDCZA+invalid", to reflect the NA setting. When HASPT.algo is
"NotWorn", HASPT.ignore.invalid is automatically set to NA.

#### loglocation

Character (default = NULL). Path to csv file with sleep log information.
See package vignette for how to format this file.

#### colid

Numeric (default = 1). Column number in the sleep log spreadsheet in
which the participant ID code is stored.

#### coln1

Numeric (default = 2). Column number in the sleep log spreadsheet where
the onset of the first night starts.

#### nnights

Numeric (default = NULL). This argument has been deprecated.

#### relyonguider

Boolean (default = FALSE). Sustained inactivity bouts (sib) that overlap
with the guider are labelled as sleep. If relyonguider = FALSE and the
sib overlaps only partially with the guider then it is the sib that
defines the edge of the SPT window and not the guider. If relyonguider =
TRUE and the sib overlaps only partially with the guider then it is the
guider that defines the edge of the SPT window and not the sib. If
participants were instructed NOT to wear the accelerometer during waking
hours and ignorenonware=FALSE then set to relyonguider=TRUE, in all
other scenarios set to FALSE.

#### def.noc.sleep

Numeric (default = 1). The time window during which sustained inactivity
will be assumed to represent sleep, e.g., def.noc.sleep = c(21, 9). This
is only used if no sleep log entry is available. If left blank
def.noc.sleep = c() then the 12 hour window centred at the least active
5 hours of the 24 hour period will be used instead. Here, L5 is
hardcoded and will not change by changing argument winhr in function
g.part2. If def.noc.sleep is filled with a single integer, e.g.,
def.noc.sleep=c(1) then the window will be detected with based on built
in algorithms. See argument HASPT.algo from HASPT for specifying which
of the algorithms to use.

#### sleeplogsep

Character (default = NULL). This argument is deprecated.

#### sleepwindowType

Character (default = "SPT"). To indicate type of information in the
sleeplog, "SPT" for sleep period time. Set to "TimeInBed" if sleep log
recorded time in bed to enable calculation of sleep latency and sleep
efficiency.

#### possible_nap_window

Numeric (default = NULL). Numeric vector of length two with range in
clock hours during which naps are assumed to take place, e.g.,
possible_nap_window = c(9, 18). The nap classification is only applied
if both possible_nap_window and possible_nap_dur are specified. More
documentation to follow in 2025 when development work has completed.

#### possible_nap_dur

Numeric (default = NULL). Numeric vector of length two with range in
duration (minutes) of a nap, e.g., possible_nap_dur = c(15, 240). The
nap classification is only applied if both possible_nap_window and
possible_nap_dur are both specified. More documentation to follow in
2025 when development work has completed.

#### possible_nap_gap

Numeric (default = 0). Time gap expressed in seconds that is allowed
between the sustained inactivity bouts that form the naps.

#### possible_nap_edge_acc

Numeric (default = Inf). Maximum acceleration before or after the SIB
for the nap to be considered. By default this will allow all possible
naps.

#### nap_model

Character (default = NULL). To specify classification model. Currently
the only option is "hip3yr", which corresponds to a model trained with
hip data in 3-3.5 olds trained with parent diary data. This
functionality is currently superseded by nap detection which is
triggered by possible_nap_window and possible_nap_dur. Leave nap_model
as NULL when using the new functionality. More documentation to follow
in 2025 when development work has completed.

#### sleepefficiency.metric

Numeric (default = 1). If 1 (default), sleep efficiency is calculated as
detected sleep time during the SPT window divided by log-derived time in
bed. If 2, sleep efficiency is calculated as detected sleep time during
the SPT window divided by detected duration in sleep period time plus
sleep latency (where sleep latency refers to the difference between time
in bed and sleep onset). sleepefficiency.metric is only considered when
parameter sleepwindowType = "TimeInBed"

#### HDCZA_threshold

Numeric (default = c()) If HASPT.algo is set to "HDCZA" and
HDCZA_threshold is NOT NULL, (e.g., HDCZA_threshold = 0.2), then that
value will be used as threshold in the 6th step in the diagram of Figure
1 in van Hees et al. 2018 Scientific Report (doi:
10.1038/s41598-018-31266-z). However, doing so has not been supported by
research yet and is only intended to facilitate methodological research,
so we advise sticking with the default in line with the publication.
Further, if HDCZA_threshold is set to a numeric vector of length 2,
e.g. c(10, 15), that will be used as percentile and multiplier for the
above mentioned 6th step.

#### oakley_threshold

Numeric (default = 20) Threshold as used by the Oakley algorithm.
Original documentation suggestion to choose between 20, 40 or 80, while
in GGIR this can be any absolute number.

#### consider_marker_button

Boolean (default = FALSE) Whether to consider the marker button as
guider. Currently only functional for Actiwatch-type
count-accelerometers such as Philips Health Band and MotionWatch 8. For
details see:
<https://wadpac.github.io/GGIR/articles/chapter9_SleepFundamentalsGuiders.html>

#### impute_marker_button

Boolean (default = FALSE) Whether to impute marker buttons on other days
of the recording in the context of using it as guider with
consider_marker_button. For details see
<https://wadpac.github.io/GGIR/articles/chapter9_SleepFundamentalsGuiders.html>

#### sib_must_fully_overlap_with_TimeInBed

Boolean (default = c(TRUE, TRUE)). To indicate whether sib must fully
overlap with TimeInBed to be considered sleep for the start and end of
time in bed, respectively. Only considered when parameter
sleepwindowType = "TimeInBed". Note that negative sleep latency, if any,
will be reported in the night summary report (part4_nightsummary_sleep
csv files) but these negative sleep latency and corresponding sleep
efficiency values when calculating the person summary aggregate as
stored in the person level report (part4_summary_sleep csv files).

#### nap_markerbutton_method

Numeric (default = 0) Integer to indicate whether and how to use marker
button for nap detection: 0 = do not use marker button for nap detection
(default); 1 = if marker button is available use it for nap detection,
if not rely on accelerometer. 2 = nearby marker button is condition for
nap detection but rely on accelerometer to define exact nap timing, and;
3 = nearby marker button is condition for nap detection and used instead
of accelerometer classification.

#### nap_markerbutton_max_distance

Numeric (default = 30) When using nap_markerbutton_method with a value
other than 0 nap_markerbutton_max_distance sets the maximum distance in
minutes between the nearest marker button and the edge of a sustained
inactivity bout. If the marker button lies inside the sustained
inactivity bout but beyond the midpoint, no maximum distance is applied.
For example, if a sustained activity bout lasts from 11:00:00 to
13:00:00 then its midpoint will be 12:00:00 and with the default
nap_markerbutton_max_distance any marker button between 10:30:00 and
12:00:00 will be considered valid for the start of the nap and any
marker button between 12:00:00 and 13:30:00 will be considered valid for
the end of the nap.

#### SRI1_smoothing_wsize_hrs

Numeric (default = NULL) As used for Sleep Regularity Calculation in
g.part3. If SRI1_smoothing_wsize_hrs and SRI1_smoothing_frac are both
specified, this is the windowsize for smoothing of detected sustained
inactivity bouts before calculating SRI1. For example, if set to 1 and
0.8, we apply a rolling 1 hour window where each window with 80% or more
sleep is classificed as sleep.

#### SRI1_smoothing_frac

Numeric (default = NULL) As used for Sleep Regularity Calculation in
g.part3. If SRI1_smoothing_wsize_hrs and SRI1_smoothing_frac are both
specified, this is the fraction used to smooth detected sustained
inactivity bouts before calculating SRI1. For example, if set to 1 and
0.8, we apply a rolling 1 hour window where each window with 80% or more
sleep is classificed as sleep.

#### spt_min_block_dur

Numeric (default = 30). Minimum size in minutes of a resting block to be
considered part of SPT in the estimation of the guider window (relevant
for HASPT.algo settings: HDCZA, HorAngle and NotWorn).

#### spt_max_gap_dur

Numeric (default = 60). Maximum size in minutes of a gap between resting
blocks to be considered part of SPT in the estimation of the guider
window (relevant for HASPT.algo settings: HDCZA, HorAngle and NotWorn).

#### spt_max_gap_ratio

Numeric (default = 1). Only considered when set to a value less than 1
Maximum ratio between gap duration and either rest block next to it. If
the ratio for both is less than spt_max_gap_ratio the gap is merged with
the neighbouring rest blocks and together they are considered as
estimation of the SPT guider window (relevant for HASPT.algo settings:
HDCZA, HorAngle and NotWorn).

#### HorAngle_threshold

Numeric (default = 60). Angle threshold used for the HorAngle algorithm.

#### guider_cor_maxgap_hrs

Numeric (default = 2). Maximum gap duration in hours as used for the
correction method.

#### guider_cor_min_frac_sib

Numeric (default = 0.5) Required fraction of guider-based rest period
with sustained inactivity bouts (sib) to be considered as possible
extension of guider.

#### guider_cor_min_hrs

Numeric (default = 2) Minimum guider-based rest duration (hours) to be
considered as possible extension of guider.

#### guider_cor_meme_frac_out

Numeric (default = 0.9) Minimum required fraction of initial guider
classification outside the median start to median end (abbreviated as
meme) of the guider across days to initiate search for secondary that
falls within this window.

#### guider_cor_meme_frac_in

Numeric (default = 0.4) Minimum required fraction of alternative guider
classification inside the median start to median end (abbreviated as
meme) of the guider across days to be considered the new guider window.

#### guider_cor_meme_min_hrs

Numeric (default = 1) Minimum required duration of the alternative
guider classification inside the median start to median end (abbreviated
as meme) of the guider across days to be considered the new guider
window.

#### guider_cor_do

Boolean (default = FALSE) Whether to apply the guider correction
algorithm as discussed in Chapter 9 of the online documentation:
<https://wadpac.github.io/GGIR/articles/chapter9_SleepFundamentalsGuiders.html>.
In short, when sleep is fragmented, algorithms to guide the detection of
SPT such as HDCZA may occassionally miss a part of the night or
incorrectly consider a day time nap as the main sleep window in a day.
This optional algorithm aims to correct for such misclassifications.

#### guider_cor_meme_min_dys

Numeric (default = 3) Minimum number of days required to consider
median-median based step in the guider correction algorithm as
controlled by parameter guider_cor_meme_frac_out,
guider_cor_meme_frac_in, and guider_cor_meme_min_hrs. This parameter is
set to 3 by default as a median becomes only meaningful with at least
three values.

### Physical activity Parameters

#### mvpathreshold

Numeric (default = NULL). Legacy parameter, if not provided GGIR uses
the value of threshold.mod for this. Acceleration threshold for MVPA
estimation in GGIR g.part2. This can be a single number or an vector of
numbers, e.g., mvpathreshold = c(100, 120). In the latter case the code
will estimate MVPA separately for each threshold. If this variable is
left blank, e.g., mvpathreshold = c(), then MVPA is not estimated.

#### boutcriter

Numeric (default = NULL). Legacy parameter, if not provided GGIR uses
the value of boutcriter.mvpa for this. A number between 0 and 1, it
defines what fraction of a bout needs to be above the mvpathreshold,
only used in GGIR g.part2.

#### mvpadur

Numeric (default = c(1, 5, 10)). The bout duration(s) for which MVPA
will be calculated. Only used in GGIR g.part2.

#### boutcriter.in

Numeric (default = 0.9). A number between 0 and 1, it defines what
fraction of a bout needs to be below the threshold.lig.

#### boutcriter.lig

Numeric (default = 0.8). A number between 0 and 1, it defines what
fraction of a bout needs to be between the threshold.lig and the
threshold.mod.

#### boutcriter.mvpa

Numeric (default = 0.8). A number between 0 and 1, it defines what
fraction of a bout needs to be above the threshold.mod.

#### threshold.lig

Numeric (default = 40). In g.part5: Threshold for light physical
activity to separate inactivity from light. Value can be one number or
an vector of multiple numbers, e.g., threshold.lig =c(30,40). If
multiple numbers are entered then analysis will be repeated for each
combination of threshold values. Threshold is applied to the first
metric in the milestone data, so if you have only specified do.enmo =
TRUE then it will be applied to ENMO.

#### threshold.mod

Numeric (default = 100). In g.part5: Threshold for moderate physical
activity to separate light from moderate. Value can be one number or an
vector of multiple numbers, e.g., threshold.mod = c(100, 120). If
multiple numbers are entered then analysis will be repeated for each
combination of threshold values. Threshold is applied to the first
metric in the milestone data, so if you have only specified do.enmo =
TRUE then it will be applied to ENMO.

#### threshold.vig

Numeric (default = 400). In g.part5: Threshold for vigorous physical
activity to separate moderate from vigorous. Value can be one number or
an vector of multiple numbers, e.g., threshold.vig =c(400,500). If
multiple numbers are entered then analysis will be repeated for each
combination of threshold values. Threshold is applied to the first
metric in the milestone data, so if you have only specified do.enmo =
TRUE then it will be applied to ENMO.

#### boutdur.mvpa

Numeric (default = c(1, 5, 10)). Duration(s) of MVPA bouts in minutes to
be extracted. It will start with the identification of the longest to
the shortest duration. In the default setting, it will start with the 10
minute bouts, followed by 5 minute bouts in the rest of the data, and
followed by 1 minute bouts in the rest of the data.

#### boutdur.in

Numeric (default = c(10, 20, 30)). Duration(s) of inactivity bouts in
minutes to be extracted. Inactivity bouts are detected in the segments
of the data which were not labelled as sleep or MVPA bouts. It will
start with the identification of the longest to the shortest duration.
In the default setting, it will start with the identification of 30
minute bouts, followed by 20 minute bouts in the rest of the data, and
followed by 10 minute bouts in the rest of the data. Note that we use
the term inactivity instead of sedentary behaviour for the lowest
intensity level of behaviour. The reason for this is that GGIR does not
attempt to classifying the activity type sitting at the moment, by which
we feel that using the term sedentary behaviour would fail to
communicate that.

#### boutdur.lig

Numeric (default = c(1, 5, 10)). Duration(s) of light activity bouts in
minutes to be extracted. Light activity bouts are detected in the
segments of the data which were not labelled as sleep, MVPA, or
inactivity bouts. It will start with the identification of the longest
to the shortest duration. In the default setting, this will start with
the identification of 10 minute bouts, followed by 5 minute bouts in the
rest of the data, and followed by 1 minute bouts in the rest of the
data.

#### frag.metrics

Character (default = NULL). Fragmentation metric to extract. Can be
"mean", "TP", "Gini", "power", or "CoV", "NFragPM", or all the above
metrics with "all". See package vignette for description of
fragmentation metrics.

#### part6_threshold_combi

Character (default = NULL) to indicate the threshold combination derived
in part 5 to be used for part 6. For example, "40_100_120". If left in
default value GGIR will use the first threshold value from parameters
threshold.lig, threshold.mod, and threshold.vig.

### 24/7 Parameters

#### qwindow

Numeric or character (default = c(0, 24)). To specify windows over which
all variables are calculated, e.g., acceleration distribution, number of
valid hours, LXMX analysis, MVPA. If numeric, qwindow should have length
two, e.g., qwindow = c(0, 24), all variables will only be calculated
over the full 24 hours in a day. If qwindow = c(8, 24) variables will be
calculated over the window 0-8, 8-24 and 0-24. All days in the recording
will be segmented based on these values. If you want to use a day
specific segmentation in each day then you can set qwindow to be the
full path to activity diary file (character). Expected format of the
activity diary is: First column headers followed by one row per
recording, first column is recording ID, which needs to match with the
ID GGIR extracts from the accelerometer file. Followed by date column in
format "23-04-2017", where date format is specified by parameter
qwindow_dateformat (below). Use the character combination date, Date or
DATE in the column name. This is followed by one or multiple columns
with start times for the activity types in that day format in
hours:minutes:seconds. The header of the column will be used as label
for each activity type. Insert a new date column before continuing with
activity types for next day. Leave missing values empty. If an activity
log is used then individuals who do not appear in the activity log will
still be processed with value qwindow = c(0, 24). Dates with no activity
log data can be skipped, no need to have a column with the date followed
by a column with the next date. If times in the activity diary are not
multiple of the short window size (epoch length), the next epoch is
considered (e.g., with epoch of 5 seconds, 8:00:02 will be redefined as
8:00:05 in the activity log). When using the qwindow functionality in
combination with GGIR part 5 then make sure to check that parameters
segmentWEARcrit.part5 and segmentDAYSPTcrit.part5 are specified to your
research needs. When using an activity diary be aware that any column
name including the words "impute" or "uncertain" will be ignored. This
means you can, for you own convenience, add columns to log which
timestamps have been manually imputed or are considered uncertain.

#### qlevels

Numeric (default = NULL). Vector of percentiles for which value needs to
be extracted. These need to be expressed as a fraction of 1, e.g.,
c(0.1, 0.5, 0.75). There is no limit to the number of percentiles. If
left empty then percentiles will not be extracted. Distribution will be
derived from short epoch metric data. Parameter qlevels can for example
be used for the MX-metrics (e.g. Rowlands et al) as discussed in
<https://wadpac.github.io/GGIR/articles/chapter7_DescribingDataWithoutKnowingSleep.html#sets-of-quantiles-mx-metrics-by-rowlands-et-al->

#### qwindow_dateformat

Character (default = "%d-%m-%Y"). To specify the date format used in the
activity log as used by \[base\]strptime. For guidance on how to specify
time formats in R see:
<https://wadpac.github.io/GGIR/articles/DateTimeFormatsInR.html>

#### ilevels

Numeric (default = NULL). Levels for acceleration value frequency
distribution in m, e.g., ilevels = c(0,100,200). There is no limit to
the number of levels. If left empty then the intensity levels will not
be extracted. Distribution will be derived from short epoch metric data.

#### IVIS_windowsize_minutes

This argument has been deprecated.

#### IVIS_epochsize_seconds

Numeric (default = NULL). This parameter has been deprecated.

#### IVIS.activity.metric

This argument has been deprecated.

#### IVIS_acc_threshold

This argument has been deprecated.

#### qM5L5

Numeric (default = NULL). Percentiles (quantiles) to be calculated over
L5 and M5 window.

#### MX.ig.min.dur

Numeric (default = 10). Minimum MX duration needed in order for
intensity gradient to be calculated.

#### M5L5res

Numeric (default = 10). Resolution of L5 and M5 analysis in minutes.

#### winhr

Numeric (default = 5). Vector of window size(s) (unit: hours) of LX and
MX analysis, where look for least and most active consecutive number of
X hours.

#### iglevels

Numeric (default = NULL). Levels for acceleration value frequency
distribution in mused for intensity gradient calculation (according to
the method by Rowlands 2018). By default this is parameter is empty and
the intensity gradient calculation is not done. The user can either
provide a single value (any) to make the intensity gradient use the bins
iglevels = c(seq(0, 4000, by = 25), 8000) or the user could specify
their own distribution. There is no constriction to the number of
levels.

#### LUXthresholds

Numeric (default = c(0, 100, 500, 1000, 3000, 5000, 10000)). Vector with
numeric sequence corresponding to the thresholds used to calculate time
spent in LUX ranges.

#### LUX_cal_constant

Numeric (default = NULL). If both LUX_cal_constant and LUX_cal_exponent
are provided LUX values are converted based on formula y = constant \*
exp(x \* exponent)

#### LUX_cal_exponent

Numeric (default = NULL). If both LUX_cal_constant and LUX_cal_exponent
are provided LUX LUX values are converted based on formula y = constant
\* exp(x \* exponent)

#### LUX_day_segments

Numeric (default = NULL). Vector with hours at which the day should be
segmented for the LUX analysis.

#### L5M5window

Has been deprecated after version 1.5-24. This parameter used to define
the start and end time, in 24 hour clock hours, over which L5M5 needs to
be calculated. Now this is done with parameter qwindow.

#### cosinor

Boolean (default = FALSE). Whether to apply the cosinor analysis from
the ActCR package in part 2. In part 6 cosinor analysis is applied by
default and cannot be turned off.

#### part6CR

Boolean (default = FALSE) to indicate whether circadian rhythm analysis
should be run by part 6, this includes: cosinor analysis, extended
cosinor analysis, IS, IV, and phi. Optionally this can be expanded with
detrended fluctutation analysis which is controlled by parameter
`part6DFA`.

#### part6HCA

Boolean (default = FALSE) to indicate whether Household Co Analysis
should be run by part 6.

#### part6Window

Character vector with length two (default = c("start", "end")) to
indicate the start and the end of the time series to be used for
circadian rhythm analysis in part 6. In other words, this parameters is
not used for Household co-analysis. Alternative values are: "Wx", "Ox",
"Hx", where "x" is a number to indicat the xth wakeup, onset or hour of
the recording. Negative values for "x" are also possible and will count
relative to the end of the recording. For example, c("W1", "W-1") goes
from the first till the last wakeup, c("H5", "H-5") ignores the first
and last 5 hours, and c("O2", "W10") goes from the second onset till the
10th wakeup time.

#### part6DFA

Boolean (default = FALSE) to indicate whether to perform Detrended
Fluctuation Analysis. Turned off by default because it can be time
consuming.

#### clevels

Not fully operational yet, to be actived in 2025. Numeric vector
(default = c(30, 150)) with length 2 to indicate cadence ranges used
when summarising step counts, if available. When set to NULL, step
counts will not be summarised in part 5 output.

#### SRI2_WASOmin

Numeric (default = 30) Minimum WASO duration in minutes as used for
Sleep Regularity Calculation in g.part6.

### Output Parameters

#### epochvalues2csv

Boolean (default = FALSE). In g.part2: If TRUE then epoch values are
exported to a csv file. Here, non-wear time is imputed where possible.

#### save_ms5rawlevels

Boolean (default = TRUE). In g.part5: Whether to save the time series
classification (levels) as csv or RData files (as defined by
save_ms5raw_format). Note that time stamps will be stored in the column
timenum in UTC format (i.e., seconds from 1970-01-01). To convert
timenum to time stamp format, you need to specify your desired time
zone, e.g., as.POSIXct(mdat\$timenum, tz = "Europe/London"). If you are
not using GGIR part 6, are not interested in the visualreport
generation, and not interested in time series then you may want to
consider setting this parameter to FALSE.

#### save_ms5raw_format

Character (default = "RData"). In g.part5: To specify how data should be
stored: "csv", "RData", or both via c("csv", "RData"). Only used if
save_ms5rawlevels = TRUE.

#### save_ms5raw_without_invalid

Boolean (default = TRUE). In g.part5: To indicate whether to remove
invalid days from the time series output files. Only used if
save_ms5rawlevels = TRUE.

#### storefolderstructure

Boolean (default = FALSE). Store folder structure of the accelerometer
data.

#### timewindow

Character (default = c("MM", "WW")). In g.part5: Timewindow over which
summary statistics are derived. Value can be "MM" (midnight to
midnight), "WW" (waking time to waking time), "OO" (sleep onset to sleep
onset), or any combination of them.

#### viewingwindow

Numeric (default = 1). Centre the day as displayed around noon
(viewingwindow = 1) or around midnight (viewingwindow = 2) in the
"Report\_" visual report generated with visualreport = TRUE.

#### dofirstpage

Boolean (default = TRUE). To indicate whether a first page with
histograms summarizing the whole measurement should be added in the file
summary reports named "old_report\_" generated with visualreport = TRUE.

#### visualreport

Boolean (default = TRUE). If TRUE, store a visualreport in outputfolder
‘results/file summary reports’ for each recording. Note that this report
was introduced in release 3.1-8 and replaced an older visual report that
had been in GGIR for many years. The older report was only intended as a
quick attempt to have something visual to show to study participants
back in 2015. It was not designed for data quality checking purposes.
The new visual report is specifically aimed at supporting data quality
checks as it shows the time series data as generated and used in GGIR
part 5. .

#### week_weekend_aggregate.part5

Boolean (default = FALSE). In g.part5: To indicate whether week and
weekend-days aggregates should be stored. This is turned off by default
as it generates a large number of extra columns in the output report.

#### do.part3.pdf

Boolean (default = FALSE). In g.part3: Whether to generate a pdf for
g.part3.

#### outliers.only

Boolean (default = FALSE). In g.part4: Only used if do.visual = TRUE. If
FALSE, all available nights are included in the visual representation of
the data and sleeplog. If TRUE, then only nights with a difference in
onset or waking time larger than the variable of parameter criterror
will be included.

#### criterror

Numeric (default = 3). In g.part4: Only used if do.visual = TRUE and
outliers.only = TRUE. criterror specifies the number of minimum number
of hours difference between sleep log and accelerometer estimate for the
night to be included in the visualisation.

#### do.visual

Boolean (default = TRUE). In g.part4: If TRUE, the function will
generate a pdf with a visual representation of the overlap between the
sleeplog entries and the accelerometer detections. This can be used to
visually verify that the sleeplog entries do not come with obvious
mistakes.

#### do.sibreport

Boolean (default = TRUE). In g.part4: To indicate whether to generate
report for the sustained inactivity bouts (SIB). If set to TRUE and when
an advanced sleep diary is available in part 4 then part 5 will use this
to generate summary statistics on the overlap between self-reported
nonwear and napping with SIB. Here, SIB can be filter based on parameter
possible_nap_edge_acc and the first value of possible_nap_dur

#### do.part2.pdf

Boolean (default = TRUE). Now replaced by do.part2.png. If specified and
do.part2.png is not specified then value will be assigned to
do.part2.png. Used in g.part2 to indicate whether to generate a pdf
before, but this is now a png file.

#### do.part2.png

Boolean (default = TRUE). Used in g.part2 to indicate whether to
generate a png file for each recording.

#### sep_reports

Character (default = ","). Value used as sep parameter in
\[data.table\]fwrite for writing csv reports.

#### sep_config

Character (default = ","). Value used as sep parameter in
\[data.table\]fwrite for writing csv config file.

#### dec_reports

Character (default = "."). Value used as dec parameter in
\[data.table\]fwrite for writing csv reports.

#### dec_config

Character (default = "."). Value used as dec parameter in
\[data.table\]fwrite for writing csv config file.

#### visualreport_without_invalid

Boolean (default = TRUE). If TRUE, then reports generated with
visualreport = TRUE named "old_report\_" only show the windows with
sufficiently valid data according to includedaycrit when viewingwindow =
1 or includenightcrit when viewingwindow = 2

#### old_visualreport

Boolean (default = FALSE). If TRUE, then generate old visual report in
addition to new visualreport. The old visualreport will eventually be
deprecated and is not suitable for data quality assessment, see
documentation for visualreport above for details.

#### visualreport_hrsPerRow

Numeric (default = 36). Width of the plots in the new visualreport
(named "report\_…") expressed in hours. Expected to be in the range
24-48. If more than 24 then the extra time overlaps with the beginning
of the next plot.

#### visualreport_focus

Character (default = "day") Whether new visual report (names start with
"report…") focuss on day or night. If set ot "night" the focus will be
on the night.

#### visualreport_validcrit

Numeric (default = 0). Value between 0 and 1 representing the fraction
of data in a plot that is expected to be valid. If this criteria is not
met the row is skipped. Only used in new visual report (names start with
"report…").

#### require_complete_lastnight_part5

Boolean (default = FALSE). When set to TRUE: The last WW window is
excluded if the recording ends between midnight and 3pm, and starts on a
date that is on or one day before the recording end date; The last OO
and MM window are excluded if recording ends between midnight and 9am,
and starts on a date that is on or one day before the recording end
date. This to avoid risk that recording end biases the sleep estimates
for the last night.

#### method_research_vars

Character (default = NULL). Vector with names of methodological variable
categories to store in the csv output files. Currently on "nap" is
available which affects the part5 output. The variables are intended for
methodological research only and are by default turned off.

![GGIR logo](GGIR-MASTERLOGO-RGB.png)
