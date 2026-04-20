# Day segment analyses with GGIR

**NOTE: If you are viewing this page via CRAN note that the main GGIR
documentation has been migrated to the [GGIR GitHub
pages](https://wadpac.github.io/GGIR/).**

## Introduction

Is this specific person more active in the morning or in the afternoon?
Are children more active during their work hours or their leisure time?
How much inactivity occurs at work in office workers? Questions like
these can be answered with GGIR but you first have to specify a few
parameters.

The main input argument to be specified is `qwindow`, which can be used
the following ways:

- To specify the clock hours in the day based on which the segmented day
  analyses should take place.
- To specify an activity log (diary) that should be used to guide the
  segmentation per individual and per day of the recording.

In the following sections I will discuss both scenarios.

## Clock hour-based segmentation

To perform clock hour segmentation, you will need to provide function
GGIR with argument `qwindow` and assign it a numeric vector with the
hours for the segmentation. If the start and end of the day, are not
explicitly provided in the vector GGIR will add them. Please find below
some example values for `qwindow`. The number of values used by
`qwindow` is unlimited, but be aware that some of the analyses are such
as MX-metrics are impossible for very small windows and will produce
empty results.

| qwindow value | Resulting segment(s) to be analysed |
|----|----|
| c(0,24) | midnight to following midnight (24 hours), the full day is the only segment. |
| c(8,24) | midnight-8:00 (8 hour segment), 8:00-midnight (16 hour segment), and midnight-midnight (24 hour segment). |
| c(6,11, 13, 17) | midnight-6:00 (6 hour segment), 6:00-11:00 (5 hour segment), 11:00-13:00 (2 hour segment), 13:00-17:00 (4 hour segment), 17:00-midnight (7 hour segment), and midnight-midnight (24 hour segment). |
| c(0:24) | 25 segments: 24 segments of 1 hour corresponding to each hour of the day, and midnight-midnight (24 hour segment). |

Day Saving Time (DST) is taken into account when identifying the start
of the day, but not when identifying the day segments. In other words, a
23 hour days is processed as the 24 hours after the first midnight. This
to ensure that segment length is identical across days of the week,
which is needed to ease comparison of outcome variables across days.

## Segmentation guided by activity log

To perform activity-log based segmentation, you will need to provide
function `GGIR` with argument `qwindow` and assign it the full path to
your activity log in `.csv format`,
e.g. `qwindow="C:/myactivitylog.csv"`.

The activity log is expected to be a .csv-file with the following
structure:

| ID   | date     | work    | travelhome | home     | date     | work    | travelhome | home     |
|------|----------|---------|------------|----------|----------|---------|------------|----------|
| 1234 | 04-11-20 | 7:45:00 | 17:00:00   | 17:30:00 | 05-11-20 |         |            | 17:30:00 |
| 4567 | 24-11-20 | 7:45:00 | 17:00:00   | 17:30:00 | 25-11-20 | 7:45:00 | 17:00:00   | 17:30:00 |

**Rows:** First row represents the column headers after which each row
represents one accelerometer recording.

**ID-column:** The first column is expected to hold the recording ID,
which needs to match with the ID GGIR extracts from the accelerometer
file. If unsure how to format the ID values, apply GGIR to a sample of
your accelerometer files using the default argument settings. The ID
column in the generated part 2 .csv reports will show how the
participant ID is extracted by GGIR. If no ID is extracted, see
documentation for argument `idloc`, which helps you to specify the
location of the participant in the file name or file header. If ID
extraction fails the accelerometer files cannot be matched with the
corresponding activity log entries.

**Date-column:** The ID column is followed by a date column for the
first log day. To ensure GGIR recognises this date correctly, specify
argument `qwindow_dateformat`. The default format is `"\%d-\%m-\%Y"` as
in 23-2-2021 to indicate the 23rd of February 2021. If your date is
formatted as 2-23-21 then specify`"\%m-\%d-\%y"`. The column name of the
date column needs to include the character combination “date” or “Date”
or “DATE”. Use the same date format consistently throughout your
activity diary.

**Start-times:** The date column is followed by one or multiple columns
with start times for the activity types in that day format in
hours:minutes:seconds. Do not provide dates in these cells. The header
of the column will be used as label for each activity type. Insert a new
date column before continuing with activity types for next day. Leave
missing values empty.

**Missing values:** If values are missing the preceding and succeeding
time point will be used as the edges of the segment. In the example
below this means that we will define a segment from `A-C` for ID `1234`,
while for ID `6789` we only defined segments `A-B` and `B-C`, segment
`A-C` is not derived here.

| ID   | date     | A       | B        | C        |
|------|----------|---------|----------|----------|
| 1234 | 04-11-20 | 7:45:00 |          | 17:30:00 |
| 6789 | 24-11-20 | 7:45:00 | 13:00:00 | 17:30:00 |

**Notes:** - If an activity log was collected for some individuals then
those will be processed with qwindow value c(0,24). - Dates with no
activity log data can be skipped, no need to have a column with the date
followed by a column with the next date. - The end time of one activity
is assumed to be the start time of the next activity. We currently do
not facilitate overlapping time segments.

## Implementation in GGIR

Both approaches are implemented in GGIR part 2 and part 5. Therefore the
specific output variables that are calculated both in part 2 and 5 are
available per day, per person, and per segment of the day based on the
argument `qwindow` Note that `qwindow` is only used in **part 5** when
`timewindow` includes `"MM"` (see specific documentation for
`timewindow`} in the [parameters
vignette](https://CRAN.R-project.org/package=GGIR/vignettes/GGIRParameters.html))

At the moment, specifying the argument `qwindow` triggers the
calculation of the `qwindow` segments both in part 2 and part 5, which
may result in a longer time to finish the analysis. If only interested
in the segments in either part 2 or part 5, an option might be to run
GGIR parts 1:2 with the argument `qwindow` of interest, and then set
`qwindow = NULL` and run GGIR parts 3:5 (or vice versa: `qwindow = NULL`
for GGIR parts 1:2, and then the desired `qwindow` segments when running
GGIR parts 3:5).

For more information about the output variables calculated in each part
of the pipeline, see the main [GGIR
vignette](https://CRAN.R-project.org/package=GGIR/vignettes/GGIR.html).

## Examples

For more information about how to use the GGIR function call see
explanation in the main [GGIR
vignette](https://CRAN.R-project.org/package=GGIR/vignettes/GGIR.html).

### Clock-hour based segmentation:

``` r
library("GGIR")
GGIR(datadir = "/your/data/directory",
             outputdir = "/your/output/directory",
             mode = 1:5, # <= run GGIR parts 1 to 5
             do.report = c(2, 5), # <= generate csv-report for GGIR part 2 and part 5
             qwindow = c(0, 6, 12, 18, 24),
             timewindow = "MM")
```

### Activity log based segmentation:

``` r
library("GGIR")
GGIR(datadir = "/your/data/directory",
             outputdir = "/your/output/directory",
             mode = 1:5, # <= run GGIR parts 1 to 5
             do.report = c(2, 5), # <= generate csv-report for GGIR part 2 and part 5
             qwindow = "/path/to/your/activity/log.csv",
             timewindow = "MM")
```

After running this code GGIR creates an output folder in the output
directory as specified with argument outputdir. In the subfolder
`results` you will then find csv files with the reports generated in
part 2 and part 5 of the pipeline:

**Part 2**

- `part2_summary.csv` the recording level summary, with 1 row per
  recording and in recording level aggregates of day segments in
  columns.
- `part2_daysummary.csv` the day level summary, with 1 row per day and
  day segment specific outcomes in columns.
- `part2_daysummary_longformat.csv` the day level summary in long
  format, such that each row represents one segment from one day in one
  recording.

In both `part2_summary.csv` and `part2_daysummary.csv` the column names
tell you the day segment they correspond to. For example, column names
ending with `_18-24hr` refer to the time segment 18:00-24:00. In
`part2_daysummary_longformat.csv` the time segment is clarified via
columns qwindow_timestamps and qwindow_name.

**Part 5**

In part 5, information about the segments of the days are exported in
different csv reports than the person-level and day-level summaries.
These files include the word “Segments” in the filename and are provided
in the long format and aggregated per day and per person:

- `part5_daysummary_Segments[...].csv` the day level summary in long
  format, such that each row represents one segment from one day in one
  recording.
- `part5_personsummary_Segments[...].csv` the recording level summary in
  long format, such that each row represents the average for each
  outcome in one specific segments across all days in which that segment
  is available per participant.

## Cleaning parameters for day segments (in part 5):

In part 5, the analyses performed per segment of the day come with the
possibility to clean the reports based on the information available in
the segments. The users can select to include only those segments with a
given amount of wear time during the segment (`segmentWEARcrit.part5`),
as well as with a given awake time or sleep period time in the segment
(`segmentDAYSPTcrit.part5`).

These arguments are likely to be critical for a meaningful analysis of
the data. The presence of sleep in a segment with physical activity will
bias physical inactivity estimates and the presence of physical activity
in a segment with sleep will bias sleep estimates. It will then become
impossible to quantify whether it was the lack of one or the presence of
the other behaviour that drives the association with for example a
health outcome.

## Analyses performed per day segment

The analyses that GGIR per segment of the day, include:

**Acceleration distribution (in part 2):** Derived if argument `ilevels`
is specified. You will find these under the variable names such as
`[0,36)_ENMO_mg` which means time spent between 0 and 36 mg defined by
acceleration metric ENMO.

**Number of valid hours of data (in part 2):** You will recognise these
as `N_valid_hours_in_window` which tells you the number of valid hours
per time window, and `N_valid_hours` which is the number of valid hours
per day.

**Non-wear time percentage (in part 5):** `nonwear_day_perc`,
`nonwear_spt_perc`, and `nonwear_day_spt_perc` tell you the proportion
of the segment classified as non-wear during awake time (day) and during
sleep period time (spt).

**LXMX analysis (in part 2 and part5):** LXMX analysis, which stands for
least and most active X hours of the segment. You will recognise these
variable names like `L5hr_ENMO_mg` which is the start time of the least
active five hours defined by metric ENMO, and `L5_ENMO_mg` which is the
average acceleration for those hours.

**Intensity gradient analysis (in part 2 and part 5):** You will find
these as variables that start with `ig_gradient_` See
[description](https://cran.r-project.org/package=GGIR/vignettes/GGIR.html#41_Output_part_2)
of GGIR part 2 output in the main GGIR vignette for further details.

**Time spent in Moderate or Vigorous Physical Activity (MVPA) (in part 2
and part 5):** You will find these as variables such as
`MVPA_E5S_T201_ENMO` or `MVPA_E5S_B1M80%_T201_ENMO`. See
[description](https://cran.r-project.org/package=GGIR/vignettes/GGIR.html#41_Output_part_2)
of GGIR part 2 output in the main GGIR vignette for further details.

**Time spent in sleeping, in inactivity and physical activity
intensities (part 5):** You will find these variables in the part 5
reports, in their bouted, unbouted, and total time version of the
variables. See
[description](https://cran.r-project.org/package=GGIR/vignettes/GGIR.html#41_Output_part_5)
of GGIR part 5 output in the main GGIR vignette for further details.

![GGIR logo](GGIR-MASTERLOGO-RGB.png)
