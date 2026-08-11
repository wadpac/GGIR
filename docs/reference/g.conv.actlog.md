# Function to read activity log and make it useful for the rest of GGIR.

Function to read activity log and convert it into data.frame that has
for each ID and date a different qwindow vector.

## Usage

``` r
g.conv.actlog(qwindow, qwindow_dateformat="%d-%m-%Y", epochSize = 5)
```

## Arguments

- qwindow:

  Path to csv file with activity log. Expected format of the activity
  diary is: First column headers followed by one row per recording,
  first column is recording ID, which needs to match with the ID GGIR
  extracts from the accelerometer file. Followed by date column in
  format "23-04-2017", where date format is specified by argument
  qwindow_dateformat (below). Use the character combination date, Date
  or DATE in the column name. This is followed by one or multiple
  columns with start times for the activity types in that day format in
  hours:minutes:seconds. The header of the column will be used as label
  for each activity type. Insert a new date column before continuing
  with activity types for next day. Leave missing values empty. If an
  activitylog is used then individuals who do not appear in the
  activitylog will still be processed with value c(0,24). Dates with no
  activiy log data can be skipped, no need to have a column with the
  date followed by a column with the next date. If times in the
  activitylog are not multiple of the short window size (epoch length),
  the next epoch is considered (e.g., with epoch of 5 seconds, 8:00:02
  will be redefined as 8:00:05 in the activity log).

- qwindow_dateformat:

  Character specifying the date format used in the activity log.

- epochSize:

  Short epoch size (first value of windowsizes in
  [g.getmeta](https://wadpac.github.io/GGIR/reference/g.getmeta.md)).

## Value

Data.frame with column ID, date and qwindow, where each qwindow value is
a qwindow vector

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
