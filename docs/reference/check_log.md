# Function to revise format of user-provided logs

Function to revise the format for missing values and dates in
user-provided

## Usage

``` r
check_log(log, dateformat, colid = 1, datecols = c(), 
            logPath, logtype)
```

## Arguments

- log:

  Data frame with the log as read by
  [`data.table::fread`](https://rdrr.io/pkg/data.table/man/fread.html).

- dateformat:

  Character specifying the expected date format used in the log.

- colid:

  Numeric with the column containing the file ID.

- datecols:

  Numeric with the column/s containing the dates.

- logPath:

  Character containing the full path to the activity log that is being
  checked.

- logtype:

  Character which accepts "activity log" or "study dates log" at the
  moment. Only used for warning messages.

## Value

Data.frame containing the revised log.

## Author

Vincent T van Hees \<v.vanhees@accelting.com\> Jairo H Migueles
\<jairo@jhmigueles.com\>
