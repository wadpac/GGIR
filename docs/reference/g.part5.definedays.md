# Fix missing night in part 4 output

Not intended for direct use by GGIR users. Defines when day windows
start and end as part of
[g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md).

## Usage

``` r
g.part5.definedays(nightsi, wi, indjump,
                     epochSize, qqq_backup = c(), ts,
                     timewindowi, Nwindows, qwindow, ID = NULL,
                     dayborder = 0)
```

## Arguments

- nightsi:

  Vector with indices for the midnights

- wi:

  Numeric to indicate window number

- indjump:

  Number of indices to jump

- epochSize:

  Numeric epoch size in seconds

- qqq_backup:

  Backup of qqq object, which holds the start and end indices of a
  window

- ts:

  Data.frame with time series

- timewindowi:

  Timewindow definition either "MM" or "WW"

- Nwindows:

  Number of windows in the data

- qwindow:

  qwindow argument

- ID:

  ID of participant

- dayborder:

  dayborder argument

## Value

List of qqq and qqq_backup

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
