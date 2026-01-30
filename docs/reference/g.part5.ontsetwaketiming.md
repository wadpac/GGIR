# Identify wake and sleepperiod window timing

Not intended for direct use by GGIR users. Labels timing of wakeing up
and sleep onset as part of
[g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md).

## Usage

``` r
g.part5.onsetwaketiming(qqq, ts, min, sec, hour, timewindowi)
```

## Arguments

- qqq:

  Start and end index of window to analyses

- ts:

  Data.frame with time series as created in
  [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md)

- min:

  Numeric vector with minute values

- sec:

  Numeric vector with second values

- hour:

  Numeric vector with hour values

- timewindowi:

  Character to indicate what timewindow definition is used either "MM"
  or "WW"

## Value

A list with objects: wake, onset, wakei, onseti, skiponset, and
skipwake.

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
