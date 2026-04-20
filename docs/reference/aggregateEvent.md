# Aggregates event data as produced by external function

Aggregates event data as produced by external function

## Usage

``` r
aggregateEvent(metric_name, epochsize, daysummary, ds_names,
                          fi, di, vari, segmentInfo, myfun = NULL,
                          params_247)
```

## Arguments

- metric_name:

  Character with name of metric

- epochsize:

  Numeric epochsize in seconds of the timeseries

- daysummary:

  Matrix with prelimenary day summary

- ds_names:

  Daysummary column names

- fi:

  Column index of daysummary

- di:

  Row index of daysummary representing the recording day

- vari:

  Time series derived from metashort

- segmentInfo:

  List passed on from
  [g.analyse.perday](https://wadpac.github.io/GGIR/reference/g.analyse.perday.md)
  with time series indices of segment to analyse segment name and
  segment number.

- myfun:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- params_247:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
