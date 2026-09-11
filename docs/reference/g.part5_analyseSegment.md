# Analyses the time series per time segment for part 5

Not intended for direct use by GGIR users, part of
[g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md).

## Usage

``` r
g.part5_analyseSegment(indexlog, timeList, levelList,
                                  segments,
                                  segments_names,
                                  dsummary, ds_names,
                                  params_general, params_output,
                                  params_sleep, params_247,
                                  params_phyact,
                                  sumSleep, sibDef,
                                  fullFilename,
                                  add_one_day_to_next_date,
                                  lightpeak_available,
                                  tail_expansion_log,
                                  foldernamei, sibreport = NULL)
```

## Arguments

- indexlog:

  List of objects related to indices of window, file, and segment that
  are passed on from
  [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md) to aid
  selecting time segments or keeping track of where in file the code is.

- timeList:

  List of objects related to time series passed on from
  [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md).

- levelList:

  List of objects related to intensity levels passed on from
  [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md).

- segments:

  List produced by
  [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md)

- segments_names:

  Vector produced by
  [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md)

- dsummary:

  Matrix to hold all daysummary (and segment summary)

- ds_names:

  Character vector with column names of the dsummary matrix. The code
  collects these separately in this vector and assigns them at the end.

- params_general:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- params_output:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- params_sleep:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- params_247:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- params_phyact:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- sumSleep:

  Section of data.frame produced by
  [g.part4](https://wadpac.github.io/GGIR/reference/g.part4.md) passed
  on from [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md).

- sibDef:

  Character to identify sib definition.

- fullFilename:

  Character with full filename being processed

- add_one_day_to_next_date:

  Boolean to indicate whether one day should be added to next date

- lightpeak_available:

  Boolean to indicate whether light peak is available

- tail_expansion_log:

  Object generated in
  [g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md) and
  passed on to
  [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md) when
  argument `recordingEndSleepHour` is used.

- foldernamei:

  Character with folder name in which the data file is stored.

- sibreport:

  Sibreport object as passed on from
  [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md)

## Value

List with objects: indexlog, timeList, and the matrix with the
prelimenary results and column names (dsummary and ds_names, see input
arguments above)
