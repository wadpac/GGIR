# sustiained inactivty bouts detection

Detects sustiained inactivty bouts. Function not intended for direct use
by package user

## Usage

``` r
g.sib.det(M, IMP, I, twd = c(-12, 12),
             acc.metric = "ENMO", desiredtz = "",
             myfun=c(), sensor.location = "wrist", params_sleep = c(), zc.scale = 1, ...)
```

## Arguments

- M:

  Object produced by
  [g.getmeta](https://wadpac.github.io/GGIR/reference/g.getmeta.md)

- IMP:

  Object produced by
  [g.impute](https://wadpac.github.io/GGIR/reference/g.impute.md)

- I:

  Object produced by
  [g.inspectfile](https://wadpac.github.io/GGIR/reference/g.inspectfile.md)

- twd:

  Vector of length 2, indicating the time window to consider as hours
  relative to midnight.

- acc.metric:

  Which one of the metrics do you want to consider to analyze L5. The
  metric of interest need to be calculated in M (see
  [g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md))

- desiredtz:

  See [g.part3](https://wadpac.github.io/GGIR/reference/g.part3.md)

- myfun:

  External function object to be applied to raw data. See details
  [applyExtFunction](https://wadpac.github.io/GGIR/reference/applyExtFunction.md).

- sensor.location:

  Character to indicate sensor location, default is wrist. If it is hip
  HDCZA algorithm also requires longitudinal axis of sensor to be
  between -45 and +45 degrees.

- params_sleep:

  See [g.part3](https://wadpac.github.io/GGIR/reference/g.part3.md)

- zc.scale:

  Used for zero-crossing counts only. Scaling factor to be applied after
  counts are calculated (GGIR part 3). See
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- ...:

  Any argument used in the previous version of g.sib.det, which will now
  be used to overrule the arguments specified with the parameter
  objects.

## Value

- output = Dataframe for every epoch a classification

- detection.failed = Boolean whether detection failed

- L5list = L5 for every day (defined from noon to noon)

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
