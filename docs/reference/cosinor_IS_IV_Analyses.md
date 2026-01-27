# Apply cosinor anlaysis and extended cosinor analysis

Applies cosinor anlaysis from the ActCR package to the time series, as
well as IV, IS and phi estimates.

## Usage

``` r
cosinor_IS_IV_Analyses(Xi, epochsize = 60, timeOffsetHours = 0, threshold = NULL)
```

## Arguments

- Xi:

  Vector with time series of movement indicators if the maximum \< 8 and
  mean \< 1 then input is assumed to be in g-units and is multiplied by
  1000.

- epochsize:

  Numeric epochsize in seconds

- timeOffsetHours:

  Numeric time in hours relative to next midnight

- threshold:

  Numeric value to use as threshold to distinguish inactivity from
  active behaviour for the IV and IS analysis. GGIR uses parameter
  threshold.lig to set this threshold.

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
