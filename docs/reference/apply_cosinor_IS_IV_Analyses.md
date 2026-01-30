# Apply Cosinor Analyses to time series

Wrapper function around
[cosinor_IS_IV_Analyses](https://wadpac.github.io/GGIR/reference/cosinor_IS_IV_Analyses.md)
that first prepares the time series before applying the cosinorAnlayses

## Usage

``` r
apply_cosinor_IS_IV_Analyses(ts, qcheck, midnightsi, epochsizes, threshold = NULL)
```

## Arguments

- ts:

  Data.frame with timestamps and acceleration metric.

- qcheck:

  Vector of equal length as number of rows in ts with value 1 for
  invalid timestamps, 0 otherwise.

- midnightsi:

  Indices for midnights in the time series

- epochsizes:

  Epoch size for ts and qcheck respectively

- threshold:

  See
  [cosinor_IS_IV_Analyses](https://wadpac.github.io/GGIR/reference/cosinor_IS_IV_Analyses.md)

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
