# MX LX analysis

Identifies the most and least active X hours

## Usage

``` r
MXLX(Y = NULL, X = 5, epochSize = 1, tseg = c(0, 24), resolutionMin = 10)
```

## Arguments

- Y:

  Vector with numeric time series

- X:

  Single numeric value of X in hours

- epochSize:

  Numeric epoch size in seconds of Y

- tseg:

  Numeric vector of length two reflecting the time windownof Y in real
  clock hours. For example, if Y represent noon-midnight then set `tseg`
  to c(12, 24).

- resolutionMin:

  Numeric value to indicate the resolution in minutes of the MX and LX
  search

## Value

A data.frame with the:

- LX the average value of Y for LX

- LXhr the hour in the day at which LX starts

- start_LX the index of Y where LX starts

- end_LX the index of Y where LX ends

- MX the average value of Y for MX

- MXhr the hour in the day at which MX starts

- start_MX the index of Y where MX starts

- end_MX the index of Y where MX ends

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
