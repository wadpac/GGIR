# Extract M5 and L5 from time series

Extract M5 and L5 from time series, function used by
[g.analyse](https://wadpac.github.io/GGIR/reference/g.analyse.md) and
not intended for direct use by package user. Please see
[g.analyse](https://wadpac.github.io/GGIR/reference/g.analyse.md) for
further clarification on functionalities

## Usage

``` r
g.getM5L5(varnum, epochSize, t0_LFMF, t1_LFMF, M5L5res, winhr, qM5L5 = c(), 
            iglevels = c(), MX.ig.min.dur = 10, UnitReScale = 1000)
```

## Arguments

- varnum:

  Numeric vector of epoch values

- epochSize:

  Small epoch size in seconds

- t0_LFMF:

  Start hour of the day for the M5L5 analyses, e.g. 0 for midnight

- t1_LFMF:

  End hour of the day for the M5L5 analyses, e.g. 24 for midnight

- M5L5res:

  Resolution of hte M5L5 analyses in minutes

- winhr:

  windowsize of M5L5 analyses, e.g. 5 hours

- qM5L5:

  Percentiles (quantiles) to be calculated over L5 and M5 window.

- iglevels:

  See [g.analyse](https://wadpac.github.io/GGIR/reference/g.analyse.md).
  If provided then the intensity gradient will be calculated for all MX
  windows larger or equal than argument MX.ig.min.dur

- MX.ig.min.dur:

  Minimum MX duration needed in order for intensity gradient to be
  calculated

- UnitReScale:

  Numeric value with which acceleration values are multiple to rescale

## Value

- DAYL5HOUR = Starting time in hours of L5

- DAYL5VALUE = average acceleration during L5

- DAYM5HOUR = Starting time in hours of M5

- DAYM5VALUE = average acceleration during M5

- V5NIGHT = average acceleration between 1am and 6am

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

## Examples

``` r
  if (FALSE) { # \dontrun{
    data(data.getmeta)
    g.getM5L5(varnum=data.getmeta,ws3=5,t0_LFMF=0,t1_LFMF=24,M5L5res=10,winhr=5)
  } # }
```
