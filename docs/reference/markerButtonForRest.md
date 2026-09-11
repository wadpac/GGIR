# Improve the labelling of rest with marker button data

Improve the labelling of rest with marker button data. Currently only
functional for Actiwatch and Philips Healthband.

## Usage

``` r
markerButtonForRest(sibreport, params_sleep, ts)
```

## Arguments

- sibreport:

  sibreport data.frame produced by
  [g.sibreport](https://wadpac.github.io/GGIR/reference/g.sibreport.md)

- params_sleep:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- ts:

  Data.frame with time series object passed on from
  [g.part5_analyseSegment](https://wadpac.github.io/GGIR/reference/g.part5_analyseSegment.md)
  and [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md).

## Value

Updated sibreport

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
