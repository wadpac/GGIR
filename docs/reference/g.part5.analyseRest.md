# Analyse rest (internal function)

Analyses overlap self-reported napping, non-wear and sib. Internal
function not intended for direct use by GGIR end-user.

## Usage

``` r
g.part5.analyseRest(sibreport = NULL, dsummary = NULL,
                                ds_names = NULL, fi = NULL,
                                di = NULL, ts = NULL, tz = NULL,
                                params_sleep = NULL)
```

## Arguments

- sibreport:

  sibreport data.frame produced by
  [g.sibreport](https://wadpac.github.io/GGIR/reference/g.sibreport.md)

- dsummary:

  matrix created internally by
  [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md)

- ds_names:

  character vector with variable names corresponding to dsummary created
  internally by
  [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md)

- fi:

  Numeric scalar to indicate variable index, created internally by
  [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md)

- di:

  Numeric scalar to indicate recording index, created internally by
  [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md)

- ts:

  Data.frame with time series object passed on from
  [g.part5_analyseSegment](https://wadpac.github.io/GGIR/reference/g.part5_analyseSegment.md)
  and [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md),

- tz:

  Timezone database name

- params_sleep:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

## Value

List with updated objects dsummary, ds_names, fi, and di

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
