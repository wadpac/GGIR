# Adds the sustained inactivity bout to the ts series.

Not intended for direct use by GGIR users. Adds the sustained inactivity
bout to the ts series as part of
[g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md).

## Usage

``` r
g.part5.addsib(ts, epochSize, part3_output, desiredtz,
              sibDefinition, nightsi)
```

## Arguments

- ts:

  Data.frame object as passed on from
  [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md)

- epochSize:

  Short epoch size in seconds

- part3_output:

  Segment of part 3 output relevant for current sleep definition

- desiredtz:

  see [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- sibDefinition:

  Character to indicate definition of sib (sustained inactivity bout)

- nightsi:

  Vector with indices for the midnights

## Value

Data.frame ts

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
