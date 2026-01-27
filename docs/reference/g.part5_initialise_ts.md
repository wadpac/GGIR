# Initialise time series data from for part 5

Initialise time series dataframe ts, part of
[g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md).

## Usage

``` r
g.part5_initialise_ts(IMP, M, params_247, params_general, longitudinal_axis = c())
```

## Arguments

- IMP:

  Object derived from
  [g.part2](https://wadpac.github.io/GGIR/reference/g.part2.md)

- M:

  Object derived from
  [g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md).

- params_247:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- params_general:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- longitudinal_axis:

  As passed on from g.part5, which could be specified by user or
  estimated from hip data in part 2.

## Value

Data.frame ts
