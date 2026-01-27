# Adds first wake if it is missing in part 4 output.

Not intended for direct use by GGIR users. Adds first wake if it is
missing in part 4 output as part of
[g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md).

## Usage

``` r
g.part5.addfirstwake(ts, summarysleep, nightsi, sleeplog,
  ID, Nepochsinhour, SPTE_end)
```

## Arguments

- ts:

  Data.frame object as passed on from
  [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md)

- summarysleep:

  Data.frame object as passed on from
  [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md) with
  sleep summary information from
  [g.part4](https://wadpac.github.io/GGIR/reference/g.part4.md).

- nightsi:

  Vector with indices for the nights

- sleeplog:

  Data.frame with all sleeplog information

- ID:

  Participant ID

- Nepochsinhour:

  Number of epochs in an hour

- SPTE_end:

  Sleep period time end index

## Value

Data.frame ts updated with first wakeup time

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
