# Generate sustiained inactivty bouts report

Generate sustained inactivity bout report. Function not intended for
direct use by package user

## Usage

``` r
g.sibreport(ts, ID, epochlength, logs_diaries=c(), desiredtz="")
```

## Arguments

- ts:

  Data frame with time series as created inside function
  [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md)

- ID:

  Recording identifier (character or numeric)

- epochlength:

  Numeric to indicate epoch length in seconds in the ts object

- logs_diaries:

  Object produced by
  [g.loadlog](https://wadpac.github.io/GGIR/reference/g.loadlog.md)
  function

- desiredtz:

  See [g.getmeta](https://wadpac.github.io/GGIR/reference/g.getmeta.md)

## Value

Dataframe with one row per sustained inactivity bout and corresponding
properties stored in the data.frame columns.

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
