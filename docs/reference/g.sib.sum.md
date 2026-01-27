# sustiained inactivty bouts detection

Detects sustiained inactivty bouts. Function not intended for direct use
by package user

## Usage

``` r
g.sib.sum(SLE,M,ignorenonwear=TRUE,desiredtz="")
```

## Arguments

- SLE:

  Output from
  [g.sib.det](https://wadpac.github.io/GGIR/reference/g.sib.det.md)

- M:

  Object produced by
  [g.getmeta](https://wadpac.github.io/GGIR/reference/g.getmeta.md)

- ignorenonwear:

  If TRUE then ignore detected monitor non-wear periods to avoid
  confusion between monitor non-wear time and sustained inactivity
  (default = TRUE)

- desiredtz:

  See [g.part3](https://wadpac.github.io/GGIR/reference/g.part3.md)

## Value

Dataframe with per night and per definition of sustained inactivity
bouts the start and end time of each sustained inactivity bout

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
