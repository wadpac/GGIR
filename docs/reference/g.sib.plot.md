# Create plot of sustained inactivity bouts

Function create plot of sustained inactivity bouts for quality check
purposes as part of
[g.part3](https://wadpac.github.io/GGIR/reference/g.part3.md). Not
intended for direct use by package user

## Usage

``` r
g.sib.plot(SLE, M, I, plottitle, nightsperpage=7, desiredtz="")
```

## Arguments

- SLE:

  Output from
  [g.sib.det](https://wadpac.github.io/GGIR/reference/g.sib.det.md)

- M:

  Output from
  [g.getmeta](https://wadpac.github.io/GGIR/reference/g.getmeta.md)

- I:

  Output from
  [g.inspectfile](https://wadpac.github.io/GGIR/reference/g.inspectfile.md)

- plottitle:

  Title to be used in the plot

- nightsperpage:

  Number of nights to show per page

- desiredtz:

  See [g.part3](https://wadpac.github.io/GGIR/reference/g.part3.md)

## Value

Function has no output other than the plot

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
