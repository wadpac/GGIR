# Abbreviates daynames to numbers, needed for report generation in [g.plot5](https://wadpac.github.io/GGIR/reference/g.plot5.md)

Abbreviates daynames Monday becomes MON and Sunday becomes SUN

## Usage

``` r
g.abr.day.names(daynames)
```

## Arguments

- daynames:

  Vector of daynames in character format

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

## Examples

``` r
  daynames = c("Monday","Friday")
  daynames_converted = g.abr.day.names(daynames)
```
