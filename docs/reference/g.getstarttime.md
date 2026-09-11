# Extract start time of a measurement

Extract start time of a measurement. GGIR calculates all timestamps by
using the first timestamp and sample frequency. Not intended for direct
use by package user

## Usage

``` r
g.getstarttime(datafile, data, mon, dformat, desiredtz,
  configtz = NULL)
```

## Arguments

- datafile:

  Full path to data file

- data:

  Data part of
  [g.readaccfile](https://wadpac.github.io/GGIR/reference/g.readaccfile.md)
  output

- mon:

  Same as in
  [g.dotorcomma](https://wadpac.github.io/GGIR/reference/g.dotorcomma.md)

- dformat:

  Same as in
  [g.dotorcomma](https://wadpac.github.io/GGIR/reference/g.dotorcomma.md)

- desiredtz:

  Same as in
  [g.dotorcomma](https://wadpac.github.io/GGIR/reference/g.dotorcomma.md)

- configtz:

  Same as in
  [g.dotorcomma](https://wadpac.github.io/GGIR/reference/g.dotorcomma.md)

## Value

The starttime

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
