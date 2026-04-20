# Get basic info form the part 1 milestone file

Internal function to get basic info form the part 1 milestone file

## Usage

``` r
getPart1BasicInfo(fn, idloc, tz)
```

## Arguments

- fn:

  Character with full file path to RData file as stored by
  [g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md)

- idloc:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- tz:

  As passed by [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)
  and equal to parameter desiredtz as documented in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

## Value

Data.frame with ID, start and end time of the recording, filename and
brand name.

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>
